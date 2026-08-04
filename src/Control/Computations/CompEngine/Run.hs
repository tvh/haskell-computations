{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Computations.CompEngine.Run (
  Blocking (..),
  RunCompEngineIf (..),
  RunSettings (..),
  NextRun (..),
  noNextRun,
  startNextRun,
  ShouldStartNextRun,
  CompRunIterationLimit (..),
  initStateIf,
  withStateIf,
  runCompEngine,
  garbageHandler,
  deleteDeadOutputs,
)
where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine.CompFlow
import Control.Computations.CompEngine.CompFlowRegistry
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import qualified Control.Computations.CompEngine.Impl as Impl
import Control.Computations.CompEngine.SimpleStateIf
import Control.Computations.CompEngine.Types
import Control.Computations.CompEngine.Utils.DefTable (ArenaCompactionStats (..))
import Control.Computations.Utils.Logging
import qualified Control.Computations.Utils.MultiMap as MM
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.TimeUtils
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Foldable as F
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import Data.Word
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stack
import System.Environment (lookupEnv)
import Text.Printf (printf)

newtype RunSettings = RunSettings
  { rs_maxRunIterations :: Option CompRunIterationLimit
  -- ^ overwrites 'rcif_maxRunIterations' (from RunCompEngineIf) for a single run if set.
  -- Useful for tests.
  }
  deriving (Show, Eq)

defaultRunSettings :: RunSettings
defaultRunSettings =
  RunSettings
    { rs_maxRunIterations = None
    }

data NextRun
  = NoNextRun
  | StartNextRun RunSettings
  deriving (Show, Eq)

noNextRun :: NextRun
noNextRun = NoNextRun

startNextRun :: NextRun
startNextRun = StartNextRun defaultRunSettings

type ShouldStartNextRun a m =
  Int
  -- ^ number of the next run
  -> Bool
  -- ^ true if changes were found in the last run
  -> Int
  -- ^ stale caps from last run
  -> a
  -- ^ state returned from the last check
  -> m (NextRun, a)
  -- ^ return 'StartNextRun' and a new state if the run loop should continue

withStateIf
  :: (CompEngineStateIf IO -> IO a)
  -> IO a
withStateIf action =
  bracket
    (initStateIf False)
    (\(_, stop) -> liftIO stop)
    (action . fst)

initStateIf
  :: Bool
  -> IO (CompEngineStateIf IO, IO ())
initStateIf shouldValidate =
  do
    (stateIf, reportLockStats) <- setupSimpleStateIf shouldValidate
    exitMVar <- liftIO newEmptyMVar
    -- NOTE: 'reportLockStats' is a no-op unless 'setupSimpleStateIf' turned
    -- on lock instrumentation (see its haddock). @close@ is already run via
    -- 'withStateIf'\'s 'bracket' on every path that tears an engine down --
    -- including 'Control.Concurrent.Async.cancel', whose release-action
    -- guarantee is what the bench relies on (see "Bench/Main.hs") -- so this
    -- is the one place a stats dump is guaranteed to happen exactly once,
    -- after the engine has genuinely stopped taking the lock.
    let close = reportLockStats >> liftIO (putMVar exitMVar ())
    return (stateIf, close)

-- | Per-method call counter and elapsed-time accumulator, one instance per
-- 'CompEngineStateIf' field. Same shape as the aggregate @callsRef@\/
-- @holdNsRef@ pair in 'setupSimpleStateIf' below, just multiplied by ten.
data MethodStats = MethodStats
  { ms_calls :: !(IORef Word64)
  , ms_ns :: !(IORef Word64)
  }

newMethodStats :: IO MethodStats
newMethodStats = MethodStats <$> newIORef 0 <*> newIORef 0

-- | Time one call, bumping @stats@'s counters around it. Unlike the
-- aggregate instrumentation in 'ssif_withState' (which now separates lock
-- wait from lock hold -- see its own haddock), this wraps the whole method
-- call as seen from 'Impl.hs': wait *and* hold together, because that is the
-- only boundary a per-method wrapper *can* sit at without threading a
-- method tag down into 'ssif_withState' itself (see this module's own
-- haddock on why the flag-off path must stay a single baked-in closure, and
-- "Tests/ObservingStateIf.hs" for the same per-field wrapping shape used
-- here).
--
-- That makes this table's own grand total (see 'printMethodStatsTable')
-- directly useful under contention, not just a curiosity: __table TOTAL
-- minus the aggregate's hold time is per-method wait time__, the same
-- quantity 'ssif_withState's own 'waitNsRef' measures directly, just
-- decomposed by which method actually waited. The two are not expected to
-- match exactly (nine extra 'getMonotonicTimeNSec' calls and 'IORef' bumps
-- here, over and above the aggregate's own three), only to agree closely
-- enough to cross-check one against the other -- 'printMethodStatsTable'
-- prints both numbers side by side for exactly that comparison.
timeMethod :: MethodStats -> IO a -> IO a
timeMethod stats act = do
  t0 <- getMonotonicTimeNSec
  x <- act
  t1 <- getMonotonicTimeNSec
  modifyIORef' (ms_calls stats) (+ 1)
  modifyIORef' (ms_ns stats) (+ (t1 - t0))
  return x

-- | Print the nine methods' stats as one table, sorted by total time
-- descending, alongside how much of the *instrumented* total (sum of the
-- nine methods' own totals, not the aggregate hold-time line) each accounts
-- for. @aggregateHoldS@\/@aggregateWaitS@ are 'ssif_withState's own
-- directly-measured totals (in seconds), printed alongside the table's own
-- TOTAL row purely so the "table TOTAL minus aggregate hold is per-method
-- wait" identity 'timeMethod'\'s haddock describes can be read off directly,
-- instead of requiring the reader to go compute it by hand from two
-- separately-printed numbers.
printMethodStatsTable :: Double -> Double -> [(String, MethodStats)] -> IO ()
printMethodStatsTable aggregateHoldS aggregateWaitS methods = do
  rows <- forM methods $ \(name, stats) -> do
    calls <- readIORef (ms_calls stats)
    ns <- readIORef (ms_ns stats)
    return (name, calls, ns)
  let totalNs = sum [ns | (_, _, ns) <- rows]
      totalCalls = sum [c | (_, c, _) <- rows]
      sorted = sortOn (\(_, _, ns) -> Down ns) rows
      pct ns = if totalNs == 0 then 0 else 100 * fromIntegral ns / fromIntegral totalNs :: Double
      meanNs calls ns = if calls == 0 then 0 else fromIntegral ns / fromIntegral calls :: Double
  putStrLn "=== COMP_ENGINE_LOCK_STATS: per-method breakdown ==="
  putStrLn
    ( "NOTE: each row times the *whole* CompEngineStateIf method call (wait "
        ++ "for the lock, plus holding it), not lock hold time alone -- it is "
        ++ "not the same quantity as the aggregate hold-time line above, and "
        ++ "the two should not be read as measuring the same thing (see "
        ++ "Run.hs's 'timeMethod' haddock)."
    )
  printf
    "%-24s %10s %14s %12s %10s\n"
    ("method" :: String)
    ("calls" :: String)
    ("total (s)" :: String)
    ("mean (ns)" :: String)
    ("% of total" :: String)
  forM_ sorted $ \(name, calls, ns) ->
    printf
      "%-24s %10d %14.6f %12.1f %9.2f%%\n"
      name
      calls
      (fromIntegral ns / (1e9 :: Double))
      (meanNs calls ns)
      (pct ns)
  printf
    "%-24s %10d %14.6f\n"
    ("TOTAL" :: String)
    totalCalls
    (fromIntegral totalNs / (1e9 :: Double))
  let tableTotalS = fromIntegral totalNs / (1e9 :: Double)
      impliedWaitS = tableTotalS - aggregateHoldS
  printf
    "implied per-method wait (table TOTAL - aggregate hold): %.6f s -- directly measured aggregate wait: %.6f s\n"
    impliedWaitS
    aggregateWaitS

-- | Print the edge-arena compaction breakdown: one row per (def, arena
-- kind) pair that has compacted at least once (see 'debugCompactionStats'),
-- sorted by total compaction time descending, plus a grand total. This is
-- the report the hospital-benchmark investigation in
-- @docs\/benchmark-notes.md@ reads by differencing two runs' worth of it
-- (one with the rerun phase disabled, one with it enabled) to isolate how
-- much of a live-update round's cost 'DT.eaCompact' actually accounts for
-- -- see that doc's "CSR compaction" section for the methodology and the
-- 'DT.eaCompact' haddock in "Utils/DefTable.hs" for what each column
-- means.
printCompactionStatsTable :: [CompactionReportRow] -> IO ()
printCompactionStatsTable rows = do
  let compacted = [r | r <- rows, acs_compactions (crStats r) > 0]
      totalNs = sum [acs_totalNs (crStats r) | r <- compacted]
      totalCompactions = sum [acs_compactions (crStats r) | r <- compacted]
      totalRowsWalked = sum [acs_rowsWalked (crStats r) | r <- compacted]
      sorted = sortOn (\r -> Down (acs_totalNs (crStats r))) compacted
  putStrLn "=== COMP_ENGINE_LOCK_STATS: edge-arena compaction breakdown ==="
  putStrLn
    ( "NOTE: (def, arena) pairs that never compacted are omitted. "
        ++ "'rows walked' sums, across every compaction, the def's row "
        ++ "count at compaction time -- eaCompact's O(rowCount) scan, the "
        ++ "natural 'how much work' measure, independent of how many rows "
        ++ "were actually alive or how many edges got copied."
    )
  printf
    "%-28s %-9s %12s %14s %12s\n"
    ("def" :: String)
    ("arena" :: String)
    ("compactions" :: String)
    ("rows walked" :: String)
    ("total (s)" :: String)
  forM_ sorted $ \r ->
    printf
      "%-28s %-9s %12d %14d %12.6f\n"
      (crDefLabel r)
      (crArenaKind r)
      (acs_compactions (crStats r))
      (acs_rowsWalked (crStats r))
      (fromIntegral (acs_totalNs (crStats r)) / (1e9 :: Double))
  printf
    "%-28s %-9s %12d %14d %12.6f\n"
    ("TOTAL" :: String)
    ("" :: String)
    totalCompactions
    totalRowsWalked
    (fromIntegral totalNs / (1e9 :: Double))

{- | 'SifState' is genuinely, internally mutable (growable unboxed/boxed
 vectors behind 'Data.IORef.IORef's) rather than an immutable value swapped
 through a 'TVar' -- running arbitrary in-place vector mutation inside an
 STM transaction would be unsound (a transaction can retry, replaying the
 mutation). Access is serialized with a plain 'MVar' lock instead: this
 gives up the free, lock-free snapshots a 'TVar' would offer, but
 'stepCompEngine' is sequential, so there is no use case in practice that
 would exploit them.
-}
setupSimpleStateIf
  :: Bool
  -> IO (CompEngineStateIf IO, IO ())
setupSimpleStateIf shouldValidate =
  do
    st <- newSifState
    lock <- newMVar ()
    -- Read once, here, rather than per-call: 'ssif_withState' is on the
    -- hottest path in the engine (every 'withCompState' call, millions of
    -- times in the 1M-instance bench), so the choice between the plain and
    -- instrumented body has to be made once, at setup time, and baked into
    -- which closure 'stateIf' below actually captures -- not re-checked on
    -- every call. This is also why lock stats are opt-in via an env var
    -- rather than always-on: even a single branch or a pair of IORef bumps
    -- per call would perturb the very hold-time baseline this exists to
    -- measure.
    mLockStatsEnv <- lookupEnv "COMP_ENGINE_LOCK_STATS"
    let lockStatsEnabled = case mLockStatsEnv of
          Nothing -> False
          Just "" -> False
          Just "0" -> False
          Just _ -> True
    if lockStatsEnabled
      then do
        callsRef <- newIORef (0 :: Word64)
        holdNsRef <- newIORef (0 :: Word64)
        -- Total time every 'ssif_withState' caller spent waiting to acquire
        -- @lock@ (blocked inside 'withMVar' before its body got to run),
        -- summed the same way 'holdNsRef' sums time spent *holding* it once
        -- acquired -- see 'reportLockStats' below for how the two are
        -- reported and cross-checked against the per-method table.
        waitNsRef <- newIORef (0 :: Word64)
        let stateIf =
              SimpleStateIf
                { ssif_withState =
                    \f -> do
                      -- tWaitStart is taken *before* the MVar is even
                      -- attempted, so @t0 - tWaitStart@ (below) is genuinely
                      -- "time spent waiting for the lock" and nothing else
                      -- -- not, e.g., any time this closure's own caller
                      -- spent before reaching this point.
                      tWaitStart <- getMonotonicTimeNSec
                      withMVar lock $ \() -> do
                        t0 <- getMonotonicTimeNSec
                        x <- f st
                        when shouldValidate $ validateSifState st
                        t1 <- getMonotonicTimeNSec
                        modifyIORef' callsRef (+ 1)
                        modifyIORef' holdNsRef (+ (t1 - t0))
                        modifyIORef' waitNsRef (+ (t0 - tWaitStart))
                        return x
                }
            baseCeif = mkSimpleCompEngineStateIf stateIf
        -- One 'MethodStats' per 'CompEngineStateIf' field, so
        -- 'ssif_withState's aggregate (which can't tell methods apart, see
        -- its own haddock) can be broken down by which of the ten methods
        -- actually did the work. (Ten, not eleven: 'trackOutput' is gone --
        -- sink outputs are staged per-evaluation in 'CompMEnv' now, not in
        -- shared state, so there is nothing left here to instrument for it.
        -- 'lookupCapResultDequeueIfStale' is the newest addition, fusing
        -- what used to be two separate 'lookupCapResult'/'dequeueGivenCap'
        -- acquisitions on the hot path into one -- see its haddock in
        -- "Core.hs".)
        msLookupCapResult <- newMethodStats
        msLookupCapResultDequeueIfStale <- newMethodStats
        msCapEvaluationStarted <- newMethodStats
        msCapEvaluationFinished <- newMethodStats
        msDequeueGivenCap <- newMethodStats
        msDequeueNextCap <- newMethodStats
        msStaleQueueSize <- newMethodStats
        msEnqueueStaleCaps <- newMethodStats
        msGetCompSinkOuts <- newMethodStats
        msGetQueue <- newMethodStats
        -- Written out as an explicit record rather than a record update
        -- (@baseCeif { lookupCapResult = ... }@): most fields are
        -- higher-rank (@forall a. IsCompResult a => ...@), and GHC rejects
        -- record-update syntax against a record with higher-rank fields --
        -- same reason "Tests/ObservingStateIf.hs" builds its wrapper this
        -- way.
        let instrumentedCeif =
              CompEngineStateIf
                { lookupCapResult = \cap -> timeMethod msLookupCapResult (lookupCapResult baseCeif cap)
                , lookupCapResultDequeueIfStale = \staleOk cap -> timeMethod msLookupCapResultDequeueIfStale (lookupCapResultDequeueIfStale baseCeif staleOk cap)
                , capEvaluationStarted = \cap -> timeMethod msCapEvaluationStarted (capEvaluationStarted baseCeif cap)
                , capEvaluationFinished = \cap deps outputs mres -> timeMethod msCapEvaluationFinished (capEvaluationFinished baseCeif cap deps outputs mres)
                , dequeueGivenCap = \cap -> timeMethod msDequeueGivenCap (dequeueGivenCap baseCeif cap)
                , dequeueNextCap = timeMethod msDequeueNextCap (dequeueNextCap baseCeif)
                , staleQueueSize = timeMethod msStaleQueueSize (staleQueueSize baseCeif)
                , enqueueStaleCaps = \deps -> timeMethod msEnqueueStaleCaps (enqueueStaleCaps baseCeif deps)
                , getCompSinkOuts = \s -> timeMethod msGetCompSinkOuts (getCompSinkOuts baseCeif s)
                , getQueue = timeMethod msGetQueue (getQueue baseCeif)
                }
            methodStatsTable =
              [ ("lookupCapResult", msLookupCapResult)
              , ("lookupCapResultDequeueIfStale", msLookupCapResultDequeueIfStale)
              , ("capEvaluationStarted", msCapEvaluationStarted)
              , ("capEvaluationFinished", msCapEvaluationFinished)
              , ("dequeueGivenCap", msDequeueGivenCap)
              , ("dequeueNextCap", msDequeueNextCap)
              , ("staleQueueSize", msStaleQueueSize)
              , ("enqueueStaleCaps", msEnqueueStaleCaps)
              , ("getCompSinkOuts", msGetCompSinkOuts)
              , ("getQueue", msGetQueue)
              ]
            -- Reports how long the engine-state lock was actually *held*
            -- (time inside this 'withMVar' body, from just before @f st@ to
            -- just after @validateSifState@) *and*, separately, how long any
            -- caller spent *waiting* for it (from just before 'withMVar' is
            -- even attempted to the moment its body actually starts running
            -- -- see 'waitNsRef' above).
            --
            -- This module's own haddock used to predict this exact moment:
            -- at width 1, 'ssif_withState' is only ever called under this
            -- single 'MVar' one caller at a time, so wait time was
            -- definitionally zero and hold time was the whole story --
            -- true right up until parallel eval (see
            -- "Control.Computations.CompEngine.Impl"'s @ce_par@) started
            -- making genuinely concurrent calls into the same
            -- 'CompEngineStateIf' from forked eval leaves. 'waitNsRef' is
            -- what turns "this number alone would understate actual lock
            -- cost" from a predicted risk into something actually measured,
            -- without disturbing 'holdNsRef' itself: hold time is still
            -- computed exactly as before, from the same @t0@/@t1@ pair,
            -- just with the lock's own wait now recorded alongside it
            -- instead of being invisible.
            --
            -- The per-method table printed alongside it measures a
            -- *different* quantity (see 'timeMethod's haddock): whole-call
            -- time (wait + hold) per method, not hold time alone -- so under
            -- contention, that table's own TOTAL row minus this function's
            -- @holdS@ *is* per-method wait time, and 'printMethodStatsTable'
            -- prints that alongside 'waitS' (this line's own direct
            -- measurement) as a cross-check between the two. At width 1
            -- (wait ~= 0) the two numbers are expected to be close to zero
            -- and to each other; they are not expected to match exactly --
            -- structurally different instrumentation (three
            -- 'getMonotonicTimeNSec' calls and three 'IORef' bumps here per
            -- acquisition, versus two of each per method call there).
            reportLockStats = do
              calls <- readIORef callsRef
              holdNs <- readIORef holdNsRef
              waitNs <- readIORef waitNsRef
              let holdS = fromIntegral holdNs / (1e9 :: Double)
                  waitS = fromIntegral waitNs / (1e9 :: Double)
              putStrLn "=== COMP_ENGINE_LOCK_STATS ==="
              printf
                "engine-state lock: %d calls, %d ns total hold time (%.6f s), %d ns total wait time (%.6f s)\n"
                calls
                holdNs
                holdS
                waitNs
                waitS
              printMethodStatsTable holdS waitS methodStatsTable
              debugCompactionStats st >>= printCompactionStatsTable
        return (instrumentedCeif, reportLockStats)
      else do
        let stateIf =
              SimpleStateIf
                { ssif_withState =
                    \f ->
                      withMVar lock $ \() -> do
                        x <- f st
                        when shouldValidate $ validateSifState st
                        return x
                }
        return (mkSimpleCompEngineStateIf stateIf, return ())

data RunCompEngineIf a = RunCompEngineIf
  { rcif_shouldStartWithRun :: ShouldStartNextRun a IO
  -- ^ blocks or returns if run loop should continue
  , rcif_emptyChangesMode :: Blocking
  -- ^ whether `allCompSrcChanges` should block if there are no changes
  , rcif_getTime :: IO UTCTime
  , rcif_maxLoopRunTime :: TimeSpan
  -- ^ maximum time the processing loop runs without checking for new changes
  , rcif_maxRunIterations :: CompRunIterationLimit
  , --  ^ maximum number of iterations the processing loop runs without checking for new changes
    rcif_reportGarbage :: Garbage -> IO ()
  }

data CompRunIterationLimit
  = CompRunLimitIterationsTo Word64
  | CompRunUnlimitedIterations
  deriving (Eq, Show)

runCompEngine
  :: forall a
   . CompEngineIfs
  -> [AnyCompAp]
  -> RunCompEngineIf a
  -> a
  -> IO ()
runCompEngine ceIfs comps rcif startState =
  main ceIfs
 where
  main :: CompEngineIfs -> IO ()
  main ifs =
    do
      ce <- Impl.startCompEngine ifs comps
      -- 'finally', not a plain sequential call: 'loop' only returns on
      -- 'NoNextRun', which none of this project's benchmarks ever signal --
      -- every one of them tears its engine down via
      -- 'Control.Concurrent.Async.cancel' instead (see 'initStateIf's own
      -- haddock, which already relies on exactly this guarantee for
      -- 'reportLockStats'). Without 'finally' here, 'Impl.stopCompEngine'
      -- -- and the 'Impl.reportEngineDiag' call it now makes internally --
      -- would silently never run on that path.
      loop ce 1 startState True 0 mempty
        `Control.Exception.finally` Impl.stopCompEngine ce
  loop
    :: Impl.CompEngine
    -> Int
    -> a
    -> Bool
    -> Int
    -> Impl.GenDel
    -> IO ()
  loop ce !run !lastState !hadChanges !staleCapsFromLastRun !lastG =
    do
      (continue, newState) <-
        rcif_shouldStartWithRun rcif run hadChanges staleCapsFromLastRun lastState
      case continue of
        NoNextRun -> logNote "Stopping CompEngine loop due to user request."
        StartNextRun runSettings -> do
          logDebug
            ( "Preparing run "
                ++ show run
                ++ " of CompEngine loop, hadChanges="
                ++ show hadChanges
                ++ ", staleCapsFromLastRun="
                ++ show staleCapsFromLastRun
                ++ ", emptyChangesMode="
                ++ show (rcif_emptyChangesMode rcif)
            )
          loop' ce run newState staleCapsFromLastRun lastG runSettings
  loop' ce run newState staleCapsFromLastRun lastG runSettings =
    do
      changes <- do
        let b =
              if staleCapsFromLastRun > 0
                then DontBlock
                else rcif_emptyChangesMode rcif
        logDebug ("Waiting for changes, blocking=" ++ show b)
        atomically $ allCompSrcChanges (ce_compFlowRegistry ceIfs) b
      let lenChanges = HashSet.size changes
          changesize = show lenChanges ++ " changes"
          changesList = HashSet.toList changes
          changesByType = MM.fromList [(identifyType x, x) | x <- changesList]
          maxRunIterations =
            fromOption (rcif_maxRunIterations rcif) (rs_maxRunIterations runSettings)
          changerepr =
            case changesList of
              [] -> "no change"
              [x] -> "change " ++ show x
              _ -> changesize
          logChange :: forall a. (Show a, HasCallStack) => LogLevel -> String -> a -> IO ()
          logChange level prefix =
            doLog level callStack . ((prefix ++ ": ") ++) . show
      if null changes then logDebug "Found no changes" else logInfo ("Found " ++ changerepr)
      forM_ (MM.toSetList changesByType) $ \(ty, changesOfTy@(HashSet.size -> changesSize)) ->
        if changesSize < 10
          then mapM_ (logChange INFO "  - ") changesOfTy
          else do
            let changesList = (HashSet.toList changesOfTy)
            logInfo
              ( " "
                  ++ T.unpack (unTypeId ty)
                  ++ " has "
                  ++ show changesSize
                  ++ " changes. "
                  ++ "Here are 5 of them:"
              )
            mapM_ (logChange INFO "  - ") (take 5 changesList)
            logDebug "Here are the remaining: "
            mapM_ (logChange DEBUG "  - ") (drop 5 changesList)
      (hasStaleCaps, nextG) <-
        if not (null changes) || staleCapsFromLastRun > 0
          then do
            enqInfo <- Impl.notifyCompEngine ce changesList
            logInfo
              ( "Notified CompEngine about changes. "
                  ++ show (Map.size (ei_affectedCaps enqInfo))
                  ++ " new stale caps."
              )
            logStale changerepr (Map.keys (ei_affectedCaps enqInfo))
            let remBefore = ei_currentQueueSize enqInfo
            logNote
              ( "Starting run "
                  ++ show run
                  ++ " of CompEngine with "
                  ++ show remBefore
                  ++ " stale caps."
              )
            t0 <- rcif_getTime rcif
            let innerLoop state@(fromIntegral @Int -> i, _) g =
                  rcif_getTime rcif >>= \t ->
                    if ( t
                          `diffTime` t0
                          > rcif_maxLoopRunTime rcif
                          || CompRunLimitIterationsTo i
                          == maxRunIterations
                       )
                      then return (state, g)
                      else continueLoop state g
                continueLoop (i, _) g =
                  do
                    (r, newG) <- Impl.stepCompEngine ce g
                    if r < 0
                      then return ((i, 0), newG)
                      else innerLoop (i + 1, r) newG
                s0 = (0, remBefore)
            ((iterations, remAfter), g) <- innerLoop s0 lastG
            nextG <-
              if (remAfter <= 0)
                then do
                  rcif_reportGarbage rcif (Impl.garbage g)
                  return mempty
                else return g
            logInfo
              ( "Finished run "
                  ++ show run
                  ++ " with "
                  ++ show iterations
                  ++ " iterations and "
                  ++ show remAfter
                  ++ " stale caps remaining."
              )
            return (remAfter, nextG)
          else return (0, mempty)
      let haveChanges = not (null changes)
      loop ce (run + 1) newState haveChanges hasStaleCaps nextG

garbageHandler :: CompFlowRegistry -> Garbage -> IO ()
garbageHandler reg g =
  do
    let garbageMap = unionsAnyCompSinkOutsMap $ HashMap.elems $ garbage_outputs g
    forM_ (anyOutsMapToList garbageMap) $ \(_, anyOutputs) ->
      withCompSinkForOuts reg anyOutputs $ \sink xs ->
        do
          logInfo $
            "Deleting "
              ++ show (HashSet.size xs)
              ++ " outputs of "
              ++ show (compSinkId sink)
          compSinkDeleteOutputs sink xs
    forAllSrcs_ reg srcFun
 where
  srcFun :: forall s. CompSrc s => s -> IO ()
  srcFun src = do
    let key = compSrcId src
        deps =
          -- list monad
          do
            (ForAnyCompFlow ident _ depKey) <- F.toList (garbage_deps g)
            guard (ident == key)
            case cast depKey of
              Nothing -> []
              Just (SomeCompSrcKey d :: SomeCompSrcKey s) -> [d]
    case deps of
      [] -> logDebug $ "No deps to delete for " <> show key
      _ ->
        do
          let depsSet = HashSet.fromList deps
          logInfo $
            "Deleting " ++ show (HashSet.size depsSet) ++ " deps of " ++ show key
          compSrcUnregister src depsSet

withCompSinkForOuts
  :: forall a
   . CompFlowRegistry
  -> AnyCompSinkOuts
  -> (forall s. CompSink s => s -> CompSinkOuts s -> IO a)
  -> IO a
withCompSinkForOuts reg (ForAnyCompFlow id _ someOuts) fun = go someOuts
 where
  go :: forall s. CompSink s => SomeCompSinkOuts s -> IO a
  go (SomeCompSinkOuts outs) = withCompSinkId reg id more >>= failInM
   where
    more :: s -> IO a
    more sink = fun sink outs

{- | Asks the StateIf which outputs currently live and asks the DataIf
 which outputs exists and deletes all those that exist but aren't alive.
-}
deleteDeadOutputs :: CompSink s => CompEngineStateIf IO -> s -> IO ()
deleteDeadOutputs stateIf sink =
  case compSinkListExistingOutputs sink of
    None ->
      logNote $
        "CompSink "
          ++ show (compSinkId sink)
          ++ " doesn't support deleting dead outputs. Not deleting anything."
    Some listAction ->
      do
        aliveOutputs <- getCompSinkOuts stateIf sink
        diskOutputs <- listAction
        let deadOutputs = diskOutputs `HashSet.difference` aliveOutputs
            diskSize = HashSet.size diskOutputs
            deadSize = HashSet.size deadOutputs
            aliveSize = HashSet.size aliveOutputs
            sinkName = show (compSinkId sink)
        logInfo
          ( sinkName
              ++ " has "
              ++ show diskSize
              ++ " outputs on disk, "
              ++ show aliveSize
              ++ " alive"
          )
        when (diskSize > 0) $ logDebug ("On disk (first 10): " ++ show (take 10 (HashSet.toList diskOutputs)))
        when (aliveSize > 0) $ logDebug ("Alive (first 10): " ++ show (take 10 (HashSet.toList aliveOutputs)))
        unless (HashSet.null deadOutputs) $
          do
            logNote
              ( sinkName
                  ++ ": Deleting "
                  ++ show deadSize
                  ++ " outputs that exist but are no longer produced."
              )
            logDebug ("Dead (first 10): " ++ show (take 10 (HashSet.toList deadOutputs)))
            compSinkDeleteOutputs sink deadOutputs
