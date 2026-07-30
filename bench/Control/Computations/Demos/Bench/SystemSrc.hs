{-# LANGUAGE TypeFamilies #-}

{- | A synthetic 'CompSrc' modelling one external clinical system (the
 hospital bench in "Control.Computations.Demos.Bench.Hospital" registers
 five instances of this type: admissions\/discharge\/transfer, vitals, labs,
 pharmacy, notes). Shaped like "Control.Computations.FlowImpls.HashMapFlow"
 -- an in-memory @TVar (HashMap Key Val)@ plus a change-set 'TVar' feeding
 'compSrcWaitChanges' in exactly the same way (see that module's haddock for
 the reasoning behind that shape; it is not repeated here) -- plus the two
 things a real service call has that 'HashMapFlow'\'s in-memory
 'Control.Computations.FlowImpls.HashMapFlow.hmfLookup' does not:

 * a per-instance __latency__ (a 'threadDelay' inside 'compSrcExecute',
   standing in for real network\/service time -- this is the whole reason
   the hospital bench exists: 'Control.Computations.Demos.Bench.Main'\'s
   graph has no latency anywhere to hide, so concurrency can never show up
   in its numbers regardless of width);
 * a __call counter__ and a __concurrency high-water mark__, both plain
   'IORef'\/'TVar' bookkeeping bumped on every request served, so the
   benchmark can report how many source calls actually happened and how many
   of them were genuinely observed overlapping (see 'sysCallCount'\/
   'sysHighWaterMark');
 * a __batch call counter__, separate from the request counter: this type
   overrides 'compSrcExecuteBatch' (see 'executeBatchImpl') to serve every
   request the engine has bundled together against this instance with one
   'readTVarIO' and one simulated-latency 'threadDelay', instead of one of
   each per request -- 'sysCallCount' still counts every individual request
   served, so 'sysCallCount' \/ 'sysBatchCallCount' is directly the round-trip
   reduction this buys, visible in the benchmark's own report rather than
   only inferred from timing.
-}
module Control.Computations.Demos.Bench.SystemSrc (
  SystemSrc,
  SystemReq (..),
  Key,
  Val,
  initSystemSrc,
  sysInsert,
  sysInsertBatch,
  sysCallCount,
  sysBatchCallCount,
  sysHighWaterMark,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine
import Control.Computations.Utils.Hash
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef
import qualified Data.Text as T
import Data.Typeable

type Key = BS.ByteString
type Val = BS.ByteString
type Ver = Hash128

type SystemDep = Dep Key (Maybe Ver)

data SystemSrc = SystemSrc
  { sys_ident :: T.Text
  , sys_dataVar :: TVar (HashMap Key Val)
  , sys_changesVar :: TVar (HashSet SystemDep)
  , sys_latencyUs :: Int
  , sys_callCount :: IORef Int
  , sys_batchCallCount :: IORef Int
  , sys_inFlight :: TVar Int
  , sys_highWater :: TVar Int
  }
  deriving (Typeable)

data SystemReq a where
  SystemLookupReq :: Key -> SystemReq (Maybe Val)

instance CompSrc SystemSrc where
  type CompSrcReq SystemSrc = SystemReq
  type CompSrcKey SystemSrc = Key
  type CompSrcVer SystemSrc = Maybe Ver
  compSrcInstanceId = CompSrcInstanceId . sys_ident
  compSrcExecute = executeImpl
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges = waitChangesImpl

  {- | 'compSrcExecute' (see 'executeImpl') bumps two counters via plain STM,
   optionally 'threadDelay's, then takes a 'readTVarIO' snapshot of the
   backing map -- no write, no lock, nothing here requires calls against this
   instance to be serialized against each other. Declaring 'FlowConcurrent'
   is the entire reason this type exists: with a nonzero 'sys_latencyUs',
   this is what lets several reads against *different* instances overlap
   their 'threadDelay's (see "Control.Computations.CompEngine.Impl"'s
   @doSuspended@); reads against the *same* instance within one batch don't
   need that anymore -- see 'compSrcExecuteBatch' below, which collapses
   them into one delay instead.
  -}
  compSrcConcurrency _ = FlowConcurrent

  -- | The whole reason this type has a per-call latency: a batch of N
  -- lookups against this instance now pays 'sys_latencyUs' once, not N
  -- times -- see 'executeBatchImpl'.
  compSrcExecuteBatch = executeBatchImpl

-- | Build a fresh, empty instance named @ident@ with the given
-- per-call latency in microseconds (0 disables the delay entirely -- see
-- 'executeImpl'\/'executeBatchImpl'), paid once per 'compSrcExecute' call
-- and once per 'compSrcExecuteBatch' call regardless of how many requests
-- that batch call serves.
initSystemSrc :: T.Text -> Int -> IO SystemSrc
initSystemSrc ident latencyUs = do
  dataV <- newTVarIO HashMap.empty
  changesV <- newTVarIO HashSet.empty
  countRef <- newIORef 0
  batchCountRef <- newIORef 0
  inFlightV <- newTVarIO 0
  highWaterV <- newTVarIO 0
  pure (SystemSrc ident dataV changesV latencyUs countRef batchCountRef inFlightV highWaterV)

waitChangesImpl :: SystemSrc -> STM (HashSet SystemDep)
waitChangesImpl sys = do
  set <- readTVar (sys_changesVar sys)
  writeTVar (sys_changesVar sys) HashSet.empty
  if HashSet.null set
    then retry
    else pure set

{- | Bump the in-flight\/high-water pair, optionally sleep 'sys_latencyUs'
 microseconds to stand in for a real service call, then decrement -- shared
 by 'executeImpl' (one request pays this once) and 'executeBatchImpl' (a
 whole bundled group pays this once total, not once per request -- the
 collapse this type exists to demonstrate). The in-flight bump\/decrement
 brackets only the delay, so 'sysHighWaterMark' reports how many *calls*
 (single or batched) were genuinely inside the simulated wait at once, not
 how many individual requests were being served -- that's 'sysCallCount'.
-}
simulateRoundTrip :: SystemSrc -> IO ()
simulateRoundTrip sys = do
  atomically $ do
    n <- (+ 1) <$> readTVar (sys_inFlight sys)
    writeTVar (sys_inFlight sys) n
    hw <- readTVar (sys_highWater sys)
    when (n > hw) (writeTVar (sys_highWater sys) n)
  when (sys_latencyUs sys > 0) (threadDelay (sys_latencyUs sys))
  atomically (modifyTVar' (sys_inFlight sys) (subtract 1))

-- | Serve exactly one request: bump the request counter by one, pay one
-- simulated round trip ('simulateRoundTrip'), then answer from the
-- in-memory map. The engine only ever reaches this for a request that
-- never joined a batch (see 'Control.Computations.CompEngine.Impl'\'s
-- @doCompSrcReq@) -- every request inside a 'CompReqCombined' batch is
-- served by 'executeBatchImpl' instead, however many requests that batch
-- turns out to hold (including exactly one).
executeImpl :: SystemSrc -> SystemReq a -> IO (HashSet SystemDep, Fail a)
executeImpl sys (SystemLookupReq key) = do
  atomicModifyIORef' (sys_callCount sys) (\n -> (n + 1, ()))
  simulateRoundTrip sys
  mVal <- HashMap.lookup key <$> readTVarIO (sys_dataVar sys)
  pure (HashSet.singleton (Dep key (fmap largeHash128 mVal)), Ok mVal)

{- | Serve every request the engine bundled together against this instance
 with __one__ simulated round trip ('simulateRoundTrip', so one
 'threadDelay' for the whole group instead of one per request) and __one__
 'readTVarIO' snapshot, then answer each request with a pure
 'HashMap.lookup' against that shared snapshot. Bumps 'sys_callCount' by
 the number of requests served (so it keeps meaning \"how many individual
 lookups happened\" regardless of how they were grouped) and
 'sys_batchCallCount' by exactly one (so it means \"how many round trips
 happened\") -- their ratio is this feature's payoff, directly reportable
 rather than only inferable from timing.
-}
executeBatchImpl :: SystemSrc -> [SrcFetch SystemSrc] -> IO ()
executeBatchImpl sys fetches = do
  atomicModifyIORef' (sys_callCount sys) (\n -> (n + length fetches, ()))
  atomicModifyIORef' (sys_batchCallCount sys) (\n -> (n + 1, ()))
  simulateRoundTrip sys
  m <- readTVarIO (sys_dataVar sys)
  F.for_ fetches (answerFetch m)
 where
  -- See "Control.Computations.FlowImpls.HashMapFlow"'s own answerFetch
  -- helper for why this needs its own top-level-shaped, explicitly-typed
  -- equation rather than an inline lambda.
  answerFetch :: HashMap Key Val -> SrcFetch SystemSrc -> IO ()
  answerFetch m (SrcFetch (SystemLookupReq key) write) =
    let mVal = HashMap.lookup key m
     in write (Right (HashSet.singleton (Dep key (fmap largeHash128 mVal)), Ok mVal))

-- | Insert or overwrite @key@, and record the change so a comp body reading
-- it sees the update -- the source-side mutation hook used for the
-- benchmark's live-update phase (mirrors
-- 'Control.Computations.FlowImpls.HashMapFlow.hmfInsert').
sysInsert :: SystemSrc -> Key -> Val -> IO ()
sysInsert sys key val = atomically (insertOneSTM sys key val)

-- | Insert or overwrite many @(source, key, value)@ triples -- possibly
-- against several different 'SystemSrc' instances -- in one single STM
-- transaction, so they become visible to the engine as one indivisible unit.
--
-- This is not just a convenience: it is load-bearing for any caller that
-- wants to mutate more than one key \"in one go\" and then wait for the
-- engine to settle. 'compSrcWaitChanges' (see 'waitChangesImpl') is an STM
-- @retry@ loop, and STM wakes a blocked transaction \-\- and lets it commit
-- \-\- as soon as *any* single 'TVar' it reads changes, not once every write
-- a caller /intends/ as one batch has landed. A caller that instead issues
-- @N@ separate 'sysInsert' calls races its own remaining writes against the
-- driver thread: the driver's blocked
-- "Control.Computations.CompEngine.CompFlowRegistry".@allCompSrcChanges@
-- transaction can wake on the *first* write, process only that one change,
-- and report a run with zero leftover stale caps while the other @N-1@
-- writes are still pending -- which is indistinguishable, from
-- 'Control.Computations.CompEngine.Driver.waitForFullSettle's own
-- perspective, from \"genuinely fully drained\" (see that function's
-- haddock and the callers' own settle-discipline comments). The result is a
-- silent undercount, not a crash: confirmed by hand, a batch of 1,000
-- separate 'sysInsert' calls followed by one 'waitForFullSettle' produced
-- 1,981 reruns on one process run and 7,578 on another, for the identical
-- mutation set. Committing every write in the batch inside one 'atomically'
-- block removes the race outright: STM guarantees the whole transaction is
-- either invisible to every other transaction or fully visible, so the
-- driver can never observe a partial batch.
sysInsertBatch :: [(SystemSrc, Key, Val)] -> IO ()
sysInsertBatch = atomically . mapM_ (\(sys, key, val) -> insertOneSTM sys key val)

-- | Shared by 'sysInsert' and 'sysInsertBatch': record @val@ at @key@ in
-- @sys@'s data map and its change set. Not itself wrapped in 'atomically' --
-- callers compose it into whatever transaction they need (a single-key one
-- for 'sysInsert', an arbitrarily-sized one for 'sysInsertBatch').
insertOneSTM :: SystemSrc -> Key -> Val -> STM ()
insertOneSTM sys key val = do
  modifyTVar' (sys_dataVar sys) (HashMap.insert key val)
  modifyTVar'
    (sys_changesVar sys)
    (HashSet.union (HashSet.singleton (Dep key (Just (largeHash128 val)))))

-- | Total number of individual requests this instance has served so far,
-- via either 'executeImpl' or 'executeBatchImpl'.
sysCallCount :: SystemSrc -> IO Int
sysCallCount sys = readIORef (sys_callCount sys)

-- | Total number of 'compSrcExecuteBatch' round trips this instance has
-- served so far -- always <= 'sysCallCount', and 'sysCallCount' \/
-- 'sysBatchCallCount' is the bundling win as an observed ratio.
sysBatchCallCount :: SystemSrc -> IO Int
sysBatchCallCount sys = readIORef (sys_batchCallCount sys)

-- | The largest number of 'compSrcExecute' calls against this instance ever
-- observed genuinely in flight at once (see 'executeImpl'). 1 means every
-- call so far ran strictly after the previous one finished -- i.e. no
-- overlap was ever observed, regardless of what caused that (width 1,
-- 'FlowSerial', or simply no batch wide enough to dispatch a job).
sysHighWaterMark :: SystemSrc -> IO Int
sysHighWaterMark sys = readTVarIO (sys_highWater sys)
