{- | Scale benchmark mirroring the persistence-free phases of the Rust
 @persist_bench@ example
 (@rust-computations\/crates\/computations\/examples\/persist_bench.rs@):

   1. __Cold eval__: build a synthetic 10-level, 50-definition dependency
      graph and run it to settled, measuring wall time, achieved instance
      count (comp body invocations) and RSS. This is Rust's "phase 5, cold
      restart, no persistence".
   2. __Live incremental__: with the same still-running engine, mutate
      exactly one source key and measure wall time until the next
      propagation round completes, plus the rerun count during that round.
      This is Rust's "phase 7".

 Persistence and engine optimizations are explicitly out of scope: this
 measures the reference (@fullCaching@) engine as-is, in a single process.

 Scale is configurable via the @PERSIST_BENCH_SCALE@ environment variable
 (a float multiplier on every entry of 'baseLevelSizes', each floored at 1;
 defaults to @1.0@, the ~1,000,000-instance configuration). The
 'Control.Computations.CompEngine.CompFlowRegistry.CompFlowConcurrency'
 width (see 'withCompFlows') is likewise configurable via
 @PERSIST_BENCH_CONCURRENCY@, defaulting to @1@ -- but this graph never
 builds a batch wide enough to dispatch a job regardless of width, so
 changing it is not expected to move any number below.

 Rerun counting has no honest way to run from inside a 'CompM' comp body
 (there is no @liftIO@, unlike the Rust benchmark's async bodies which do a
 plain @run_counter.fetch_add@ right there). Rather than smuggle an
 'unsafePerformIO'-based counter into comp bodies -- which does not work
 reliably anyway, since GHC's optimizer is free to float, share, or
 eliminate a "pure" IO action that doesn't genuinely vary per call, and in
 practice does exactly that -- this benchmark counts at the engine's own
 interface boundary instead: 'countingStateIf' wraps 'CompEngineStateIf'
 (see "Control.Computations.CompEngine.Core") so that every call to
 'capEvaluationStarted' -- which "Control.Computations.CompEngine.Impl"
 calls exactly once, immediately before running each computation body --
 first bumps an 'IORef', then delegates to the original implementation.
 This is ordinary, honest IO in the orchestrating driver thread, and
 observes precisely the same event the Rust benchmark's
 @run_counter.fetch_add@ observes, just from outside the body rather than
 inside it. Comp bodies themselves stay pure 'CompM' code with no counting
 plumbing at all.
-}
module Control.Computations.Demos.Bench.Main (benchMain) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine
import Control.Computations.FlowImpls.HashMapFlow
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Word
import GHC.Stats
import System.Environment (lookupEnv)
import System.Posix.Process (getProcessID)
import System.Process (readProcess)
import System.Timeout (timeout)
import Text.Printf (printf)
import Text.Read (readMaybe)

----------------------------------------
-- Graph shape (mirrors Rust persist_bench.rs)
----------------------------------------

-- | Number of dependency levels. Level 0 reads sources; the last level
-- ("top") writes to the sink.
levelsCount :: Int
levelsCount = 10

-- | Definitions per level.
defsPerLevel :: Int
defsPerLevel = 5

-- | Distinct source keys level-0 instances read from (@index \`mod\` srcKeys@).
srcKeys :: Word32
srcKeys = 300

-- | Instances per level, bottom (level 0) to top (level 9), at
-- @PERSIST_BENCH_SCALE=1.0@ (the default). See the Rust module docs for why
-- these are front-loaded rather than flat.
baseLevelSizes :: [Word32]
baseLevelSizes = [205000, 205000, 205000, 205000, 205000, 128945, 101885, 61090, 27060, 1000]

-- | Reads @PERSIST_BENCH_SCALE@ (defaulting to @1.0@ when unset or
-- unparseable).
readScale :: IO Double
readScale = do
  mScale <- lookupEnv "PERSIST_BENCH_SCALE"
  pure (fromMaybe 1.0 (mScale >>= readMaybe))

-- | The scaled level-size vector, each entry floored at 1.
scaledLevelSizes :: Double -> [Word32]
scaledLevelSizes scale = map scaleOne baseLevelSizes
 where
  scaleOne base = max 1 (round (fromIntegral base * scale :: Double))

----------------------------------------
-- Counting interceptor + adapted driver
----------------------------------------

{- | Wraps a 'CompEngineStateIf' so every call to 'capEvaluationStarted' --
 the engine's own per-instance "about to run this computation body" hook,
 called exactly once immediately before each real (non-cache-hit)
 invocation -- first bumps @ref@, then delegates to the original
 implementation. All other fields delegate unchanged.

 Built as an explicit record rather than a record update
 (@orig { capEvaluationStarted = ... }@): most fields are higher-rank
 (@forall a. IsCompResult a => ...@), and GHC rejects record-update syntax
 against a record with higher-rank fields.
-}
countingStateIf :: IORef Int -> CompEngineStateIf IO -> CompEngineStateIf IO
countingStateIf ref orig =
  CompEngineStateIf
    { lookupCapResult = lookupCapResult orig
    , capEvaluationStarted = \cap -> do
        atomicModifyIORef' ref (\n -> (n + 1, ()))
        capEvaluationStarted orig cap
    , capEvaluationFinished = capEvaluationFinished orig
    , dequeueGivenCap = dequeueGivenCap orig
    , dequeueNextCap = dequeueNextCap orig
    , staleQueueSize = staleQueueSize orig
    , enqueueStaleCaps = enqueueStaleCaps orig
    , trackOutput = trackOutput orig
    , getCompSinkOuts = getCompSinkOuts orig
    , getQueue = getQueue orig
    }

{- | Adapted from 'compDriver'' (see
 "Control.Computations.CompEngine.Driver"): identical driver-loop wiring,
 but wraps the state-if record with 'countingStateIf' before building
 'CompEngineIfs', so every genuine computation-body invocation is honestly
 counted in @counterRef@.
-}
benchCompDriver
  :: (IsCompParam p, IsCompResult r)
  => IORef Int
  -> TVar (Option RunStats)
  -> (CompFlowRegistry -> IO () -> IO ())
  -> CompWireM (Comp p r)
  -> p
  -> IO ()
benchCompDriver counterRef runVar withRegisteredFlows wireComps startVal = do
  reg <- newCompFlowRegistry
  withStateIf $ \stateIf -> withRegisteredFlows reg $ do
    let ifs =
          CompEngineIfs
            { ce_compFlowRegistry = reg
            , ce_stateIf = countingStateIf counterRef stateIf
            }
        rifs =
          RunCompEngineIf
            { rcif_shouldStartWithRun = shouldStartNextRun stateIf reg runVar
            , rcif_emptyChangesMode = Block
            , rcif_getTime = getCurrentTime
            , rcif_maxLoopRunTime = seconds 10
            , rcif_maxRunIterations = CompRunUnlimitedIterations
            , rcif_reportGarbage = garbageHandler reg
            }
    comps <- rootComps
    runCompEngine ifs comps rifs ()
 where
  runDeletes stateIf reg =
    do
      logNote "Deleting leftovers from previous program run"
      forAllSinks_ reg (deleteDeadOutputs stateIf)
  shouldStartNextRun stateIf reg runVarLocal nRun hadChanges nStaleCaps state =
    do
      when (nRun == 1) (runDeletes stateIf reg)
      let !stats = RunStats{rs_run = nRun, rs_hadChanges = hadChanges, rs_staleCaps = nStaleCaps}
      atomically $ writeTVar runVarLocal (Some stats)
      pure (startNextRun, state)
  rootComps = (failInM . fmap snd) $
    runCompWireM $
      do
        c <- wireComps
        pure [wrapCompAp (mkCompAp c startVal)]

----------------------------------------
-- Flow wiring
----------------------------------------

{- | Reads @PERSIST_BENCH_CONCURRENCY@ (defaulting to 1, i.e. today's
 behaviour: no worker threads) and sets it on @reg@ before registering any
 flow. Settable here, on the registry @benchCompDriver@ already constructs
 and hands to this very callback, without forking the driver -- see
 'Control.Computations.CompEngine.CompFlowRegistry.CompFlowRegistry's own
 haddock for why that's exactly the intended way to reach it.

 This exists to let the width knob be exercised end-to-end by this
 benchmark's own numbers, not because this particular graph has anything to
 gain from it: every read here is 'HashMapFlow's in-memory
 'Control.Computations.FlowImpls.HashMapFlow.hmfLookup' (a 'readTVarIO'),
 and 'wireAllComps' never combines sibling reads via '<*>' (each level's
 body sequences its 'evalCompOrFail' calls monadically, one at a time), so
 no 'CompReqCombined' batch wide enough to dispatch a job is ever built
 here regardless of width -- see the module haddock's "no speedup at any
 width" note.
-}
withCompFlows :: HashMapFlow -> HashMapFlow -> CompFlowRegistry -> IO () -> IO ()
withCompFlows kv docSink reg action = do
  concurrency <- maybe 1 (max 1) . (>>= readMaybe) <$> lookupEnv "PERSIST_BENCH_CONCURRENCY"
  setCompFlowConcurrency reg (mkCompFlowConcurrency concurrency)
  registerCompSrc reg kv
  registerCompSink reg docSink
  action

----------------------------------------
-- Graph definitions
----------------------------------------

type Level = [Comp Word32 Word64]

docKey :: Word32 -> BSC.ByteString
docKey i = BSC.pack ("doc_" ++ show i)

srcKeyFor :: Word32 -> BSC.ByteString
srcKeyFor i = BSC.pack (show (i `mod` srcKeys))

parseWord64 :: BSC.ByteString -> Maybe Word64
parseWord64 = readMaybe . BSC.unpack

-- | Level-0 def body: read source key @show (i \`mod\` srcKeys)@; base is
-- the parsed value or 0; result is @base + i + d@ (all wrapping 'Word64').
level0Body :: TypedCompSrcId HashMapFlow -> Word64 -> Word32 -> CompM Word64
level0Body kvSrcId dWord i = do
  mval <- compSrcReq kvSrcId (HashMapLookupReq (srcKeyFor i))
  let base = fromMaybe 0 (mval >>= parseWord64)
  pure (base + fromIntegral i + dWord)

-- | Level-@L>0@ def body: fold three fan-in edges from the previous level
-- (via fixed modular-arithmetic index formulas, matching the Rust spec
-- exactly so 'Word32' wraps identically), then combine with @d@ and @i@.
-- Top-level instances additionally write their result to the doc sink.
higherLevelBody
  :: TypedCompSinkId HashMapFlow -> Level -> Word32 -> Bool -> Word64 -> Word32 -> CompM Word64
higherLevelBody docSinkId prevLevel prevSize isTop dWord i = do
  let c0 = (i * 2 + 1) `mod` prevSize
      c1 = (i * 7 + 13) `mod` prevSize
      c2 = (i * 31 + 101) `mod` prevSize
      pick c = prevLevel !! fromIntegral (c `mod` fromIntegral defsPerLevel)
  r0 <- evalCompOrFail (pick c0) c0
  r1 <- evalCompOrFail (pick c1) c1
  r2 <- evalCompOrFail (pick c2) c2
  let result = r0 + r1 + r2 + dWord + fromIntegral i
  when isTop $
    compSinkReq docSinkId (HashMapStoreReq (docKey i) (BSC.pack (show result)))
  pure result

-- | Wires all 'levelsCount' levels (each with 'defsPerLevel' defs), bottom
-- to top, threading each level's defs and size down as "prev" for the next.
buildLevels
  :: TypedCompSrcId HashMapFlow -> TypedCompSinkId HashMapFlow -> [Word32] -> CompWireM [Level]
buildLevels kvSrcId docSinkId sizes = go (0 :: Int) Nothing sizes
 where
  numLevels = length sizes
  go :: Int -> Maybe (Level, Word32) -> [Word32] -> CompWireM [Level]
  go _ _ [] = pure []
  go lvl mPrev (sz : rest) = do
    let isTop = lvl == numLevels - 1
    curLevel <- mapM (wireDef lvl isTop mPrev) [0 .. defsPerLevel - 1]
    restLevels <- go (lvl + 1) (Just (curLevel, sz)) rest
    pure (curLevel : restLevels)
  wireDef :: Int -> Bool -> Maybe (Level, Word32) -> Int -> CompWireM (Comp Word32 Word64)
  wireDef lvl isTop mPrev d =
    wireComp $ defineComp name fullCaching bodyFun
   where
    name = "l" ++ show lvl ++ "_d" ++ show d
    dWord = fromIntegral d :: Word64
    bodyFun = case mPrev of
      Nothing -> level0Body kvSrcId dWord
      Just (prevLevel, prevSize) -> higherLevelBody docSinkId prevLevel prevSize isTop dWord

-- | Wires the full graph plus its root: for @i@ in @[0, topSize)@, grouped
-- by @i \`mod\` defsPerLevel@, evaluate every top-level def at its group's
-- params (sequential 'mapM' over 'evalCompOrFail', mirroring the Rust
-- benchmark's use of @eval_all@).
--
-- Takes the kv source\/doc sink ids as arguments rather than referencing
-- top-level ids, so 'benchMain' can derive them with 'typedCompSrcIdOf'\/
-- 'typedCompSinkIdOf' from the very 'HashMapFlow' instances it constructs --
-- each instance's name ("bench-kv"\/"bench-doc-sink") is then written down
-- exactly once, at the 'initHashMapFlow' call site.
wireAllComps :: TypedCompSrcId HashMapFlow -> TypedCompSinkId HashMapFlow -> [Word32] -> CompWireM (Comp () ())
wireAllComps kvSrcId docSinkId sizes = do
  levels <- buildLevels kvSrcId docSinkId sizes
  let topLevel = last levels
      topSize = last sizes
      groups = [[i | i <- [0 .. topSize - 1], i `mod` fromIntegral defsPerLevel == g] | g <- [0 .. fromIntegral defsPerLevel - 1]]
  wireComp $
    defineComp "root" fullCaching $ \() ->
      forM_ (zip topLevel groups) $ \(comp, is) -> mapM_ (evalCompOrFail comp) is

----------------------------------------
-- Memory measurement
----------------------------------------

-- | Shells out to @ps -o rss= -p \<this process\>@ (same method as the Rust
-- benchmark's @rss_mb@) and returns resident set size in MB (0 if @ps@'s
-- output doesn't parse).
getRssMb :: IO Double
getRssMb = do
  pid <- getProcessID
  out <- readProcess "ps" ["-o", "rss=", "-p", show pid] ""
  pure $ case words out of
    (kbStr : _) -> maybe 0 (\kb -> fromIntegral (kb :: Int) / 1024) (readMaybe kbStr)
    [] -> 0

----------------------------------------
-- Orchestration
----------------------------------------

-- 'waitForRunAtLeast' and 'waitForFullSettle' now live in
-- "Control.Computations.CompEngine.Driver" (re-exported via
-- "Control.Computations.CompEngine"): they're generic to any
-- 'compDriver''-style @TVar (Option RunStats)@, not bench-specific, and
-- promoting them lets "Control.Computations.CompEngine.Tests.TestDriver"
-- exercise their settle-detection contract directly.

benchMain :: IO ()
benchMain = do
  scale <- readScale
  let sizes = scaledLevelSizes scale
      targetInstances = 1000000 * scale :: Double
      topSize = last sizes

  putStrLn "=== bench: graph shape (persistence-free port of Rust persist_bench) ==="
  printf "PERSIST_BENCH_SCALE=%.4f (target ~%.0f achieved instances)\n" scale targetInstances
  printf "levels: %d, defs/level: %d, total defs: %d\n" levelsCount defsPerLevel (levelsCount * defsPerLevel)
  putStrLn ("declared level sizes (bottom -> top): " ++ show sizes)
  printf "source keys: %d, top-level sink outputs: %d\n" srcKeys topSize
  putStrLn ""

  kv <- initHashMapFlow "bench-kv"
  docSink <- initHashMapFlow "bench-doc-sink"

  -- Pre-populate the 300 default source keys (k -> k*7+3), mirroring the
  -- Rust benchmark's make_kv.
  forM_ [0 .. srcKeys - 1] $ \k ->
    hmfInsert kv (BSC.pack (show k)) (BSC.pack (show (fromIntegral k * 7 + 3 :: Word64)))

  runVar <- newTVarIO None
  counterRef <- newIORef 0
  t0 <- getCurrentTime
  allocated0 <- allocated_bytes <$> getRTSStats
  engineHandle <-
    async
      ( benchCompDriver
          counterRef
          runVar
          (withCompFlows kv docSink)
          (wireAllComps (typedCompSrcIdOf kv) (typedCompSinkIdOf docSink) sizes)
          ()
      )

  -- Cold settle: the *entire* initial evaluation runs synchronously inside
  -- Impl.startCompEngine, before the driver loop's first
  -- rcif_shouldStartWithRun call is even made -- so the first time
  -- compDriver' writes to runVar (run == 1) is exactly the moment the cold
  -- eval has finished.
  rs1 <- waitForRunAtLeast runVar 1
  tCold <- getCurrentTime
  coldReruns <- readIORef counterRef
  rssCold <- getRssMb
  allocatedCold <- allocated_bytes <$> getRTSStats

  let coldWallTime = realToFrac (diffUTCTime tCold t0) :: Double
      -- 'allocated_bytes' is GHC's own running total of bytes allocated
      -- since process start -- program-driven, not GC-driven, so for a fixed
      -- program and input it should be stable run to run, in a way wall time
      -- on this machine has proven not to be (see
      -- "Control.Computations.Demos.Bench.Hospital"'s equivalent note for the
      -- measured magnitude). The delta across a phase boundary is that
      -- phase's own allocation, independent of when GC happens to run.
      -- Deliberately *not* folded into the wall-time printf above -- these
      -- are two independently useful numbers, and burying one inside the
      -- other's format string would make either harder to grep out of a log.
      coldAllocated = allocatedCold - allocated0
  putStrLn "--- 1. cold eval (no persistence) ---"
  printf
    "achieved instance count: %d (target ~%.0f, %+.1f%%)\n"
    coldReruns
    targetInstances
    (100 * (fromIntegral coldReruns - targetInstances) / targetInstances)
  printf "wall time: %.3f s\n" coldWallTime
  printf "RSS after cold settle: %.1f MB\n" rssCold
  printf
    "allocated_bytes (cold eval): %d (%.1f MB)\n"
    coldAllocated
    (fromIntegral coldAllocated / 1000000 :: Double)

  -- Assert every top-level doc was written, mirroring the Rust benchmark's
  -- own assertion.
  docsAfterCold <- getHashMap docSink
  let expectedDocs = fromIntegral topSize :: Int
  when (HashMap.size docsAfterCold /= expectedDocs) $
    ioError
      ( userError
          ( "expected "
              ++ show expectedDocs
              ++ " top-level docs written, got "
              ++ show (HashMap.size docsAfterCold)
          )
      )

  -- The 300 pre-population inserts above are still sitting as unconsumed
  -- "changes" on the kv source (see HashMapFlow's changesVar): the driver
  -- loop's first real iteration drains them. Everything that read those
  -- keys during the cold eval recorded exactly the version now being
  -- reported as "changed", so this drains as a strictly version-matching
  -- no-op (0 reruns expected) -- but we still wait for it to *fully*
  -- complete (see 'waitForFullSettle') before mutating, so the live-update
  -- measurement below is attributable to *only* our mutation.
  --
  -- IMPORTANT: start the settle-poll from 'rs1's own run number, not
  -- @rs_run rs1 + 1@. 'waitForRunAtLeast' only guarantees @rs_run rs >= n@,
  -- not equality -- 'shouldStartNextRun' posts a run's 'RunStats' (tagged
  -- with the *upcoming* run number) describing the *previous* run's
  -- leftover 'rs_staleCaps' before that upcoming run has even attempted its
  -- own blocking wait. So 'rs1' can race ahead and already report run 2 (or
  -- later) with 'rs_staleCaps' 0 -- i.e. already fully settled. Blindly
  -- waiting for @rs_run rs1 + 1@ then asks for a run that will never come
  -- until *something* mutates a source, but nothing does until after this
  -- wait returns -- a genuine deadlock (reproduced: 'waitForRunAtLeast 1'
  -- observed run=2/staleCaps=0 while the engine thread was already blocked
  -- in 'compSrcWaitChanges', and 'waitForFullSettle' then waited on run 3
  -- forever).
  --
  -- ⚠ Residual ambiguity: 'rs1' is *itself* an eager
  -- pre-post -- describing run 0's (nonexistent, trivially-0) leftover --
  -- so "seed with rs1's own run number" can ALSO return before run 1's own
  -- body (this 300-key drain) has even looked for changes, not just after.
  -- Both "genuinely fully drained and blocked" and "hasn't started yet"
  -- present as @rs_staleCaps == 0@ at run 1; the 'RunStats' payload alone
  -- can't tell them apart. Harmless when the engine is slow relative to how
  -- quickly this gets checked, but observable when the engine is fast
  -- enough to close that gap: confirmed via a diagnostic
  -- counterRef recheck that 'waitForFullSettle' can return instantly here
  -- while ~10k real reruns are still in flight, uncounted, on the driver
  -- thread. Not a Driver.hs contract bug (its "at least" semantics are
  -- exactly as documented) and not touched here -- resolved locally: race
  -- a bounded wait for a *confirmed* advance past run 1 against a timeout,
  -- falling back to trusting 'rs1' iff the timeout fires (which only
  -- happens in the genuinely-already-settled case, where 'rs1' was correct
  -- all along).
  mRs2Advanced <- timeout 2000000 (waitForFullSettle runVar (rs_run rs1 + 1))
  rs2 <- case mRs2Advanced of
    Just rs -> pure rs
    Nothing -> waitForFullSettle runVar (rs_run rs1)
  preLiveReruns <- readIORef counterRef
  when (preLiveReruns /= coldReruns) $
    putStrLn
      ( "NOTE: draining leftover pre-population changes triggered "
          ++ show (preLiveReruns - coldReruns)
          ++ " unexpected reruns (expected 0)"
      )

  -- 2. Live incremental update: mutate exactly one source key on the
  -- still-running engine and time until the driver's next propagation
  -- round *fully* completes (see 'waitForFullSettle' -- at large scale a
  -- single mutation's cascade can exceed the driver's per-iteration
  -- 'rcif_maxLoopRunTime' budget and span several run numbers).
  --
  -- Unlike the cold-drain wait above, @rs_run rs2 + 1@ *is* safe here:
  -- 'rs2' is 'waitForFullSettle's postcondition (genuinely 0 leftover
  -- stale caps), and with 'rcif_emptyChangesMode' = 'Block' the driver
  -- cannot advance its run counter again until a source actually changes --
  -- so the engine is provably parked, still tagged with run 'rs_run rs2',
  -- inside its blocking wait at the moment we mutate. The very next
  -- 'RunStats' the driver posts is therefore guaranteed to report the
  -- outcome of processing *this* mutation.
  tBeforeMutate <- getCurrentTime
  allocatedPreLive <- allocated_bytes <$> getRTSStats
  hmfInsert kv "0" "13371337"
  _rs3 <- waitForFullSettle runVar (rs_run rs2 + 1)
  tAfterMutate <- getCurrentTime
  allocatedPostLive <- allocated_bytes <$> getRTSStats
  liveReruns <- readIORef counterRef
  let liveWallTime = realToFrac (diffUTCTime tAfterMutate tBeforeMutate) :: Double
      liveRerunCount = liveReruns - preLiveReruns
      liveAllocated = allocatedPostLive - allocatedPreLive

  putStrLn ""
  putStrLn "--- 2. live incremental, 1 changed input (no persistence) ---"
  printf "wall time: %.4f s\n" liveWallTime
  printf "reruns: %d\n" liveRerunCount
  printf "allocated_bytes (live update): %d\n" liveAllocated

  rssFinal <- getRssMb
  rtsStats <- getRTSStats

  putStrLn ""
  putStrLn "--- memory ---"
  printf "RSS at end: %.1f MB\n" rssFinal
  printf
    "GHC max_live_bytes: %d (%.1f MB)\n"
    (max_live_bytes rtsStats)
    (fromIntegral (max_live_bytes rtsStats) / 1000000 :: Double)
  printf
    "GHC max_mem_in_use_bytes: %d (%.1f MB)\n"
    (max_mem_in_use_bytes rtsStats)
    (fromIntegral (max_mem_in_use_bytes rtsStats) / 1000000 :: Double)
  printf "GHC gcs: %d\n" (gcs rtsStats)

  when (coldReruns > 0) $ do
    printf
      "bytes/instance (RSS after cold settle / instances): %.1f B/instance\n"
      (rssCold * 1000000 / fromIntegral coldReruns :: Double)
    printf
      "bytes/instance (GHC max_live_bytes / instances): %.1f B/instance\n"
      (fromIntegral (max_live_bytes rtsStats) / fromIntegral coldReruns :: Double)

  -- OPTIONAL DIAGNOSTIC, off by default: repeats the mutate-and-settle step
  -- so a profile taken over the whole process is dominated by live-phase
  -- propagation rather than cold eval. Runs strictly after every number
  -- reported above, so it never perturbs the existing measurements; a run with
  -- PERSIST_BENCH_LIVE_LOOPS unset (or <=1) reproduces them byte-for-byte.
  -- Cycles source keys "0".."299" (a fresh value each time) so each
  -- iteration is a genuine distinct-key mutation, matching the shape of the
  -- single-key measurement above rather than a no-op repeat.
  liveLoops <- maybe 1 (max 1) . (>>= readMaybe) <$> lookupEnv "PERSIST_BENCH_LIVE_LOOPS"
  when (liveLoops > 1) $ do
    nextRunRef <- newIORef (rs_run rs2 + 1)
    loopRerunsRef <- newIORef (0 :: Int)
    tLoopStart <- getCurrentTime
    allocatedLoopStart <- allocated_bytes <$> getRTSStats
    forM_ [1 .. liveLoops - 1] $ \n -> do
      let key = BSC.pack (show (n `mod` fromIntegral srcKeys :: Int))
          val = BSC.pack (show (1000000 + n :: Int))
      before <- readIORef counterRef
      nextRun <- readIORef nextRunRef
      hmfInsert kv key val
      rsN <- waitForFullSettle runVar nextRun
      writeIORef nextRunRef (rs_run rsN + 1)
      after <- readIORef counterRef
      modifyIORef' loopRerunsRef (+ (after - before))
    tLoopEnd <- getCurrentTime
    allocatedLoopEnd <- allocated_bytes <$> getRTSStats
    loopReruns <- readIORef loopRerunsRef
    let loopWall = realToFrac (diffUTCTime tLoopEnd tLoopStart) :: Double
        loopAllocated = allocatedLoopEnd - allocatedLoopStart
    putStrLn ""
    putStrLn "--- 2b. live incremental loop (diagnostic, PERSIST_BENCH_LIVE_LOOPS) ---"
    printf "loop iterations: %d\n" (liveLoops - 1)
    printf "loop wall time: %.4f s\n" loopWall
    printf "loop reruns: %d\n" loopReruns
    printf
      "loop allocated_bytes: %d (%.1f MB)\n"
      loopAllocated
      (fromIntegral loopAllocated / 1000000 :: Double)
    when (loopReruns > 0) $ do
      printf "loop us/rerun: %.3f\n" (loopWall * 1e6 / fromIntegral loopReruns :: Double)
      -- A program-driven, GC-timing-independent proxy for work done per
      -- rerun, comparable across runs and across machines in a way
      -- wall-clock us/rerun above is not: 'allocated_bytes' counts what GHC
      -- allocated, not how long the OS scheduler and GC pauses happened to
      -- take to get there.
      printf
        "loop allocated_bytes/rerun: %.1f B/rerun\n"
        (fromIntegral loopAllocated / fromIntegral loopReruns :: Double)

  cancel engineHandle
