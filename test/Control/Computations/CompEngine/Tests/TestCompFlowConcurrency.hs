{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | Exercises the bounded worker pool a wide 'CompReqCombined' batch (see
 "Control.Computations.CompEngine.Types") may dispatch 'FlowConcurrent'
 source *groups* to once 'Control.Computations.CompEngine.CompFlowRegistry.setCompFlowConcurrency'
 raises a registry's width above 1 -- see "Control.Computations.CompEngine.Impl"'s
 @prepSrcLeaf@\/@Prep@\/@SrcGroup@\/@runGroupOnce@ for the implementation
 these tests pin down. "Control.Computations.CompEngine.Tests.TestCompReqCombined"
 keeps the width-1 (default, unchanged-behaviour) regression guards; this
 module is specifically about width > 1.

 Since every 'CompLeafSrc' leaf resolving to the same instance within one
 batch is now bundled into a single
 'Control.Computations.CompEngine.CompSrc.compSrcExecuteBatch' call (see
 that method's haddock), "several requests against the same instance
 overlap at width > 1" is no longer something the engine's job dispatch
 provides by itself -- a batch against one instance is one call regardless
 of width. It is still achievable, but now it is the instance's own
 responsibility, via its 'compSrcExecuteBatch' override (see
 'ConcurrentBatchSrc' below) -- exactly like a real batched backend that can
 issue several sub-requests of one round trip concurrently would do it
 itself, rather than relying on the engine to schedule its point lookups on
 separate threads.

 Every test here builds its own 'Control.Computations.CompEngine.CompFlowRegistry.CompFlowRegistry'
 by hand (rather than going through "Control.Computations.CompEngine.Tests.TestHelper"'s
 @initCompEngineTest@) precisely so it can call
 'Control.Computations.CompEngine.CompFlowRegistry.setCompFlowConcurrency'
 on it before running -- @initCompEngineTest@ never hands its registry back
 to the caller.
-}
module Control.Computations.CompEngine.Tests.TestCompFlowConcurrency (htf_thisModulesTests) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CacheBehaviors
import Control.Computations.CompEngine.CompDef
import Control.Computations.CompEngine.CompEval
import Control.Computations.CompEngine.CompFlowRegistry
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Tests.ObservingStateIf
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.ConcUtils (timeout, trySync)
import Control.Computations.Utils.Fail
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types (Option (..))

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent (rtsSupportsBoundThreads, threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (IOException, finally, throwIO)
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Data.IORef
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import Test.Framework

----------------------------------------------------------------------------
-- Shared fixture: a CompSrc whose one request runs an arbitrary IO action.
-- Every test below builds its own instance(s), so the interesting behaviour
-- (blocking, throwing, counting overlap) lives entirely in scr_action
-- closures rather than in a family of near-identical CompSrc types.
----------------------------------------------------------------------------

data ScriptedReq a where
  ScriptedReq :: ScriptedReq Int

data ScriptedSrc = ScriptedSrc
  { scr_name :: T.Text
  , scr_conc :: FlowConcurrency
  , scr_action :: IO Int
  }
  deriving (Typeable)

instance CompSrc ScriptedSrc where
  type CompSrcReq ScriptedSrc = ScriptedReq
  type CompSrcKey ScriptedSrc = Int
  type CompSrcVer ScriptedSrc = Int
  compSrcInstanceId = CompSrcInstanceId . scr_name
  compSrcExecute src ScriptedReq =
    do
      v <- scr_action src
      pure (HashSet.singleton (Dep 0 v), Ok v)
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges _ = retry
  compSrcConcurrency = scr_conc

-- | Build the RunCompEngineIf/CompEngineIfs plumbing every test below needs
-- and run exactly one cold-start evaluation of @mainCompDef@ against @reg@
-- (no driver loop, no reruns -- 'noNextRun' stops right after the first
-- settle), mirroring "TestCompReqCombined"'s own hand-wired tests
-- (test_goldenOrderingTraceAcrossSrcEvalSink and friends).
runOnce :: CompFlowRegistry -> CompDef () () -> IO ()
runOnce reg mainCompDef =
  do
    (rawStateIf, closeSif) <- initStateIf True
    (_compMap, mainComp) <- failInM $ runCompWireM (wireComp mainCompDef)
    let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = rawStateIf}
        caps = [wrapCompAp (mkCompAp mainComp ())]
        rifs =
          RunCompEngineIf
            { rcif_shouldStartWithRun = \_ _ _ s -> pure (noNextRun, s)
            , rcif_emptyChangesMode = DontBlock
            , rcif_getTime = getCurrentTime
            , rcif_maxLoopRunTime = seconds 10
            , rcif_maxRunIterations = CompRunUnlimitedIterations
            , rcif_reportGarbage = \_ -> pure ()
            }
    runCompEngine ifs caps rifs () `finally` closeSif

----------------------------------------------------------------------------
-- 1 & 2: FlowSerial never overlaps regardless of width; FlowConcurrent
-- genuinely does at width > 1.
----------------------------------------------------------------------------

-- | Bump an in-flight counter, record the high-water mark it reaches, hold
-- for ~5ms (long enough that eight genuinely-concurrent callers overlap
-- rather than racing past each other), then decrement. Called from
-- 'scr_action' by every request against the returned source, so
-- 'highWater' after a batch of concurrent calls directly answers "how many
-- callers were inside this at once, at worst".
mkOverlapAction :: TVar Int -> TVar Int -> IO Int
mkOverlapAction inFlight highWater =
  do
    atomically $
      do
        n <- (+ 1) <$> readTVar inFlight
        writeTVar inFlight n
        hw <- readTVar highWater
        when (n > hw) (writeTVar highWater n)
    threadDelay 5000
    atomically (modifyTVar' inFlight (subtract 1))
    pure 0

-- | Eight distinct requests against the *same* instance, combined via
-- 'traverse' (not 'mapM_') so all eight land in one 'CompReqCombined' batch
-- -- see "TestCompReqCombined"'s B.1 for why 'traverse' specifically is
-- essential to reach the general (Prep-based) path rather than eight
-- sequential monadic steps.
mutexMainCompDef :: TypedCompSrcId ScriptedSrc -> CompDef () ()
mutexMainCompDef srcId =
  defineComp "mutex-main" inMemoryShowCaching $ \() ->
    void (traverse (\_ -> compSrcReq srcId ScriptedReq) [1 .. 8 :: Int])

runMutualExclusionCase :: FlowConcurrency -> IO Int
runMutualExclusionCase conc =
  do
    inFlight <- newTVarIO 0
    highWater <- newTVarIO 0
    let src = ScriptedSrc "mutex-src" conc (mkOverlapAction inFlight highWater)
    reg <- newCompFlowRegistry
    setCompFlowConcurrency reg (mkCompFlowConcurrency 8)
    registerCompSrc reg src
    runOnce reg (mutexMainCompDef (typedCompSrcIdOf src))
    readTVarIO highWater

-- | All eight requests above land in one batch against one instance, so the
-- engine bundles them into a single 'compSrcExecuteBatch' call regardless
-- of 'FlowConcurrency' or width (see the module haddock) -- 'ScriptedSrc'
-- never overrides that method, so its default (sequential) implementation
-- serves all eight itself, one at a time, whether the instance declares
-- 'FlowSerial' or 'FlowConcurrent'. This is the width-1-style "no overlap"
-- case for *both* declarations now; 'FlowConcurrent's own overlap guarantee
-- moves to 'test_flowConcurrentSourceGenuinelyOverlapsAtWidth8' below, via
-- an instance that actually implements it.
test_flowSerialSourceNeverOverlapsAtWidth8 :: IO ()
test_flowSerialSourceNeverOverlapsAtWidth8 =
  do
    hw <- runMutualExclusionCase FlowSerial
    assertEqual 1 hw

test_flowConcurrentSourceWithDefaultBatchExecNeverOverlapsAtWidth8 :: IO ()
test_flowConcurrentSourceWithDefaultBatchExecNeverOverlapsAtWidth8 =
  do
    hw <- runMutualExclusionCase FlowConcurrent
    assertEqual 1 hw

----------------------------------------------------------------------------
-- 1a: a source whose 'compSrcExecuteBatch' override actually parallelizes
-- its own sub-requests -- what "several requests against one instance
-- overlap" now requires, since bundling means the engine itself only ever
-- makes one call per instance per batch (see the module haddock).
----------------------------------------------------------------------------

-- | Like 'ScriptedSrc', but overrides 'compSrcExecuteBatch' to run every
-- request in the group concurrently via 'Async.mapConcurrently_', the shape
-- a real batched backend capable of overlapping its own sub-requests within
-- one round trip (e.g. several concurrent socket reads multiplexed over one
-- connection) would use. 'compSrcExecute' itself is never called here
-- (nothing in these tests issues a genuinely unbatched request), so it is
-- left undefined-but-never-forced\'s cousin: a body that would only run if
-- something regressed and started bypassing the batch override.
data ConcurrentBatchSrc = ConcurrentBatchSrc
  { cbs_name :: T.Text
  , cbs_action :: IO Int
  }
  deriving (Typeable)

instance CompSrc ConcurrentBatchSrc where
  type CompSrcReq ConcurrentBatchSrc = ScriptedReq
  type CompSrcKey ConcurrentBatchSrc = Int
  type CompSrcVer ConcurrentBatchSrc = Int
  compSrcInstanceId = CompSrcInstanceId . cbs_name
  compSrcExecute src ScriptedReq =
    do
      v <- cbs_action src
      pure (HashSet.singleton (Dep 0 v), Ok v)
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges _ = retry
  compSrcConcurrency _ = FlowConcurrent
  compSrcExecuteBatch src fs =
    Async.mapConcurrently_ (\(SrcFetch req write) -> trySync (compSrcExecute src req) >>= write) fs

mutexConcurrentMainCompDef :: TypedCompSrcId ConcurrentBatchSrc -> CompDef () ()
mutexConcurrentMainCompDef srcId =
  defineComp "mutex-concurrent-main" inMemoryShowCaching $ \() ->
    void (traverse (\_ -> compSrcReq srcId ScriptedReq) [1 .. 8 :: Int])

-- | Guarded on 'rtsSupportsBoundThreads' so this can't go flaky on a
-- non-threaded RTS, where width > 1 is deliberately a no-op (see
-- prepSrcLeaf's haddock) -- this repo's own test stanza always builds
-- @-threaded -with-rtsopts=-N@, so the guard holds here.
test_flowConcurrentSourceGenuinelyOverlapsAtWidth8 :: IO ()
test_flowConcurrentSourceGenuinelyOverlapsAtWidth8 =
  when rtsSupportsBoundThreads $
    do
      inFlight <- newTVarIO 0
      highWater <- newTVarIO 0
      let src = ConcurrentBatchSrc "mutex-concurrent-src" (mkOverlapAction inFlight highWater)
      reg <- newCompFlowRegistry
      setCompFlowConcurrency reg (mkCompFlowConcurrency 8)
      registerCompSrc reg src
      runOnce reg (mutexConcurrentMainCompDef (typedCompSrcIdOf src))
      hw <- readTVarIO highWater
      assertBool (hw > 1)

----------------------------------------------------------------------------
-- 1b: the FlowSerial guarantee across two DIFFERENT comps' bodies, each
-- forming its own nested batch against the same instance, when both bodies
-- are themselves forked onto separate threads by eval-width > 1 (see
-- Impl.hs's prepEvalLeaf). Tests 1/1a above only ever put every request
-- against one instance into a single shared batch/SrcGroup, so they would
-- keep passing even if the per-instance locks this module's own
-- CompFlowRegistry now takes (lookupSrcLock, consulted from runGroupOnce)
-- were removed entirely -- there is only ever one SrcGroup object in play
-- there, so its own sg_triggered guard alone is enough to prevent a second
-- 'compSrcExecuteBatch' call. Here, compA and compB are different comps, so
-- each gets its OWN groupsRef (fresh per top-level batch -- see doSuspended's
-- CompReqCombined case) and therefore its own SrcGroup object, both
-- resolving to the *same* registered 'ScriptedSrc' instance underneath.
-- Without the registry-level lock, nothing stops compA's runGroupOnce call
-- (running on its own forked thread) from overlapping compB's.
----------------------------------------------------------------------------

-- | A comp whose body issues its own nested batch of 8 requests against the
-- same 'FlowSerial' instance -- mirroring 'mutexMainCompDef' above, just
-- nested one level deeper inside a cap body that is itself a candidate for
-- 'prepEvalLeaf' forking.
crossBodyLeafCompDef :: String -> TypedCompSrcId ScriptedSrc -> CompDef () Int
crossBodyLeafCompDef name srcId =
  defineComp name inMemoryShowCaching $ \() ->
    sum <$> traverse (\_ -> compSrcReq srcId ScriptedReq) [1 .. 8 :: Int]

-- | Exactly two eval leaves (compA, compB) combined via `<*>` -- the shape
-- test 6's own comment identifies as needing width > 1 to escape the
-- two-leaf fast path (see doSuspended) and reach prepEvalLeaf at all; the
-- source-side width set in 'runCrossBodyOverlapCase' below takes care of
-- that here.
crossBodyMainCompDef :: Comp () Int -> Comp () Int -> CompDef () (Int, Int)
crossBodyMainCompDef compA compB =
  defineComp "cross-body-main" inMemoryShowCaching $ \() ->
    (,) <$> evalCompOrFail compA () <*> evalCompOrFail compB ()

-- | Wired by hand, like test_noCapEvaluatedTwiceAtWidth8 below, rather than
-- through 'runOnce': compA/compB must be wired *before* crossBodyMainCompDef
-- is built, since it closes over them as already-wired 'Comp' values (the
-- same reason dedupMainCompDef above takes its leaf as a parameter instead
-- of wiring it internally).
runCrossBodyOverlapCase :: IO Int
runCrossBodyOverlapCase =
  do
    inFlight <- newTVarIO 0
    highWater <- newTVarIO 0
    let src = ScriptedSrc "cross-body-src" FlowSerial (mkOverlapAction inFlight highWater)
    reg <- newCompFlowRegistry
    setCompFlowConcurrency reg (mkCompFlowConcurrency 8)
    setCompEvalConcurrency reg (mkCompFlowConcurrency 8)
    registerCompSrc reg src
    (rawStateIf, closeSif) <- initStateIf True
    (_compMap, mainComp) <-
      failInM $
        runCompWireM $
          do
            compA <- wireComp (crossBodyLeafCompDef "cross-body-leaf-a" (typedCompSrcIdOf src))
            compB <- wireComp (crossBodyLeafCompDef "cross-body-leaf-b" (typedCompSrcIdOf src))
            wireComp (crossBodyMainCompDef compA compB)
    let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = rawStateIf}
        caps = [wrapCompAp (mkCompAp mainComp ())]
        rifs =
          RunCompEngineIf
            { rcif_shouldStartWithRun = \_ _ _ s -> pure (noNextRun, s)
            , rcif_emptyChangesMode = DontBlock
            , rcif_getTime = getCurrentTime
            , rcif_maxLoopRunTime = seconds 10
            , rcif_maxRunIterations = CompRunUnlimitedIterations
            , rcif_reportGarbage = \_ -> pure ()
            }
    runCompEngine ifs caps rifs () `finally` closeSif
    readTVarIO highWater

-- | Guarded on 'rtsSupportsBoundThreads' for the same reason as
-- test_flowConcurrentSourceGenuinelyOverlapsAtWidth8 above: without real
-- OS-thread concurrency, no eval leaf ever forks, so this would trivially
-- pass (both bodies would run sequentially on the same thread) whether or
-- not the fix under test exists.
test_flowSerialSourceNeverOverlapsAcrossForkedBodiesAtWidth8 :: IO ()
test_flowSerialSourceNeverOverlapsAcrossForkedBodiesAtWidth8 =
  when rtsSupportsBoundThreads $
    do
      hw <- runCrossBodyOverlapCase
      assertEqual 1 hw

----------------------------------------------------------------------------
-- 1c: the analogous guarantee for a 'FlowSerial' *sink* -- see
-- "Control.Computations.CompEngine.CompSink"'s @compSinkConcurrency@ and
-- "Control.Computations.CompEngine.Impl"'s @withSinkInstLock@, taken around
-- every 'compSinkExecute' call (@doCompSinkReq@\/@doCompSinkReqValue@)
-- unconditionally, unlike the source side's lock, which only guards
-- 'runGroupOnce' (see 1b's own comment) -- so, unlike 1b, a single
-- (non-batched) 'compSinkReq' per body is already enough to reach the
-- locked path; no nested batch is needed to force it.
----------------------------------------------------------------------------

data ScriptedWriteReq a where
  ScriptedWriteReq :: ScriptedWriteReq ()

data ScriptedSink = ScriptedSink
  { sks_name :: T.Text
  , sks_conc :: FlowConcurrency
  , sks_action :: IO ()
  }
  deriving (Typeable)

instance CompSink ScriptedSink where
  type CompSinkReq ScriptedSink = ScriptedWriteReq
  type CompSinkOut ScriptedSink = Int
  compSinkInstanceId = CompSinkInstanceId . sks_name
  compSinkExecute sink ScriptedWriteReq =
    do
      sks_action sink
      pure (HashSet.empty, Ok ())
  compSinkDeleteOutputs _ _ = pure ()
  compSinkListExistingOutputs _ = None
  compSinkConcurrency = sks_conc

-- | A single, non-batched sink write -- see this section's own header
-- comment for why that's already enough here, unlike the source-side
-- 1b, which needs its leaf wrapped in a nested 'traverse' batch to reach
-- 'runGroupOnce' at all.
crossBodySinkLeafCompDef :: String -> TypedCompSinkId ScriptedSink -> CompDef () ()
crossBodySinkLeafCompDef name sinkId =
  defineComp name inMemoryShowCaching $ \() ->
    compSinkReq sinkId ScriptedWriteReq

crossBodySinkMainCompDef :: Comp () () -> Comp () () -> CompDef () ((), ())
crossBodySinkMainCompDef compA compB =
  defineComp "cross-body-sink-main" inMemoryShowCaching $ \() ->
    (,) <$> evalCompOrFail compA () <*> evalCompOrFail compB ()

-- | Wired by hand for the same reason 'runCrossBodyOverlapCase' above is.
runCrossBodySinkOverlapCase :: IO Int
runCrossBodySinkOverlapCase =
  do
    inFlight <- newTVarIO 0
    highWater <- newTVarIO 0
    let sink = ScriptedSink "cross-body-sink" FlowSerial (void (mkOverlapAction inFlight highWater))
    reg <- newCompFlowRegistry
    setCompFlowConcurrency reg (mkCompFlowConcurrency 8)
    setCompEvalConcurrency reg (mkCompFlowConcurrency 8)
    registerCompSink reg sink
    (rawStateIf, closeSif) <- initStateIf True
    (_compMap, mainComp) <-
      failInM $
        runCompWireM $
          do
            compA <- wireComp (crossBodySinkLeafCompDef "cross-body-sink-leaf-a" (typedCompSinkIdOf sink))
            compB <- wireComp (crossBodySinkLeafCompDef "cross-body-sink-leaf-b" (typedCompSinkIdOf sink))
            wireComp (crossBodySinkMainCompDef compA compB)
    let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = rawStateIf}
        caps = [wrapCompAp (mkCompAp mainComp ())]
        rifs =
          RunCompEngineIf
            { rcif_shouldStartWithRun = \_ _ _ s -> pure (noNextRun, s)
            , rcif_emptyChangesMode = DontBlock
            , rcif_getTime = getCurrentTime
            , rcif_maxLoopRunTime = seconds 10
            , rcif_maxRunIterations = CompRunUnlimitedIterations
            , rcif_reportGarbage = \_ -> pure ()
            }
    runCompEngine ifs caps rifs () `finally` closeSif
    readTVarIO highWater

-- | Guarded on 'rtsSupportsBoundThreads' for the same reason 1b's own
-- test is.
test_flowSerialSinkNeverOverlapsAcrossForkedBodiesAtWidth8 :: IO ()
test_flowSerialSinkNeverOverlapsAcrossForkedBodiesAtWidth8 =
  when rtsSupportsBoundThreads $
    do
      hw <- runCrossBodySinkOverlapCase
      assertEqual 1 hw

----------------------------------------------------------------------------
-- 3: no cap gets evaluated twice at width 8, even when the same batch also
-- dispatches concurrent source jobs alongside the eval leaves.
----------------------------------------------------------------------------

dedupLeafCompDef :: CompDef Int Int
dedupLeafCompDef = defineComp "dedup-leaf" fullCaching $ \p -> pure p

dedupMainCompDef :: Comp Int Int -> TypedCompSrcId ScriptedSrc -> CompDef () ()
dedupMainCompDef leaf srcId =
  defineComp "dedup-main" inMemoryShowCaching $ \() ->
    void $
      (,)
        <$> traverse (\i -> evalCompOrFail leaf (i `mod` 10)) [0 .. 999 :: Int]
        <*> traverse (\_ -> compSrcReq srcId ScriptedReq) [1 .. 32 :: Int]

test_noCapEvaluatedTwiceAtWidth8 :: IO ()
test_noCapEvaluatedTwiceAtWidth8 =
  do
    countsRef <- newIORef HashMap.empty
    let src = ScriptedSrc "dedup-src" FlowConcurrent (pure 0)
    reg <- newCompFlowRegistry
    setCompFlowConcurrency reg (mkCompFlowConcurrency 8)
    registerCompSrc reg src
    (rawStateIf, closeSif) <- initStateIf True
    let stateIf =
          observingStateIf
            ( \cap ->
                atomicModifyIORef'
                  countsRef
                  (\m -> (HashMap.insertWith (+) (show cap) (1 :: Int) m, ()))
            )
            rawStateIf
    (_compMap, mainComp) <-
      failInM $
        runCompWireM $
          do
            leaf <- wireComp dedupLeafCompDef
            wireComp (dedupMainCompDef leaf (typedCompSrcIdOf src))
    let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = stateIf}
        caps = [wrapCompAp (mkCompAp mainComp ())]
        rifs =
          RunCompEngineIf
            { rcif_shouldStartWithRun = \_ _ _ s -> pure (noNextRun, s)
            , rcif_emptyChangesMode = DontBlock
            , rcif_getTime = getCurrentTime
            , rcif_maxLoopRunTime = seconds 10
            , rcif_maxRunIterations = CompRunUnlimitedIterations
            , rcif_reportGarbage = \_ -> pure ()
            }
    runCompEngine ifs caps rifs () `finally` closeSif
    counts <- readIORef countsRef
    let leafCounts = HashMap.filterWithKey (\k _ -> "dedup-leaf" `L.isPrefixOf` k) counts
    assertEqual 10 (HashMap.size leafCounts)
    assertEqual (replicate 10 (1 :: Int)) (HashMap.elems leafCounts)

----------------------------------------------------------------------------
-- 4: when several concurrently-dispatched source leaves all throw, the
-- exception that escapes is the LEFTMOST failing leaf's -- mirroring
-- compMAp's left-error bias at the Fail level (Types.hs) at the exception
-- level too.
----------------------------------------------------------------------------

boomMainCompDef
  :: TypedCompSrcId ScriptedSrc
  -> TypedCompSrcId ScriptedSrc
  -> TypedCompSrcId ScriptedSrc
  -> CompDef () ()
boomMainCompDef leftId harmlessId rightId =
  defineComp "boom-main" inMemoryShowCaching $ \() ->
    void $
      (,,)
        <$> compSrcReq leftId ScriptedReq
        <*> compSrcReq harmlessId ScriptedReq
        <*> compSrcReq rightId ScriptedReq

test_leftmostFailingSourceLeafExceptionEscapesAtWidth8 :: IO ()
test_leftmostFailingSourceLeafExceptionEscapesAtWidth8 =
  do
    let boomLeft = ScriptedSrc "boom-left" FlowConcurrent (throwIO (userError "left boom"))
        boomHarmless = ScriptedSrc "boom-harmless" FlowConcurrent (pure 0)
        boomRight = ScriptedSrc "boom-right" FlowConcurrent (throwIO (userError "right boom"))
    reg <- newCompFlowRegistry
    setCompFlowConcurrency reg (mkCompFlowConcurrency 8)
    registerCompSrc reg boomLeft
    registerCompSrc reg boomHarmless
    registerCompSrc reg boomRight
    let mainCompDef =
          boomMainCompDef
            (typedCompSrcIdOf boomLeft)
            (typedCompSrcIdOf boomHarmless)
            (typedCompSrcIdOf boomRight)
    assertThrowsIO
      (runOnce reg mainCompDef)
      (\e -> "left boom" `L.isInfixOf` show (e :: IOException))

----------------------------------------------------------------------------
-- 5: Async.cancel reaching a worker blocked inside compSrcExecute tears the
-- pool down instead of leaking a thread that keeps running past the point
-- the engine that owned it is gone.
----------------------------------------------------------------------------

cancelMainCompDef
  :: TypedCompSrcId ScriptedSrc
  -> TypedCompSrcId ScriptedSrc
  -> TypedCompSrcId ScriptedSrc
  -> CompDef () ()
cancelMainCompDef blockedId filler1Id filler2Id =
  defineComp "cancel-main" inMemoryShowCaching $ \() ->
    void $
      (,,)
        <$> compSrcReq blockedId ScriptedReq
        <*> compSrcReq filler1Id ScriptedReq
        <*> compSrcReq filler2Id ScriptedReq

test_cancelTearsDownBlockedWorker :: IO ()
test_cancelTearsDownBlockedWorker =
  do
    activityRef <- newIORef (0 :: Int)
    mvar <- newEmptyMVar
    let blocked =
          ScriptedSrc "cancel-blocked" FlowConcurrent $
            do
              takeMVar mvar
              atomicModifyIORef' activityRef (\n -> (n + 1, ()))
              pure 0
        filler1 = ScriptedSrc "cancel-filler1" FlowConcurrent (pure 1)
        filler2 = ScriptedSrc "cancel-filler2" FlowConcurrent (pure 2)
    reg <- newCompFlowRegistry
    setCompFlowConcurrency reg (mkCompFlowConcurrency 4)
    registerCompSrc reg blocked
    registerCompSrc reg filler1
    registerCompSrc reg filler2
    let mainCompDef =
          cancelMainCompDef
            (typedCompSrcIdOf blocked)
            (typedCompSrcIdOf filler1)
            (typedCompSrcIdOf filler2)
    engineAsync <- Async.async (runOnce reg mainCompDef)
    -- Give the worker pool time to actually reach the blocking takeMVar
    -- before cancelling -- not a correctness requirement (cancel is safe at
    -- any point), just what makes this test actually exercise "cancel
    -- while a worker is blocked" rather than "cancel before dispatch".
    threadDelay 50000
    mCancelled <- timeout (seconds 5) (Async.cancel engineAsync)
    assertBool (mCancelled == Just ())
    -- The worker's takeMVar was already torn down by the cancel above, so
    -- filling the MVar now has no reader left -- if activityRef ever moved,
    -- the worker survived cancellation and kept running.
    putMVar mvar ()
    threadDelay 50000
    activityAfter <- readIORef activityRef
    assertEqual 0 activityAfter

----------------------------------------------------------------------------
-- 6: the plain two-leaf shape `(,) <$> compSrcReq srcA .. <*> compSrcReq
-- srcB ..` -- the overwhelmingly common shape a batch of exactly two
-- suspended actions takes -- must still let its two FlowConcurrent source
-- leaves genuinely overlap once width > 1. doSuspended's CompReqCombined
-- case (Impl.hs) used to take a fast path whenever neither child was
-- itself a CompReqCombined, with no regard for width at all, so a plain
-- `f <$> a <*> b` never reached dispatchJobs at any width -- only batches
-- of three or more leaves (reached via traverse, as in the mutex/dedup
-- tests above) ever got dispatched as jobs. The fast path is now taken
-- only at width 1, where it is provably a no-op change (no source leaf
-- could ever be dispatched as a job at width 1 anyway).
----------------------------------------------------------------------------

twoLeafMainCompDef
  :: TypedCompSrcId ScriptedSrc -> TypedCompSrcId ScriptedSrc -> CompDef () ()
twoLeafMainCompDef srcAId srcBId =
  defineComp "two-leaf-main" inMemoryShowCaching $ \() ->
    void $ (,) <$> compSrcReq srcAId ScriptedReq <*> compSrcReq srcBId ScriptedReq

-- | Two distinct 'FlowConcurrent' instances sharing one @inFlight@\/
-- @highWater@ pair, so 'highWater' after the run is the *global* high-water
-- mark across both -- 2 only if the two leaves' 'compSrcExecute' calls
-- genuinely ran at the same time, not merely if each instance's own calls
-- never overlapped themselves (there's only one call per instance here
-- anyway).
runTwoLeafOverlapCase :: Int -> IO Int
runTwoLeafOverlapCase width =
  do
    inFlight <- newTVarIO 0
    highWater <- newTVarIO 0
    let srcA = ScriptedSrc "two-leaf-a" FlowConcurrent (mkOverlapAction inFlight highWater)
        srcB = ScriptedSrc "two-leaf-b" FlowConcurrent (mkOverlapAction inFlight highWater)
    reg <- newCompFlowRegistry
    setCompFlowConcurrency reg (mkCompFlowConcurrency width)
    registerCompSrc reg srcA
    registerCompSrc reg srcB
    runOnce reg (twoLeafMainCompDef (typedCompSrcIdOf srcA) (typedCompSrcIdOf srcB))
    readTVarIO highWater

-- | Guarded on 'rtsSupportsBoundThreads' for the same reason as
-- test_flowConcurrentSourceGenuinelyOverlapsAtWidth8 above: without real
-- OS-thread concurrency, width > 1 is deliberately a no-op.
test_twoLeafBatchGenuinelyOverlapsAtWidth4 :: IO ()
test_twoLeafBatchGenuinelyOverlapsAtWidth4 =
  when rtsSupportsBoundThreads $
    do
      hw <- runTwoLeafOverlapCase 4
      assertEqual 2 hw

-- | Same two-leaf shape, but at width 1 -- the fast path's own domain --
-- where the two source leaves must run one at a time, exactly as before
-- jobs existed.
test_twoLeafBatchNeverOverlapsAtWidth1 :: IO ()
test_twoLeafBatchNeverOverlapsAtWidth1 =
  do
    hw <- runTwoLeafOverlapCase 1
    assertEqual 1 hw
