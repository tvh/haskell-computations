{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | Exercises what 'FlowSerial'\/'FlowConcurrent' actually guarantee now that
 there is no proactive source dispatch:
 'Control.Computations.CompEngine.CompFlowRegistry.setCompEvalConcurrency'
 controls only *eval*-leaf forking (see
 "Control.Computations.CompEngine.Impl"'s @ParState@\/@prepEvalLeaf@); a
 source group's bundled call
 (@prepSrcLeaf@\/@Prep@\/@SrcGroup@\/@runGroupOnce@) always runs lazily,
 triggered by whichever member leaf 'enginePhase's left-to-right walk
 reaches first, whether that instance is 'FlowSerial' or 'FlowConcurrent'
 and whether parallel eval is on or off (see @docs\/benchmark-notes.md@
 Stage 12's "Disposition" for why proactive dispatch was deleted: bundling
 was measured to subsume it on every workload tried).
 "Control.Computations.CompEngine.Tests.TestCompReqCombined" keeps the
 parallel-eval-off (default, unchanged-behaviour) regression guards; this
 module is specifically about parallel eval being on.

 Since every 'CompLeafSrc' leaf resolving to the same instance within one
 batch is bundled into a single
 'Control.Computations.CompEngine.CompSrc.compSrcExecuteBatch' call (see
 that method's haddock), "several requests against the same instance
 overlap" is not something the engine's own batching provides by itself --
 a batch against one instance is one call regardless. It is still
 achievable, but it is the instance's own responsibility, via its
 'compSrcExecuteBatch' override (see 'ConcurrentBatchSrc' below) -- exactly
 like a real batched backend that can issue several sub-requests of one
 round trip concurrently would do it itself, rather than relying on the
 engine to schedule its point lookups on separate threads.

 Genuine overlap *across* different instances -- or genuine mutual
 exclusion of a 'FlowSerial' instance across different comp bodies -- now
 comes entirely from 'prepEvalLeaf's eval-leaf forking: two eval leaves
 forked onto different OS threads, each building its own nested batch
 against a source, run those nested batches for real at the same time (see
 1b and 6 below). A batch of bare source leaves with no eval leaf in it, by
 contrast, can no longer overlap at all -- there is nothing left for
 'prepEvalLeaf' to fork.

 Every test here builds its own 'Control.Computations.CompEngine.CompFlowRegistry.CompFlowRegistry'
 by hand (rather than going through "Control.Computations.CompEngine.Tests.TestHelper"'s
 @initCompEngineTest@) precisely so it can call
 'Control.Computations.CompEngine.CompFlowRegistry.setCompEvalConcurrency'
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
-- 1 & 2: FlowSerial never overlaps, and neither does FlowConcurrent with
-- the engine's own default (sequential) compSrcExecuteBatch -- see the
-- module haddock on why bundling makes both of these true regardless of
-- eval concurrency: with only one registered instance in play, there is
-- only ever one SrcGroup, so it is one compSrcExecuteBatch call, triggered
-- lazily by whichever member leaf enginePhase reaches first.
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
    -- No 'setCompEvalConcurrency' call needed: with only one registered
    -- instance, there is only ever one SrcGroup, so bundling alone (see
    -- the module haddock) already guarantees a single sequential
    -- compSrcExecuteBatch call regardless of whether parallel eval is on
    -- at all.
    registerCompSrc reg src
    runOnce reg (mutexMainCompDef (typedCompSrcIdOf src))
    readTVarIO highWater

-- | All eight requests above land in one batch against one instance, so the
-- engine bundles them into a single 'compSrcExecuteBatch' call regardless
-- of 'FlowConcurrency' or dispatch (see the module haddock) -- 'ScriptedSrc'
-- never overrides that method, so its default (sequential) implementation
-- serves all eight itself, one at a time, whether the instance declares
-- 'FlowSerial' or 'FlowConcurrent'. This is the "no overlap" case for
-- *both* declarations; 'FlowConcurrent's own overlap guarantee moves to
-- 'test_flowConcurrentSourceGenuinelyOverlapsViaOwnBatchExec' below, via an
-- instance that actually implements it.
test_flowSerialSourceNeverOverlaps :: IO ()
test_flowSerialSourceNeverOverlaps =
  do
    hw <- runMutualExclusionCase FlowSerial
    assertEqual 1 hw

test_flowConcurrentSourceWithDefaultBatchExecNeverOverlaps :: IO ()
test_flowConcurrentSourceWithDefaultBatchExecNeverOverlaps =
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
-- non-threaded RTS -- this repo's own test stanza always builds
-- @-threaded -with-rtsopts=-N@, so the guard holds here. No
-- 'setCompEvalConcurrency' call needed: the overlap this test checks for
-- comes entirely from 'ConcurrentBatchSrc's own 'compSrcExecuteBatch'
-- override running its 8 sub-requests via 'Async.mapConcurrently_' inside
-- one 'runGroupOnce' call -- unaffected by whether that one call is
-- triggered lazily by 'enginePhase' or (formerly) proactively dispatched
-- (see the module haddock).
test_flowConcurrentSourceGenuinelyOverlapsViaOwnBatchExec :: IO ()
test_flowConcurrentSourceGenuinelyOverlapsViaOwnBatchExec =
  when rtsSupportsBoundThreads $
    do
      inFlight <- newTVarIO 0
      highWater <- newTVarIO 0
      let src = ConcurrentBatchSrc "mutex-concurrent-src" (mkOverlapAction inFlight highWater)
      reg <- newCompFlowRegistry
      registerCompSrc reg src
      runOnce reg (mutexConcurrentMainCompDef (typedCompSrcIdOf src))
      hw <- readTVarIO highWater
      assertBool (hw > 1)

----------------------------------------------------------------------------
-- 1b: the FlowSerial guarantee across two DIFFERENT comps' bodies, each
-- forming its own nested batch against the same instance, when both bodies
-- are themselves forked onto separate threads by eval concurrency (see
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
-- test 6's own comment identifies as needing parallel eval enabled to
-- escape the two-leaf fast path (see doSuspended) and reach prepEvalLeaf at
-- all; the 'setCompEvalConcurrency' call in 'runCrossBodyOverlapCase' below
-- takes care of that here.
crossBodyMainCompDef :: Comp () Int -> Comp () Int -> CompDef () (Int, Int)
crossBodyMainCompDef compA compB =
  defineComp "cross-body-main" inMemoryShowCaching $ \() ->
    (,) <$> evalCompOrFail compA () <*> evalCompOrFail compB ()

-- | Wired by hand, like test_noCapEvaluatedTwiceAtEvalWidth8 below, rather than
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
    setCompEvalConcurrency reg (mkCompEvalConcurrency 8)
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
-- test_flowConcurrentSourceGenuinelyOverlapsViaOwnBatchExec above: without real
-- OS-thread concurrency, no eval leaf ever forks, so this would trivially
-- pass (both bodies would run sequentially on the same thread) whether or
-- not the fix under test exists.
test_flowSerialSourceNeverOverlapsAcrossForkedBodiesAtEvalWidth8 :: IO ()
test_flowSerialSourceNeverOverlapsAcrossForkedBodiesAtEvalWidth8 =
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
    setCompEvalConcurrency reg (mkCompEvalConcurrency 8)
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
test_flowSerialSinkNeverOverlapsAcrossForkedBodiesAtEvalWidth8 :: IO ()
test_flowSerialSinkNeverOverlapsAcrossForkedBodiesAtEvalWidth8 =
  when rtsSupportsBoundThreads $
    do
      hw <- runCrossBodySinkOverlapCase
      assertEqual 1 hw

----------------------------------------------------------------------------
-- 3: no cap gets evaluated twice at eval width 8, with 1000 eval leaves
-- genuinely contending for 7 permits. The batch's 32 source reads all
-- resolve to the *same* registered instance, so bundling collapses them
-- into a single SrcGroup regardless of eval concurrency (see the module
-- haddock) -- they no longer draw on the permit pool at all (there is no
-- source dispatch left to draw from it), so the permit contention this
-- test actually exercises comes entirely from the 1000 eval leaves
-- competing among themselves.
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

test_noCapEvaluatedTwiceAtEvalWidth8 :: IO ()
test_noCapEvaluatedTwiceAtEvalWidth8 =
  do
    countsRef <- newIORef HashMap.empty
    let src = ScriptedSrc "dedup-src" FlowConcurrent (pure 0)
    reg <- newCompFlowRegistry
    setCompEvalConcurrency reg (mkCompEvalConcurrency 8)
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
-- 4: when a batch combines several source leaves against different
-- instances and more than one of them throws, the exception that escapes
-- is the LEFTMOST failing leaf's -- mirroring compMAp's left-error bias at
-- the Fail level (Types.hs) at the exception level too. This no longer
-- needs genuine concurrent completion to be meaningful the way it did when
-- 'dispatchSrcJobs' existed: a bare source leaf never forks on its own
-- (see 'prepSrcLeaf'\'s own comment on @readMySlot@), so with no dispatch
-- left, three source leaves in one batch are always triggered strictly
-- sequentially, in traversal order, by enginePhase's own left-to-right
-- walk -- @leftId@'s group throws and its slot is read before
-- @harmlessId@'s or @rightId@'s groups are ever triggered at all. The
-- assertion is unchanged and still worth guarding: it is exactly what
-- @readMySlot@'s own comment argues must hold (the slot is always filled,
-- synchronously, in order, before it is read) -- this test just no longer
-- needs eval concurrency on to prove it (left on anyway, to keep exercising
-- the same general-path/ce_par-on configuration the rest of this module
-- uses). Genuinely exercising left-bias under concurrent completion again
-- would mean wrapping each read in its own eval-leaf-forked body, the way 6
-- below does -- but that tests prepEvalLeaf's join ordering, not
-- prepSrcLeaf's, and is already covered by 6 and by
-- "Control.Computations.CompEngine.Tests.TestCompEvalOrderingContract".
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

test_leftmostFailingSourceLeafExceptionEscapesAtEvalWidth8 :: IO ()
test_leftmostFailingSourceLeafExceptionEscapesAtEvalWidth8 =
  do
    let boomLeft = ScriptedSrc "boom-left" FlowConcurrent (throwIO (userError "left boom"))
        boomHarmless = ScriptedSrc "boom-harmless" FlowConcurrent (pure 0)
        boomRight = ScriptedSrc "boom-right" FlowConcurrent (throwIO (userError "right boom"))
    reg <- newCompFlowRegistry
    setCompEvalConcurrency reg (mkCompEvalConcurrency 8)
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
-- the engine that owned it is gone. Needs 'blocked's own read to actually
-- be running on a forked thread, not the calling thread -- otherwise
-- cancelling the calling thread would trivially interrupt 'blocked' itself
-- via GHC's ordinary async-exception delivery, without ever exercising
-- 'forkTracked'\/'cancelAllTracked' at all. So, like 6 below, 'blocked' (and
-- its fellow fillers) are each wrapped in their own comp body and forked
-- via 'prepEvalLeaf', with a cheap warmup leaf ahead of them: 'prepEvalLeaf'
-- always keeps the batch's first-collected eval leaf inline on the calling
-- thread, however many permits are free (see 'ParState'\'s haddock on
-- 'ps_permits') -- without the warmup, 'blocked' itself would be that first
-- leaf and would never be forked at all.
----------------------------------------------------------------------------

cancelWarmupCompDef :: CompDef () Int
cancelWarmupCompDef = defineComp "cancel-warmup" inMemoryShowCaching $ \() -> pure 0

cancelLeafCompDef :: String -> TypedCompSrcId ScriptedSrc -> CompDef () Int
cancelLeafCompDef name srcId =
  defineComp name inMemoryShowCaching $ \() -> compSrcReq srcId ScriptedReq

cancelMainCompDef
  :: Comp () Int -> Comp () Int -> Comp () Int -> Comp () Int -> CompDef () ()
cancelMainCompDef warmup blockedComp filler1Comp filler2Comp =
  defineComp "cancel-main" inMemoryShowCaching $ \() ->
    void $
      (,,,)
        <$> evalCompOrFail warmup ()
        <*> evalCompOrFail blockedComp ()
        <*> evalCompOrFail filler1Comp ()
        <*> evalCompOrFail filler2Comp ()

-- | Wired by hand, like 'runCrossBodyOverlapCase' above: the leaf comps
-- must be wired before 'cancelMainCompDef' is built, since it closes over
-- them as already-wired 'Comp' values.
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
    -- Eval width 4 -> 3 permits, exactly enough for the three non-warmup
    -- eval leaves (blocked, filler1, filler2) to all fork at once, so
    -- 'blocked' is genuinely off running on its own forked thread (holding
    -- a permit) when the cancel below lands.
    setCompEvalConcurrency reg (mkCompEvalConcurrency 4)
    registerCompSrc reg blocked
    registerCompSrc reg filler1
    registerCompSrc reg filler2
    (rawStateIf, closeSif) <- initStateIf True
    (_compMap, mainComp) <-
      failInM $
        runCompWireM $
          do
            warmup <- wireComp cancelWarmupCompDef
            blockedComp <- wireComp (cancelLeafCompDef "cancel-blocked-leaf" (typedCompSrcIdOf blocked))
            filler1Comp <- wireComp (cancelLeafCompDef "cancel-filler1-leaf" (typedCompSrcIdOf filler1))
            filler2Comp <- wireComp (cancelLeafCompDef "cancel-filler2-leaf" (typedCompSrcIdOf filler2))
            wireComp (cancelMainCompDef warmup blockedComp filler1Comp filler2Comp)
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
    engineAsync <- Async.async (runCompEngine ifs caps rifs () `finally` closeSif)
    -- Give the fork time to actually reach the blocking takeMVar before
    -- cancelling -- not a correctness requirement (cancel is safe at any
    -- point), just what makes this test actually exercise "cancel while a
    -- worker is blocked" rather than "cancel before it even forks".
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
-- 6: two DIFFERENT FlowConcurrent instances, each read by a different comp
-- body, must still let their two reads genuinely overlap once parallel
-- eval is on. This used to need no eval leaf at all: the plain two-leaf
-- shape `(,) <$> compSrcReq srcA .. <*> compSrcReq srcB ..` -- the
-- overwhelmingly common shape a batch of exactly two suspended actions
-- takes -- escaped doSuspended's fast path once 'ce_par' was fixed to gate
-- it (see Impl.hs's own note at the fast-path condition for the bug that
-- fix corrected: gating the escape on the *source*-side width instead of
-- 'ce_par' meant a plain two-leaf batch could never be dispatched no matter
-- how wide eval concurrency was), and then reached 'dispatchSrcJobs' like
-- any other FlowConcurrent group. With no dispatch left, a bare source leaf
-- never forks on its own -- only 'prepEvalLeaf's eval leaves run on a
-- separate OS thread -- so a batch of two bare source leaves and nothing
-- else can no longer overlap at all, regardless of 'ce_par': there is
-- nothing in it for 'prepEvalLeaf' to fork. Genuine overlap between two
-- different instances is still reachable, the same way 1b reaches mutual
-- exclusion for the *same* instance: wrap each read in its own comp body
-- and let 'prepEvalLeaf' fork those bodies.
----------------------------------------------------------------------------

twoLeafLeafCompDef :: String -> TypedCompSrcId ScriptedSrc -> CompDef () Int
twoLeafLeafCompDef name srcId =
  defineComp name inMemoryShowCaching $ \() -> compSrcReq srcId ScriptedReq

twoLeafMainCompDef :: Comp () Int -> Comp () Int -> CompDef () (Int, Int)
twoLeafMainCompDef compA compB =
  defineComp "two-leaf-main" inMemoryShowCaching $ \() ->
    (,) <$> evalCompOrFail compA () <*> evalCompOrFail compB ()

-- | Two distinct 'FlowConcurrent' instances sharing one @inFlight@\/
-- @highWater@ pair, so 'highWater' after the run is the *global* high-water
-- mark across both -- 2 only if the two leaves' 'compSrcExecute' calls
-- genuinely ran at the same time. 'prepEvalLeaf' always keeps the batch's
-- first-collected eval leaf (@compA@ here) inline on the calling thread --
-- see 'ParState'\'s haddock on 'ps_permits' -- so genuine overlap needs
-- @compB@'s own fork to actually be running, concurrently, while @compA@
-- runs inline: @evalWidth@ of 1 leaves parallel eval off ('ce_par'
-- 'Nothing'), so 'prepEvalLeaf' never forks anything (its wildcarded
-- fallback matches unconditionally -- see its own haddock) and both leaves
-- run one at a time on the calling thread; @evalWidth >= 2@ gives @compB@ a
-- permit to fork with.
runTwoLeafOverlapCase :: Int -> IO Int
runTwoLeafOverlapCase evalWidth =
  do
    inFlight <- newTVarIO 0
    highWater <- newTVarIO 0
    let srcA = ScriptedSrc "two-leaf-a" FlowConcurrent (mkOverlapAction inFlight highWater)
        srcB = ScriptedSrc "two-leaf-b" FlowConcurrent (mkOverlapAction inFlight highWater)
    reg <- newCompFlowRegistry
    setCompEvalConcurrency reg (mkCompEvalConcurrency evalWidth)
    registerCompSrc reg srcA
    registerCompSrc reg srcB
    (rawStateIf, closeSif) <- initStateIf True
    (_compMap, mainComp) <-
      failInM $
        runCompWireM $
          do
            compA <- wireComp (twoLeafLeafCompDef "two-leaf-leaf-a" (typedCompSrcIdOf srcA))
            compB <- wireComp (twoLeafLeafCompDef "two-leaf-leaf-b" (typedCompSrcIdOf srcB))
            wireComp (twoLeafMainCompDef compA compB)
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
-- test_flowSerialSourceNeverOverlapsAcrossForkedBodiesAtEvalWidth8 above:
-- without real OS-thread concurrency, no eval leaf ever forks, so both
-- reads would run on the calling thread regardless of 'evalWidth'.
test_twoLeafBatchGenuinelyOverlapsWithParallelEvalOn :: IO ()
test_twoLeafBatchGenuinelyOverlapsWithParallelEvalOn =
  when rtsSupportsBoundThreads $
    do
      hw <- runTwoLeafOverlapCase 4
      assertEqual 2 hw

-- | Same shape, but with parallel eval off (@evalWidth = 1@, 'ce_par'
-- 'Nothing') -- no leaf ever forks, so both reads run one at a time on the
-- calling thread.
test_twoLeafBatchNeverOverlapsWithParallelEvalOff :: IO ()
test_twoLeafBatchNeverOverlapsWithParallelEvalOff =
  do
    hw <- runTwoLeafOverlapCase 1
    assertEqual 1 hw
