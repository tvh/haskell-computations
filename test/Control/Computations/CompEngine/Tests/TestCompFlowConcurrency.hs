{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | Exercises the bounded worker pool a wide 'CompReqCombined' batch (see
 "Control.Computations.CompEngine.Types") may dispatch 'FlowConcurrent'
 source leaves to once 'Control.Computations.CompEngine.CompFlowRegistry.setCompFlowConcurrency'
 raises a registry's width above 1 -- see "Control.Computations.CompEngine.Impl"'s
 @prepSrcLeaf@\/@Prep@\/@SrcJobs@ for the implementation these tests pin
 down. "Control.Computations.CompEngine.Tests.TestCompReqCombined" keeps the
 width-1 (default, unchanged-behaviour) regression guards; this module is
 specifically about width > 1.

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
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Tests.ObservingStateIf
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.ConcUtils (timeout)
import Control.Computations.Utils.Fail
import Control.Computations.Utils.TimeSpan

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

test_flowSerialSourceNeverOverlapsAtWidth8 :: IO ()
test_flowSerialSourceNeverOverlapsAtWidth8 =
  do
    hw <- runMutualExclusionCase FlowSerial
    assertEqual 1 hw

-- | Guarded on 'rtsSupportsBoundThreads' so this can't go flaky on a
-- non-threaded RTS, where width > 1 is deliberately a no-op (see
-- prepSrcLeaf's haddock) -- this repo's own test stanza always builds
-- @-threaded -with-rtsopts=-N@, so the guard holds here.
test_flowConcurrentSourceGenuinelyOverlapsAtWidth8 :: IO ()
test_flowConcurrentSourceGenuinelyOverlapsAtWidth8 =
  when rtsSupportsBoundThreads $
    do
      hw <- runMutualExclusionCase FlowConcurrent
      assertBool (hw > 1)

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
