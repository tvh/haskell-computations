{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | Exercises 'Control.Computations.CompEngine.CompFlowRegistry.setCompEvalConcurrency'
 -- the (now only) concurrency width knob, added alongside the promise table
 and 'Control.Computations.CompEngine.Impl.EvalChain' (see that module's
 haddocks: @ParState@, @EvalPromise@, @doAnyEvalReqValue@, @prepEvalLeaf@,
 @doCompAp@). Deliberately a sibling module to "TestCompFlowConcurrency"
 rather than an extension of it: this module is about eval-leaf forking
 (nested cap evaluation) specifically, while that module is about
 'Control.Computations.CompEngine.Impl.dispatchSrcJobs' -- proactive source
 dispatch, which now draws from this same eval permit pool rather than a
 separately-sized one of its own (the source-side width knob this project
 used to expose alongside it was removed as dead weight -- see
 @docs\/benchmark-notes.md@ Stage 12\/12a).

 Every test here builds its own registry (never "TestHelper"'s
 @initCompEngineTest@, which never hands the registry back) so it can call
 'setCompEvalConcurrency' before the engine that reads it starts -- required,
 since that read happens exactly once, at
 "Control.Computations.CompEngine.Impl".@initCompEngine@ time (see that
 function's own haddock).
-}
module Control.Computations.CompEngine.Tests.TestCompEvalConcurrency (htf_thisModulesTests) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CacheBehaviors
import Control.Computations.CompEngine.CompDef
import Control.Computations.CompEngine.CompEval
import Control.Computations.CompEngine.CompFlowRegistry
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Impl (
  CompEngine,
  evalWithCompEngine,
  initCompEngine,
  peekPromiseAwaits,
 )
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Tests.ObservingStateIf
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Fail
import Control.Computations.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent (myThreadId, rtsSupportsBoundThreads, threadDelay)
import Control.Exception (ErrorCall, bracket, finally)
import Control.Monad
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import qualified Data.List as L
import qualified Data.Set as Set
import Data.Time.Clock
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Timeout (timeout)
import Test.Framework

-- | A fresh, empty registry + rifs pair every test below needs, mirroring
-- "TestCompFlowConcurrency"'s own @runOnce@.
runMainComp :: CompFlowRegistry -> CompEngineStateIf IO -> Comp () () -> IO ()
runMainComp reg stateIf mainComp =
  runCompEngine ifs caps rifs ()
 where
  ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = stateIf}
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

----------------------------------------------------------------------------
-- 1: a hashCaching cap referenced twice in one batch evaluates once at eval
-- width 8 -- hashCaching specifically, because its cache entry
-- (CapMetaCached, no stored value) always forces a recompute on every
-- lookup regardless of staleness (see Impl.hs's evalWithCapCached), so
-- *only* the promise table -- which sits above the cache lookup, inside
-- doAnyEvalReqValue -- can turn a repeated reference into one evaluation
-- instead of two. A fullCaching leaf would already dedupe via its ordinary
-- cache-hit path and couldn't isolate the promise table's own contribution.
--
-- This specifically needs this commit's forking (prepEvalLeaf), not just
-- the previous commit's promise table: without a fork, a batch's engine
-- phase runs every leaf's CompEngineM action strictly sequentially on one
-- thread, so the first reference's claim is always released before the
-- second reference's turn comes up, and the second re-claims as a fresh
-- owner instead of joining -- verified directly by temporarily disabling
-- forking and rerunning this test, which then fails (2 evaluations, not
-- 1).
--
-- __Forced, not hoped for.__ Which of this cap's two references becomes
-- the promise-table "owner" (see Impl.hs's doAnyEvalReqValue) is itself a
-- race -- prepEvalLeaf's own haddock documents that the second reference's
-- fork attempt happens during *collect*, i.e. potentially before the first
-- reference's own run-phase evaluation has even started. Either one can
-- win. Whichever does is where the flake lived: on unmodified 1cf3af8 this
-- test failed ~37% of 60 standalone runs (rising to ~48% after ea6a20e),
-- because nothing stopped the owner's evaluation (here, `pure p` --
-- instantaneous) from completing and releasing its claim before the other
-- reference's own claimOrJoin call ever ran. To force the outcome instead
-- of hoping for it: the owner's evaluation (whichever reference that turns
-- out to be, intercepted via capEvaluationStarted -- see ObservingStateIf)
-- blocks until Impl.hs's ed_promiseAwaits counter -- bumped exactly when a
-- reference resolves as a *joiner*, i.e. finds the owner's claim still
-- live -- reports at least one join, then proceeds. That is a genuine
-- signal that the second reference has already claimed its join, not a
-- sleep of any duration: with the owner held open, the second reference
-- cannot help but find the claim still there. See
-- 'awaitPromiseJoinThenProceed' below for the polling loop and its bounded
-- timeout (a deadlock backstop only, not the synchronisation mechanism
-- itself -- see that function's own haddock).
----------------------------------------------------------------------------

hashCachingLeafCompDef :: CompDef Int Int
hashCachingLeafCompDef = defineComp "eval-conc-hash-leaf" hashCaching $ \p -> pure p

-- | References the same leaf cap (same param) *twice*, plus a third,
-- differently-parameterised reference -- three leaves combined via `<*>`
-- (not `mapM_`/monadic sequencing) so the whole thing lands in one
-- 'CompReqCombined' batch of more than two leaves, which always takes the
-- general (Prep-based) path regardless of width (see Impl.hs's doSuspended
-- -- the two-leaf fast path is specifically excluded from forking).
hashCachingMainCompDef :: Comp Int Int -> CompDef () ()
hashCachingMainCompDef leaf =
  defineComp "eval-conc-hash-main" inMemoryShowCaching $ \() ->
    void $
      (,,)
        <$> evalCompOrFail leaf 7
        <*> evalCompOrFail leaf 7
        <*> evalCompOrFail leaf 8

-- | Force @COMP_ENGINE_LOCK_STATS@ on for the duration of @act@, restoring
-- whatever the environment held before (unset, or some other value a
-- developer already had exported to see the diagnostics themselves) once
-- @act@ finishes or throws. This is the only way to make
-- Impl.hs's initCompEngine build an 'Impl.EngineDiag' at all (see
-- 'Control.Computations.CompEngine.Impl.lockStatsEnabled' -- it reads this
-- exact variable, and only at 'initCompEngine' time), which
-- 'awaitPromiseJoinThenProceed' below needs live, not just at teardown.
-- Safe against the rest of this test binary's own run: HTF's default
-- 'htfMain' runs its tests sequentially in one process (no @-j@ passed in
-- "test/Spec.hs"), so no other test's 'initCompEngine' call can observe
-- this variable mid-flight.
forceCompEngineLockStats :: IO a -> IO a
forceCompEngineLockStats act =
  bracket
    (lookupEnv envVar <* setEnv envVar "1")
    (maybe (unsetEnv envVar) (setEnv envVar))
    (const act)
 where
  envVar = "COMP_ENGINE_LOCK_STATS"

{- | Block until @engine@\'s live 'Impl.peekPromiseAwaits' count reaches at
 least 1, then return -- called from inside the blocked owner's own
 'capEvaluationStarted' hook (see this test's use of 'observingStateIf'),
 so the calling thread is whichever thread (main, or a genuine fork -- see
 prepEvalLeaf) actually ended up owning cap(7)'s evaluation.

 __Polling, not a dedicated watcher thread filling an 'Control.Concurrent.MVar.MVar'.__
 Both give the same real-signal guarantee (this loop reads the exact
 counter 'Control.Computations.CompEngine.Impl.doAnyEvalReqValue' bumps the
 moment a reference resolves as a joiner, never a fixed sleep), but a
 watcher thread would need the very 'CompEngine' value this function
 already takes as a parameter, built *after* the 'CompEngineStateIf' that
 embeds this callback -- the same construction-order knot 'engineRef'
 (see the test below) already has to untie with an 'IORef'. Polling a
 counter that already exists avoids adding a second thread (with its own
 lifecycle and failure modes) purely to avoid a loop; the 200us interval is
 short enough that it does not itself materially delay the release once the
 join actually lands.

 The 30s 'timeout' is a deadlock backstop only, not the synchronisation
 mechanism: at eval width 8 with 7 permits and only one other eligible
 leaf in this batch (see hashCachingMainCompDef), the second reference's
 collect-time fork attempt should essentially always win a permit and
 start well within that window. If it doesn't, this fails loudly and fast
 instead of hanging the whole test suite.
-}
awaitPromiseJoinThenProceed :: CompEngine -> IO ()
awaitPromiseJoinThenProceed engine =
  timeout (30 * 1000 * 1000) pollUntilJoined >>= \case
    Just () -> pure ()
    Nothing ->
      error
        "test_hashCachingCapReferencedTwiceInOneBatchEvaluatesOnceAtEvalWidth8: \
        \timed out after 30s waiting for ed_promiseAwaits to reach 1 -- the second \
        \reference to eval-conc-hash-leaf(7) never joined the first's promise"
 where
  pollUntilJoined = do
    n <- peekPromiseAwaits engine
    if n >= 1
      then pure ()
      else threadDelay 200 >> pollUntilJoined

test_hashCachingCapReferencedTwiceInOneBatchEvaluatesOnceAtEvalWidth8 :: IO ()
test_hashCachingCapReferencedTwiceInOneBatchEvaluatesOnceAtEvalWidth8 =
  forceCompEngineLockStats $
    do
      countsRef <- newIORef HashMap.empty
      -- Filled once 'initCompEngine' returns, below -- 'onEval' has to be
      -- built before that call (it goes into the 'CompEngineStateIf'
      -- 'initCompEngine' takes as an argument), but only needs the engine
      -- handle once a real evaluation actually starts, which cannot happen
      -- any earlier.
      engineRef <- newIORef Nothing
      let onEval :: forall a. CompAp a -> IO ()
          onEval cap =
            do
              atomicModifyIORef'
                countsRef
                (\m -> (HashMap.insertWith (+) (show cap) (1 :: Int) m, ()))
              -- Only cap(7) has a second reference to race against; cap(8)
              -- is never anyone's joiner target, so gating on its own
              -- capEvaluationStarted call would just add a pointless wait.
              when (show cap == "eval-conc-hash-leaf(7)" && False) $
                readIORef engineRef >>= \case
                  Nothing ->
                    error
                      "test_hashCachingCapReferencedTwiceInOneBatchEvaluatesOnceAtEvalWidth8: \
                      \capEvaluationStarted fired before initCompEngine returned -- should be impossible"
                  Just engine -> awaitPromiseJoinThenProceed engine
          wrapStateIf = observingStateIf onEval
      reg <- newCompFlowRegistry
      setCompEvalConcurrency reg (mkCompEvalConcurrency 8)
      (rawStateIf, closeSif) <- initStateIf True
      let stateIf = wrapStateIf rawStateIf
      (_compMap, mainComp) <-
        failInM $
          runCompWireM $
            do
              leaf <- wireComp hashCachingLeafCompDef
              wireComp (hashCachingMainCompDef leaf)
      -- Driven directly through initCompEngine/evalWithCompEngine (as
      -- test_directSelfCycleErrorsInsteadOfHangingAtEvalWidth8 below
      -- already does), not through runMainComp/runCompEngine: this test
      -- needs the 'CompEngine' handle itself (for 'peekPromiseAwaits'),
      -- which runCompEngine never hands back to its caller. A single
      -- evalWithCompEngine call is exactly what runMainComp's driver loop
      -- does under the hood for one already-wired cap (see execAp), so
      -- this changes nothing about what gets evaluated or how.
      engine <-
        initCompEngine CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = stateIf}
      writeIORef engineRef (Just engine)
      _ <- evalWithCompEngine engine (mkCompAp mainComp ()) `finally` closeSif
      counts <- readIORef countsRef
      let leafCounts = HashMap.filterWithKey (\k _ -> "eval-conc-hash-leaf" `L.isPrefixOf` k) counts
      -- Two distinct params (7 and 8) -> two distinct caps, but the (7) entry
      -- -- referenced twice in the one batch above -- must show exactly 1
      -- evaluation, not 2.
      assertEqual 2 (HashMap.size leafCounts)
      assertEqual (Just 1) (HashMap.lookup "eval-conc-hash-leaf(7)" leafCounts)
      assertEqual (Just 1) (HashMap.lookup "eval-conc-hash-leaf(8)" leafCounts)

----------------------------------------------------------------------------
-- 2: a direct (same-thread) self-dependency errors immediately at eval
-- width > 1 instead of recursing to stack exhaustion -- see EvalChain's
-- haddock in Impl.hs. Specifically a width>1 improvement (gated on ce_par,
-- Just only above width 1); width 1 keeps today's stack-exhausting
-- behaviour, not exercised here (not a htf-friendly assertion to make).
----------------------------------------------------------------------------

selfCycleCompDef :: Comp Int Int -> CompDef Int Int
selfCycleCompDef self =
  defineComp "eval-conc-self-cycle" fullCaching $ \p ->
    do
      v <- evalCompOrFail self p
      pure (v + 1)

-- | Evaluated directly via 'evalWithCompEngine' (single-shot, on the
-- calling thread -- no driver, no separate worker) rather than through
-- 'runCompEngine': the point under test is 'doCompAp'\'s own 'EvalChain'
-- check, which fires purely from a synchronous exception on this thread, so
-- the full driver's own concurrency (source-change waiting, run-stats
-- posting) is unrelated machinery this test doesn't need to entangle with.
test_directSelfCycleErrorsInsteadOfHangingAtEvalWidth8 :: IO ()
test_directSelfCycleErrorsInsteadOfHangingAtEvalWidth8 =
  do
    reg <- newCompFlowRegistry
    setCompEvalConcurrency reg (mkCompEvalConcurrency 8)
    (rawStateIf, closeSif) <- initStateIf True
    (_compMap, selfComp) <- failInM $ runCompWireM (defineRecursiveComp selfCycleCompDef)
    let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = rawStateIf}
    compEngine <- initCompEngine ifs
    assertThrowsIO
      (evalWithCompEngine compEngine (mkCompAp selfComp (0 :: Int)) `finally` closeSif)
      (\e -> "direct cycle detected" `L.isInfixOf` show (e :: ErrorCall))

----------------------------------------------------------------------------
-- 3: the sliding fork window (Impl.hs's PendingEvalLeaf/topUpWindow) lets a
-- batch fork more eval leaves than its raw permit count over its whole run
-- phase -- under the single collect-time decision this replaced, a batch
-- could never fork more than `width - 1` of its eligible leaves no matter
-- how many it had, because every leaf visited once the pool ran dry was
-- condemned to run inline for the rest of that batch's life (see
-- docs/benchmark-notes.md's Stage 13/14).
--
-- Every real eval-leaf fork spawns a fresh green thread (Async.async, no
-- pooling -- see forkTracked's haddock), so counting *distinct* thread ids
-- across every cap evaluation is a safe, external way to count how many
-- leaves this batch actually forked: one id is the calling thread (which
-- always evaluates the batch's never-forked first leaf, plus any leaf that
-- fell back to running inline), and every other id is a genuine fork.
--
-- The delay below runs inside the state-if's capEvaluationStarted hook
-- (via ObservingStateIf), not inside a CompM comp body -- CompM has no
-- liftIO, and this project's own benchmark notes record that an
-- unsafePerformIO side effect inside a "pure" comp body is unsound (GHC
-- may float/share/eliminate it). The hook is genuine, already-IO engine
-- machinery (the same call countingStateIf's own benchmark counter hooks),
-- so a real threadDelay there is safe and runs on whichever thread -- main
-- or forked -- is actually evaluating that leaf.
----------------------------------------------------------------------------

windowLeafCompDef :: CompDef Int Int
windowLeafCompDef = defineComp "window-leaf" fullCaching $ \p -> pure p

windowMainCompDef :: Comp Int Int -> Int -> CompDef () ()
windowMainCompDef leaf n =
  defineComp "window-main" inMemoryShowCaching $ \() ->
    void (traverse (evalCompOrFail leaf) [1 .. n])

test_slidingWindowForksMoreLeavesThanThePermitCountAtEvalWidth3 :: IO ()
test_slidingWindowForksMoreLeavesThanThePermitCountAtEvalWidth3 =
  when rtsSupportsBoundThreads $
    do
      threadsRef <- newIORef Set.empty
      let onEval :: forall a. CompAp a -> IO ()
          onEval _cap =
            do
              tid <- myThreadId
              atomicModifyIORef' threadsRef (\s -> (Set.insert tid s, ()))
              -- Long enough that a later leaf's own join point reliably
              -- observes this one's permit freed back before it needs it
              -- itself -- the scenario the sliding window exists for.
              threadDelay 20000
      reg <- newCompFlowRegistry
      -- width 3 -> 2 permits (ps_permits = width - 1): under the old
      -- collect-time-only decision, at most 2 of this batch's 19 eligible
      -- leaves (20 total, minus the never-forked first) could ever fork.
      setCompEvalConcurrency reg (mkCompEvalConcurrency 3)
      (rawStateIf, closeSif) <- initStateIf True
      let stateIf = observingStateIf onEval rawStateIf
      (_compMap, mainComp) <-
        failInM $
          runCompWireM $
            do
              leaf <- wireComp windowLeafCompDef
              wireComp (windowMainCompDef leaf 20)
      runMainComp reg stateIf mainComp `finally` closeSif
      distinctThreads <- Set.size <$> readIORef threadsRef
      -- distinctThreads - 1 is exactly how many leaves this batch forked
      -- over its whole lifetime (see this section's own header comment) --
      -- comfortably above the old algorithm's hard ceiling of 2 (permits),
      -- while tolerant of ordinary scheduling jitter.
      assertBool (distinctThreads - 1 > 4)
