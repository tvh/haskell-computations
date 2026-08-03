{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | Exercises 'Control.Computations.CompEngine.CompFlowRegistry.setCompEvalConcurrency'
 -- the eval-side width knob added alongside 'ce_par' and
 'Control.Computations.CompEngine.Impl.EvalChain' (see that module's
 haddocks: @ParState@, @EvalChain@, @doCompAp@). Deliberately a sibling
 module to "TestCompFlowConcurrency" rather than an extension of it: that
 module is entirely about the *source*-side width knob
 ('Control.Computations.CompEngine.CompFlowRegistry.setCompFlowConcurrency'),
 which this project leaves untouched and orthogonal (see
 'Control.Computations.CompEngine.CompFlowRegistry.setCompEvalConcurrency'\'s
 own haddock for why the two knobs are deliberately not shared).

 Builds its own registry (never "TestHelper"'s @initCompEngineTest@, which
 never hands the registry back) so it can call 'setCompEvalConcurrency'
 before the engine that reads it starts -- required, since that read happens
 exactly once, at
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
import Control.Computations.CompEngine.Impl (evalWithCompEngine, initCompEngine)
import Control.Computations.CompEngine.Run
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Fail

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Exception (ErrorCall, finally)
import qualified Data.List as L
import Test.Framework

----------------------------------------------------------------------------
-- 1: a direct (same-thread) self-dependency errors immediately at eval
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
    setCompEvalConcurrency reg (mkCompFlowConcurrency 8)
    (rawStateIf, closeSif) <- initStateIf True
    (_compMap, selfComp) <- failInM $ runCompWireM (defineRecursiveComp selfCycleCompDef)
    let ifs = CompEngineIfs{ce_compFlowRegistry = reg, ce_stateIf = rawStateIf}
    compEngine <- initCompEngine ifs
    assertThrowsIO
      (evalWithCompEngine compEngine (mkCompAp selfComp (0 :: Int)) `finally` closeSif)
      (\e -> "direct cycle detected" `L.isInfixOf` show (e :: ErrorCall))
