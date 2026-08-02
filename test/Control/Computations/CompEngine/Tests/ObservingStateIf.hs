{-# LANGUAGE RankNTypes #-}

{- | A single generic 'CompEngineStateIf' wrapper that observes every genuine
 computation-body evaluation.

 Test-only: the @tests:@ stanza in @package.yaml@ already compiles @src@
 alongside @test@ (see that stanza's @source-dirs@), so this lives in the
 test tree rather than as unexposed library code -- it's scaffolding for
 these tests, not part of the engine. The benchmark has its own local
 @countingStateIf@ (see
 "Control.Computations.Demos.Bench.Main") rather than importing this: the
 benchmark stanza deliberately links the built library instead of
 recompiling @src@, so it can't reach a test-tree module (or an unexposed
 library one) without giving that up.
-}
module Control.Computations.CompEngine.Tests.ObservingStateIf (
  observingStateIf,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Types

{- | Wrap a 'CompEngineStateIf' so every call to 'capEvaluationStarted' --
 the engine's own per-instance "about to run this computation body" hook,
 called exactly once immediately before each real (non-cache-hit)
 invocation (see "Control.Computations.CompEngine.Impl") -- first runs
 @onEval@, then delegates to the original implementation. All other fields
 delegate unchanged.

 Built as an explicit record rather than a record update
 (@orig { capEvaluationStarted = ... }@): most fields are higher-rank
 (@forall a. IsCompResult a => ...@), and GHC rejects record-update syntax
 against a record with higher-rank fields.
-}
observingStateIf
  :: (forall a. IsCompResult a => CompAp a -> IO ())
  -> CompEngineStateIf IO
  -> CompEngineStateIf IO
observingStateIf onEval orig =
  CompEngineStateIf
    { lookupCapResult = lookupCapResult orig
    , capEvaluationStarted = \cap -> do
        onEval cap
        capEvaluationStarted orig cap
    , capEvaluationFinished = capEvaluationFinished orig
    , dequeueGivenCap = dequeueGivenCap orig
    , dequeueNextCap = dequeueNextCap orig
    , staleQueueSize = staleQueueSize orig
    , enqueueStaleCaps = enqueueStaleCaps orig
    , getCompSinkOuts = getCompSinkOuts orig
    , getQueue = getQueue orig
    }
