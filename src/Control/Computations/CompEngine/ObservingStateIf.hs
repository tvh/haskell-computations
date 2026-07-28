{-# LANGUAGE RankNTypes #-}

{- | A single generic 'CompEngineStateIf' wrapper that observes every genuine
 computation-body evaluation.

 Factored out of what used to be a benchmark-only @countingStateIf@ in
 "Control.Computations.Demos.Bench.Main": the @bench:@ and @test:@ cabal
 stanzas don't share a source directory (@test:@ compiles @src@ directly
 alongside @test@; @bench@ only has its own directory -- see the
 @benchmarks:@ stanza in @package.yaml@), so a helper both need has to live
 somewhere both can reach. This module is other-modules of the library (not
 listed in @exposed-modules@ in @package.yaml@ -- it has no business being
 part of the public API), reachable by @test:@ because it recompiles @src@
 directly, and by @bench:@ because its stanza does the same.

 Not re-exported from the public "Control.Computations.CompEngine" facade,
 for the same reason: this exists purely to instrument the engine's own
 state-if for tests and benchmarks, not as something a library consumer
 wires into a real engine.
-}
module Control.Computations.CompEngine.ObservingStateIf (
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
    , trackOutput = trackOutput orig
    , getCompSinkOuts = getCompSinkOuts orig
    , getQueue = getQueue orig
    }
