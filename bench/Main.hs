module Main (main) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Demos.Bench.Hospital (hospitalBenchMain)
import Control.Computations.Demos.Bench.Main (benchMain)
import Control.Computations.Demos.Bench.Tiered (tieredBenchMain)

----------------------------------------
-- EXTERNAL
----------------------------------------
import System.Environment (lookupEnv)

{- | Dispatches between the three benchmarks this executable can run, by
 @HOSPITAL_BENCH@\/@TIERED_BENCH@ (mirroring @PERSIST_BENCH_SCALE@'s
 env-var-configuration style, per this repo's dev workflow -- see the
 README's Development section): unset or @"0"@ for both runs the original
 scale benchmark, unchanged and still the default (the no-regression guard
 every number in @docs\/benchmark-notes.md@ is measured against); a nonzero
 @HOSPITAL_BENCH@ runs 'hospitalBenchMain'; a nonzero @TIERED_BENCH@ runs
 'tieredBenchMain'. @HOSPITAL_BENCH@ is checked first, matching its
 pre-existing precedence -- setting both is not a supported combination and
 picks Hospital. Never more than one in one process -- each benchmark
 constructs its own
 'Control.Computations.CompEngine.CompFlowRegistry.CompFlowRegistry' and
 drives its own engine from scratch, so there is nothing to gain from
 trying to run more than one in sequence here rather than via separate
 invocations.
-}
main :: IO ()
main = do
  hospitalBench <- lookupEnv "HOSPITAL_BENCH"
  tieredBench <- lookupEnv "TIERED_BENCH"
  case hospitalBench of
    Just v | v `notElem` ["", "0"] -> hospitalBenchMain
    _ -> case tieredBench of
      Just v | v `notElem` ["", "0"] -> tieredBenchMain
      _ -> benchMain
