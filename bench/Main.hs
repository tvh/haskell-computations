module Main (main) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Demos.Bench.Hospital (hospitalBenchMain)
import Control.Computations.Demos.Bench.Main (benchMain)

----------------------------------------
-- EXTERNAL
----------------------------------------
import System.Environment (lookupEnv)

{- | Dispatches between the two benchmarks this executable can run, by
 @HOSPITAL_BENCH@ (mirroring @PERSIST_BENCH_SCALE@'s env-var-configuration
 style, per this repo's dev workflow -- see the README's Development
 section): unset or @"0"@ runs the original scale benchmark, unchanged and
 still the default (the no-regression guard every number in
 @docs\/benchmark-notes.md@ is measured against); any other value runs
 'hospitalBenchMain' instead. Never both in one process -- each benchmark
 constructs its own
 'Control.Computations.CompEngine.CompFlowRegistry.CompFlowRegistry' and
 drives its own engine from scratch, so there is nothing to gain from
 trying to run both in sequence here rather than via two separate
 invocations.
-}
main :: IO ()
main = do
  hospitalBench <- lookupEnv "HOSPITAL_BENCH"
  case hospitalBench of
    Just v | v `notElem` ["", "0"] -> hospitalBenchMain
    _ -> benchMain
