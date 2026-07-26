{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Control.Computations.Demos.Tests (testMain) where

import Control.Computations.Utils.Logging
import System.Environment (withArgs)
import Test.Framework

-- Generate with
-- egrep -R -l '^(test|prop)_' app | sed 's|src/||g; s|/|.|g; s|.hs$||g' | sort -u | gawk '{ printf "import {-@ HTF_TESTS @-} %s\n", $0 }'
import {-@ HTF_TESTS @-} Control.Computations.Demos.DirSync.Tests
import {-@ HTF_TESTS @-} Control.Computations.Demos.FlowImpls.SqliteSrc
import {-@ HTF_TESTS @-} Control.Computations.Demos.Hospital.Tests
import {-@ HTF_TESTS @-} Control.Computations.Demos.Utils.DispatcherTest
import {-@ HTF_TESTS @-} Control.Computations.Demos.Utils.FileStore.Tests

testMain :: IO ()
testMain = do
  setupLogging WARN
  -- `htfMain` reads process args via `getArgs` to build its own filter/pattern
  -- options. Those are the same process args optparse-applicative already
  -- consumed to route us to the "test" subcommand (e.g. the "test" token
  -- itself), so without clearing them here HTF treats leftover tokens like
  -- "test" as a substring filter -- which happens to match module names
  -- ending in "...Tests" but silently filters out tests defined in modules
  -- without "Test" in their name (e.g. SqliteSrc).
  withArgs [] (htfMain htf_importedTests)
