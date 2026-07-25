{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Control.Computations.Utils.Logging
import Test.Framework

-- Generate with
-- egrep -R -l '^(test|prop)_' src | sed 's|src/||g; s|/|.|g; s|.hs$||g' | sort -u | gawk '{ printf "import {-@ HTF_TESTS @-} %s\n", $0 }'
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestCompEngine
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestDriver
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestOutputs
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestRevive
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestRun
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Tests.TestStateIf
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Utils.DefTableTest
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Utils.OutputsMap
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Utils.PriorityAgingQueue
import {-@ HTF_TESTS @-} Control.Computations.CompEngine.Utils.SrcIndex
import {-@ HTF_TESTS @-} Control.Computations.FlowImpls.FileSinkTest
import {-@ HTF_TESTS @-} Control.Computations.FlowImpls.FileSrcTest
import {-@ HTF_TESTS @-} Control.Computations.FlowImpls.TimeSrcTest
import {-@ HTF_TESTS @-} Control.Computations.Utils.DataSizeTest
import {-@ HTF_TESTS @-} Control.Computations.Utils.DispatcherTest
import {-@ HTF_TESTS @-} Control.Computations.Utils.FailTest
import {-@ HTF_TESTS @-} Control.Computations.Utils.FileStore.Tests
import {-@ HTF_TESTS @-} Control.Computations.Utils.FileWatchTest
import {-@ HTF_TESTS @-} Control.Computations.Utils.IOUtilsTest
import {-@ HTF_TESTS @-} Control.Computations.Utils.MultiMapTest
import {-@ HTF_TESTS @-} Control.Computations.Utils.MultiSetTest
import {-@ HTF_TESTS @-} Control.Computations.Utils.TimeSpanTest

main :: IO ()
main = do
  setupLogging WARN
  htfMain htf_importedTests
