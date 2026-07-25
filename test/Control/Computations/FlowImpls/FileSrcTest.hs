{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.FlowImpls.FileSrcTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompSrc
import Control.Computations.FlowImpls.FileSrc
import Control.Computations.Utils.Clock
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.TimeUtils
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashSet as HashSet
import Data.Time.Clock.POSIX
import System.FilePath
import Test.Framework

testConfig :: FileSrcConfig
testConfig =
  FileSrcConfig
    { fcsc_ident = "TestFileSrc"
    , fcsc_clock = realClock
    , fcsc_pollInterval = milliseconds 10
    , fcsc_rootDir = None
    }

test_getNotificationForChangedModTime :: IO ()
test_getNotificationForChangedModTime =
  withSysTempDir $ \rootDir' ->
    withFileSrc testConfig $ \fcsf -> do
      rootDir <- canonPath rootDir'
      testPath <- canonPath (unCanonPath rootDir </> "testfile")
      (deps1, result1) <- compSrcExecute fcsf (ReadFile (unCanonPath testPath))
      assertEqual [Dep (FileKey testPath) None] (HashSet.toList deps1)
      assertBool (isFail result1)

      BS.writeFile (unCanonPath testPath) "123"
      let nextChanges = do
            logDebug "Waiting for changes..."
            changes <- atomically (compSrcWaitChanges fcsf)
            logDebug ("Got changes: " ++ show changes)
            if HashSet.null changes
              then nextChanges
              else pure changes
      changes <- nextChanges
      [Dep (FileKey p1) (Some (FileVer m1))] <- pure (HashSet.toList changes)
      assertEqual testPath p1

      (deps2, result2) <- compSrcExecute fcsf (ReadFile (unCanonPath testPath))
      [Dep (FileKey p2) (Some (FileVer m2))] <- pure (HashSet.toList deps2)
      assertEqual testPath p2
      assertEqual m1 m2
      assertEqual (Ok (BSC.pack "123")) result2

      (deps3, Ok result3) <- compSrcExecute fcsf (ListDir (unCanonPath rootDir))
      [Dep (FileKey p3) (Some (FileVer m3))] <- pure (HashSet.toList deps3)
      assertEqual rootDir p3
      [actual3] <- pure (HashSet.toList result3)
      assertEqual "testfile" (de_name actual3)
      assertEqual RegularFileType (de_type actual3)

      testPath2 <- canonPath (unCanonPath rootDir </> "testfile2")
      BS.writeFile (unCanonPath testPath2) "xyz"
      (deps4, Ok result4) <- compSrcExecute fcsf (ListDir (unCanonPath rootDir))
      [Dep (FileKey p4) (Some (FileVer m4))] <- pure (HashSet.toList deps4)
      subAssert $ assertBefore m3 m4
      assertEqual rootDir p4
      assertEqual
        (HashSet.fromList ["testfile", "testfile2"])
        (HashSet.map de_name result4)

      BS.writeFile (unCanonPath testPath2) "abc"
      (deps5, result5) <- compSrcExecute fcsf (ReadFile (unCanonPath testPath2))
      [Dep (FileKey p5) (Some (FileVer m5))] <- pure (HashSet.toList deps5)
      assertEqual testPath2 p5
      subAssert $ assertBefore m2 m5
      assertEqual (Ok (BSC.pack "abc")) result5
 where
  assertBefore t1 t2 = do
    assertBoolVerbose
      ( "Timestamp "
          ++ formatUTCTimeHiRes' (posixSecondsToUTCTime t1)
          ++ " is not before timestamp "
          ++ formatUTCTimeHiRes' (posixSecondsToUTCTime t2)
      )
      (t1 < t2)
