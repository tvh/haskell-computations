{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Utils.FileWatchTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Clock
import Control.Computations.Utils.ConcUtils
import Control.Computations.Utils.FileWatch
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.TimeUtils
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad
import qualified Data.HashSet as HashSet
import Data.List (sort)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Directory
import System.FilePath
import Test.Framework

test_basics :: IO ()
test_basics = loop 1 -- run basicTest several times because it's not deterministic
 where
  numRuns = 10
  loop :: Int -> IO ()
  loop n = do
    logNote ("Test run " ++ show n)
    basicTest
    when (n <= numRuns) $ loop (n + 1)

basicTest :: IO ()
basicTest =
  withFileWatch realClock pollInterval $ \fw -> withSysTempDir $ \root' -> do
    root <- canonicalizePath root'
    p1 <- canonicalizePath (root </> "file1")
    p2 <- canonicalizePath (root </> "file2")
    t1 <- c_currentTime realClock
    void $ watchFile fw p1
    void $ watchFile fw p2

    logInfo ("First change to p1")
    writeFile p1 "abc"
    [c1] <- getChanges fw
    t2 <- subAssert $ assertFileChange c1 p1 None (Some t1)

    logInfo ("First change to p2")
    writeFile p2 "xyz"
    [c2] <- getChanges fw
    t3 <- subAssert $ assertFileChange c2 p2 None t2

    logInfo ("Second change to p2")
    writeFile p2 "123"
    [c3] <- getChanges fw
    t4 <- subAssert $ assertFileChange c3 p2 t3 t3

    logInfo ("Also watch directory, change both files")
    void $ watchFile fw root
    writeFile p1 "yuck"
    writeFile p2 "42"
    [cRoot, cP1, cP2] <- getChanges fw
    t5 <- subAssert $ assertFileChange cP1 p1 t2 t2
    t6 <- subAssert $ assertFileChange cRoot root None t2
    _ <- subAssert $ assertFileChange cP2 p2 t4 t4

    logInfo ("Change p1 again") -- should not trigger change for directory
    writeFile p1 "x"
    [c4] <- getChanges fw
    _ <- subAssert $ assertFileChange c4 p1 t5 t5

    logInfo ("Create new file")
    writeFile (root </> "file3") "x"
    [c5] <- getChanges fw
    _ <- subAssert $ assertFileChange c5 root t6 t6
    pure ()
 where
  assertFileChange
    :: FileChange -> FilePath -> Option UTCTime -> Option UTCTime -> IO (Option UTCTime)
  assertFileChange fc path old new = do
    assertEqual path (unCanonPath (fc_path fc))
    subAssert $ assertStatus path old (fc_old fc) (==) "equal to"
    subAssert $ assertStatus path new (fc_new fc) (<) "smaller than"
    pure (fmap (posixSecondsToUTCTime . fs_mtime) (fc_new fc))
  assertStatus
    :: FilePath
    -> Option UTCTime
    -> Option FileStatus
    -> (UTCTime -> UTCTime -> Bool)
    -> String
    -> IO ()
  assertStatus path old new cmp cpmDescr =
    case (old, new) of
      (None, None) -> pure ()
      (Some t, Some status) -> do
        let statusMtime = posixSecondsToUTCTime (fs_mtime status)
            msg =
              "Expected timestamp "
                ++ formatUTCTimeHiRes' t
                ++ " is not "
                ++ cpmDescr
                ++ " mtime "
                ++ formatUTCTimeHiRes' statusMtime
                ++ " of "
                ++ path
        assertBoolVerbose msg (cmp t statusMtime)
      (a, b) ->
        assertFailure
          ( "File status "
              ++ show b
              ++ " does not match expected mtime "
              ++ show a
          )
  getChanges fw = do
    c_sleep realClock (2 * pollInterval)
    timeoutFail "waitForFileChanges should be prompt" (seconds 1) $ atomically $ do
      changeSet <- waitForFileChanges fw
      pure (sort (HashSet.toList changeSet))
  pollInterval = milliseconds 10
