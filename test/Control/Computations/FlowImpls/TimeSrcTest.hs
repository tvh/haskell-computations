{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.FlowImpls.TimeSrcTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompSrc
import Control.Computations.FlowImpls.TimeSrc
import Control.Computations.Utils.ConcUtils
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.TimeUtils
import Control.Computations.Utils.Types
import Control.Computations.Utils.VirtualTime

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad
import qualified Data.HashSet as HashSet
import Data.Time.Clock
import Test.Framework

withTestTimeSrc :: UTCTime -> (VirtualTime -> TimeSrc -> IO a) -> IO a
withTestTimeSrc t0 f =
  do
    vt <- initVirtualTime t0
    let clock = virtualClock vt
    withTimeSrc "testTimeSrc" clock $ \dif ->
      f vt dif

test_timeCompSrc :: IO ()
test_timeCompSrc =
  timeoutFail "TimeSrc should be prompt" (seconds 1) $
    withTestTimeSrc t0 $ \vt dif ->
      do
        let allIntervals = [minBound .. maxBound]
        forM_ allIntervals $ \int ->
          do
            (deps, res) <- compSrcExecute dif (GetTime int)
            let expected = truncateTime t0 int
            assertEqual (HashSet.singleton (Dep (TimeKey int) (TimeVer expected))) deps
            assertEqual (Ok expected) res

        -- no change as time stays constant
        changes <- atomically $ fmap Some (compSrcWaitChanges dif) `orElse` pure None
        assertEqual None changes

        addVirtualTimeSpan vt (seconds 1)
        changes <- atomically $ compSrcWaitChanges dif
        let t1 = addTimeSpan t0 (seconds 1)
            expected = HashSet.singleton (Dep (TimeKey TimeInterval1s) (TimeVer t1))
        assertEqual expected changes

        addVirtualTimeSpan vt (seconds 1)
        changes <- atomically $ compSrcWaitChanges dif
        let t2 = addTimeSpan t0 (seconds 2)
            expected = HashSet.singleton (Dep (TimeKey TimeInterval1s) (TimeVer t2))
        assertEqual expected changes
 where
  t0 = unsafeParseUTCTime "2023-04-28 23:32:01"
