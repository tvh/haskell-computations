{-# LANGUAGE RankNTypes #-}

{- | A small, swappable clock abstraction: current time, sleeping, and
 timeouts, all behind one record. Code that needs to know the time or wait
 takes a 'Clock' argument instead of calling "Data.Time"/"Control.Concurrent"
 directly, so tests can substitute a virtual clock. Most callers just want
 'realClock'.
-}
module Control.Computations.Utils.Clock (
  Clock (..),
  TimeoutFun,
  makeClock,
  realClock,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.TimeUtils (
  diffTime,
 )

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.Time.Clock
import qualified System.Timeout as Sys

-- | Runs @action@, giving up and returning 'Nothing' after the given
-- 'TimeSpan' if it hasn't completed.
type TimeoutFun a = TimeSpan -> IO a -> IO (Maybe a)

-- | A clock: how to read the current time, sleep for a duration or until an
-- absolute time, and time an action out. 'realClock' is the ordinary
-- wall-clock instance; construct your own with 'makeClock' to control time
-- in tests.
data Clock = Clock
  { c_currentTime :: IO UTCTime
  , c_timeout :: forall a. TimeSpan -> IO a -> IO (Maybe a)
  , c_sleep :: TimeSpan -> IO ()
  , c_sleepUntil :: UTCTime -> IO ()
  }

-- | Build a 'Clock' from a time source, a timeout function, and a sleep
-- function. 'c_sleepUntil' is derived automatically from the given
-- @getTime@/@sleepFun@.
makeClock :: IO UTCTime -> (forall a. TimeoutFun a) -> (TimeSpan -> IO ()) -> Clock
makeClock getTime timeoutFun sleepFun =
  Clock
    { c_currentTime = getTime
    , c_timeout = timeoutFun
    , c_sleep = sleepFun
    , c_sleepUntil = \targetTime ->
        do
          currentTime <- getTime
          let sleepSpan = targetTime `diffTime` currentTime
          when (isPositiveTimeSpan sleepSpan) $ sleepFun sleepSpan
    }

-- | The real wall-clock 'Clock': 'Data.Time.Clock.getCurrentTime',
-- "System.Timeout"-based timeouts, and 'Control.Concurrent.threadDelay'-based
-- sleeping.
realClock :: Clock
realClock = makeClock getCurrentTime timeout sleep
 where
  timeout ts action = Sys.timeout (asMicroseconds ts) action
  sleep ts = threadDelay (asMicroseconds ts)
