{-# LANGUAGE DeriveAnyClass #-}

{- | Small helpers for working with 'UTCTime': truncating to a coarse
 interval (see 'TimeIntervalType'), parsing/formatting in the format this
 library uses throughout (@YYYY-MM-DD HH:MM:SS@, optionally with
 sub-second precision), and 'TimeSpan' arithmetic. 'TimeSrc' (in
 "Control.Computations.FlowImpls.TimeSrc") is the main consumer of
 'TimeIntervalType'.
-}
module Control.Computations.Utils.TimeUtils (
  TimeIntervalType (..),
  truncateTime,
  parseUTCTime,
  unsafeParseUTCTime,
  formatUTCTime,
  formatUTCTime',
  formatUTCTimeNoSeconds,
  formatUTCTimeNoSeconds',
  formatUTCTimeHiRes,
  formatUTCTimeHiRes',
  formatDay,
  formatDay',
  diffTime,
  addTimeSpan,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------
import Data.Hashable
import qualified Data.LargeHashable as LH
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import GHC.Generics (Generic)

-- | A coarse, named time granularity to truncate a 'UTCTime' down to (see
-- 'truncateTime'), e.g. for bucketing periodic ticks. The sub-minute
-- intervals exist for tests; production code should generally use
-- 'TimeInterval1min' or coarser.
data TimeIntervalType
  = TimeInterval1s -- you should only use this in tests
  | TimeInterval10s -- you should only use this in tests
  | TimeInterval1min
  | TimeInterval5min
  -- more omitted
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance LH.LargeHashable TimeIntervalType

truncateToNSeconds :: Int -> UTCTime -> UTCTime
truncateToNSeconds (toInteger -> n) t =
  let secs = floor $ toRational $ utcTimeToPOSIXSeconds t
      truncated = n * (secs `div` n)
   in posixSecondsToUTCTime (fromInteger truncated)

intervalToSeconds :: TimeIntervalType -> Int
intervalToSeconds interval =
  case interval of
    TimeInterval1s -> 1
    TimeInterval10s -> 10
    TimeInterval1min -> 60
    TimeInterval5min -> 300

-- | Round a 'UTCTime' down to the start of its enclosing interval, e.g.
-- @truncateTime t TimeInterval1min@ zeroes out the seconds.
truncateTime :: UTCTime -> TimeIntervalType -> UTCTime
truncateTime t interval = truncateToNSeconds (intervalToSeconds interval) t

timeFormatString :: String
timeFormatString = "%Y-%m-%d %H:%M:%S"

timeFormatStringNoSeconds :: String
timeFormatStringNoSeconds = "%Y-%m-%d %H:%M:%S"

timeFormatStringHiRes :: String
timeFormatStringHiRes = "%Y-%m-%d %H:%M:%S.%q"

dayFormatString :: String
dayFormatString = "%Y-%m-%d"

-- | Parse @\"YYYY-MM-DD HH:MM:SS\"@ as a 'UTCTime', failing in any
-- 'MonadFail' (e.g. 'Maybe' or 'Either' 'String') on a malformed string.
parseUTCTime :: MonadFail m => String -> m UTCTime
parseUTCTime = parseTimeM True defaultTimeLocale timeFormatString

-- | Like 'parseUTCTime', but calls 'error' on a malformed string instead of
-- failing softly. Prefer 'parseUTCTime' unless the input is a compile-time
-- literal you already know is well-formed.
unsafeParseUTCTime :: String -> UTCTime
unsafeParseUTCTime s =
  case parseUTCTime s of
    Nothing -> error ("Cannot parse " ++ show s ++ " as UTCTime")
    Just t -> t

formatUTCTime :: UTCTime -> T.Text
formatUTCTime = T.pack . formatUTCTime'

formatUTCTime' :: UTCTime -> String
formatUTCTime' = formatTime defaultTimeLocale timeFormatString

formatUTCTimeNoSeconds :: UTCTime -> T.Text
formatUTCTimeNoSeconds = T.pack . formatUTCTimeNoSeconds'

formatUTCTimeNoSeconds' :: UTCTime -> String
formatUTCTimeNoSeconds' = formatTime defaultTimeLocale timeFormatStringNoSeconds

formatUTCTimeHiRes :: UTCTime -> T.Text
formatUTCTimeHiRes = T.pack . formatUTCTimeHiRes'

formatUTCTimeHiRes' :: UTCTime -> String
formatUTCTimeHiRes' = formatTime defaultTimeLocale timeFormatStringHiRes

formatDay :: Day -> T.Text
formatDay = T.pack . formatDay'

formatDay' :: Day -> String
formatDay' = formatTime defaultTimeLocale dayFormatString

-- | The 'TimeSpan' between two times, @t1 \`diffTime\` t2 = t1 - t2@.
diffTime :: UTCTime -> UTCTime -> TimeSpan
diffTime t1 t2 = nominalDiffTimeSpan (diffUTCTime t1 t2)

-- | Add a 'TimeSpan' to a 'UTCTime'.
addTimeSpan :: UTCTime -> TimeSpan -> UTCTime
addTimeSpan t ts = asNominalDiffTime ts `addUTCTime` t
