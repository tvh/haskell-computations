{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Utils.TimeSpan (
  TimeSpan,
  zeroTime,
  isPositiveTimeSpan,
  isInRange,
  nominalDiffTimeSpan,
  picoseconds,
  nanoseconds,
  microseconds,
  microseconds',
  milliseconds,
  milliseconds',
  seconds,
  seconds',
  doubleSeconds,
  minutes,
  hours,
  days,
  years,
  asNominalDiffTime,
  asMicroseconds,
  asMilliseconds,
  asSeconds,
  asSeconds',
  asMinutes,
  asHours,
  asDays,
  diffTimeSpan,
  plusTimeSpan,
  multiplyTimeSpan,
  parseTimeSpan,
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad (join, void)
import Data.Hashable
import Data.LargeHashable
import qualified Data.Text as T
import Data.Time.Clock
import Data.Void (Void)
import GHC.Generics (Generic)
import Test.Framework
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as L

-- The tiny slice of "Control.Computations.Utils.Parser" (deleted; this was
-- its sole caller) that 'timeSpanP' actually needs: a parser type alias
-- plus the whitespace/integer lexeme parsers and the two ways to run a
-- parser and get back a 'Fail'.
type Parser = P.Parsec Void T.Text

spaceP :: Parser ()
spaceP =
  L.space
    (void P.spaceChar)
    (fail "no support for line comments")
    (fail "no support for block comments")

lexemeP :: Parser a -> Parser a
lexemeP = L.lexeme spaceP

integerP :: Parser Integer
integerP = L.signed P.space (lexemeP L.decimal)

parseM :: Parser a -> FilePath -> T.Text -> Fail a
parseM parser filename value =
  case P.parse (parser <* P.eof) filename value of
    Left err -> fail (P.errorBundlePretty err)
    Right x -> return x

parseM' :: Parser a -> String -> T.Text -> Fail (a, T.Text)
parseM' p fn str =
  let initialState =
        -- Unfortunately not exported by megaparsec
        P.State
          { P.stateInput = str
          , P.stateOffset = 0
          , P.stateParseErrors = []
          , P.statePosState =
              P.PosState
                { P.pstateInput = str
                , P.pstateOffset = 0
                , P.pstateSourcePos = P.initialPos fn
                , P.pstateTabWidth = P.defaultTabWidth
                , P.pstateLinePrefix = ""
                }
          }
      (s, mRes) = P.runParser' p initialState
   in case mRes of
        Left e -> fail $ P.errorBundlePretty e
        Right res -> return (res, P.stateInput s)

-- | Represents a time span with microsecond resolution.
newtype TimeSpan = TimeSpan {unTimeSpan :: Integer}
  deriving newtype (Eq, Ord, Num, Hashable)
  deriving stock (Generic)

instance LargeHashable TimeSpan

instance Arbitrary TimeSpan where
  arbitrary =
    do
      us <- arbitrary
      s <- arbitrary
      h <- arbitrary
      d <- arbitrary
      return (microseconds us + seconds s + hours h + days d)

instance Show TimeSpan where
  showsPrec _ (TimeSpan t)
    | t < 0 = showString "-" . shows (TimeSpan (-t))
    | us == 0 && ms == 0 && s == 0 && mins == 0 && h == 0 && ds == 0 =
        showString "0us"
    | otherwise =
        possiblyShow ds "days"
          . possiblyShow h "h"
          . possiblyShow mins "min"
          . possiblyShow s "s"
          . possiblyShow ms "ms"
          . possiblyShow us "us"
   where
    possiblyShow i unit =
      if i == 0 then id else shows i . showString unit
    us = t `mod` 1000
    ms = t `div` 1000 `mod` 1000
    s = t `div` (1000 ^ two) `mod` 60
    mins = t `div` (1000 ^ two * 60) `mod` 60
    h = t `div` (1000 ^ two * 60 ^ two) `mod` 24
    ds = t `div` (1000 ^ two * 60 ^ two * 24)
    two :: Integer
    two = 2

test_showTimeSpan :: IO ()
test_showTimeSpan =
  do
    assertEqual "1ms1us" (show $ TimeSpan 1001)
    assertEqual "1days1us" (show $ days 1 `plusTimeSpan` microseconds 1)

timeSpanP :: Parser TimeSpan
timeSpanP =
  do
    spaceP
    firstTime <- join $ parseAsMs <$> integerP <*> P.many P.letterChar
    otherTimes <-
      do
        otherUs <- P.many $ (,) <$> L.decimal <*> P.many P.letterChar
        sum <$> mapM (Prelude.uncurry parseAsMs) otherUs
    return . TimeSpan $ firstTime `with` otherTimes
 where
  with initTime rest =
    let f = if initTime < 0 then (-) else (+)
     in initTime `f` rest
  parseAsMs :: Integer -> String -> Parser Integer
  parseAsMs num = \case
    "us" -> return num
    "ms" -> return $ 1000 * num
    "s" -> return $ 1000 ^ two * num
    "m" -> return $ toMin num
    "min" -> return $ toMin num
    "h" -> return $ 1000 ^ two * 60 ^ two * num
    "d" -> return $ toDays num
    "days" -> return $ toDays num
    u ->
      fail ("Got number " ++ show num ++ " with invalid unit: " ++ u)
  toMin num = 1000 ^ two * 60 * num
  toDays num = 1000 ^ two * 60 ^ two * 24 * num
  two = 2 :: Integer

test_timeSpanP :: IO ()
test_timeSpanP =
  do
    assertEqual (Ok $ minutes 3) (parseM timeSpanP "" "3m")
    assertEqual (Ok $ minutes 3) (parseM timeSpanP "" "3min")
    assertEqual (Ok $ days (-1)) (parseM timeSpanP "" "-1days")
    assertEqual (Ok $ days (-1)) (parseM timeSpanP "" "-1d")
    assertEqual (Ok (days (-1), " ")) (parseM' timeSpanP "" " -1d ")
    assertEqual (Ok $ days (-1) `plusTimeSpan` hours (-1)) (parseM timeSpanP "" "-1days1h")
    assertEqual (Ok $ days 0) (parseM timeSpanP "" " 0us")
    assertEqual (Ok longTime) (parseM timeSpanP "" "3days3h2s999ms998us")
    assertBool $ isFail (parseM timeSpanP "" "-1days-1h")
    assertBool $ isFail (parseM timeSpanP "" "-1days 1h")
 where
  longTime =
    foldr
      plusTimeSpan
      (days 0)
      [days 3, hours 3, seconds 2, milliseconds 999, microseconds 998]

prop_timeSpanP :: TimeSpan -> Bool
prop_timeSpanP f =
  parseM timeSpanP "" (showText f) == Ok f

-- Note: 'prop_readShow' (round-tripping via 'read'/'show') used to live
-- here alongside a 'Read TimeSpan' instance. Now that 'Read TimeSpan' is
-- gone (its sole consumer, Config.hs, calls 'parseTimeSpan' directly),
-- that round-trip property is exactly 'prop_timeSpanP' above --
-- 'parseTimeSpan' is defined as 'parseM timeSpanP ""' -- so it was dropped
-- as redundant rather than retargeted.

-- | Parse a 'TimeSpan' from its textual form (e.g. @"3days3h"@), the
-- megaparsec-free entry point for callers such as config-file parsing that
-- shouldn't need to know this module is implemented with 'Parser'. On
-- failure, the 'Fail' string is megaparsec's own parse-error message, which
-- is far more informative than a generic "invalid input" string.
parseTimeSpan :: T.Text -> Fail TimeSpan
parseTimeSpan = parseM timeSpanP ""

zeroTime :: TimeSpan
zeroTime = TimeSpan 0

isPositiveTimeSpan :: TimeSpan -> Bool
isPositiveTimeSpan (TimeSpan t) = t > 0

isInRange :: TimeSpan -> TimeSpan -> Bool
isInRange range obj = obj < range && obj > -range

nominalDiffTimeSpan :: NominalDiffTime -> TimeSpan
nominalDiffTimeSpan dt = nanoseconds (truncate (dt * 1000 * 1000 * 1000))

picoseconds :: Integer -> TimeSpan
picoseconds = TimeSpan . (`div` (1000 * 1000))

nanoseconds :: Integer -> TimeSpan
nanoseconds = TimeSpan . (`div` 1000)

microseconds :: Int -> TimeSpan
microseconds = TimeSpan . fromIntegral

microseconds' :: Integer -> TimeSpan
microseconds' = TimeSpan

milliseconds :: Int -> TimeSpan
milliseconds i = TimeSpan (fromIntegral i * 1000)

milliseconds' :: Integer -> TimeSpan
milliseconds' i = TimeSpan (i * 1000)

seconds :: Int -> TimeSpan
seconds i = TimeSpan (fromIntegral i * 1000000)

seconds' :: Integer -> TimeSpan
seconds' i = TimeSpan (i * 1000000)

doubleSeconds :: Double -> TimeSpan
doubleSeconds d = TimeSpan (round $ d * 1000000)

minutes :: Int -> TimeSpan
minutes i = TimeSpan (fromIntegral i * 1000000 * 60)

hours :: Int -> TimeSpan
hours i = TimeSpan (fromIntegral i * 1000000 * 60 * 60)

days :: Int -> TimeSpan
days i = TimeSpan (fromIntegral i * 24 * 1000000 * 60 * 60)

years :: Int -> TimeSpan
years i = days (i * 365)

asNominalDiffTime :: TimeSpan -> NominalDiffTime
asNominalDiffTime = asSeconds'

asMicroseconds :: Integral i => TimeSpan -> i
asMicroseconds = fromIntegral . unTimeSpan

asMilliseconds :: Integral i => TimeSpan -> i
asMilliseconds (TimeSpan t) = fromIntegral (t `div` 1000)

asSeconds :: Integral i => TimeSpan -> i
asSeconds (TimeSpan t) = fromIntegral (t `div` 1000000)

asSeconds' :: Fractional a => TimeSpan -> a
asSeconds' (TimeSpan t) = fromInteger t / (1000 * 1000)

asMinutes :: Integral i => TimeSpan -> i
asMinutes (TimeSpan t) = fromIntegral (t `div` (1000000 * 60))

asHours :: Integral i => TimeSpan -> i
asHours (TimeSpan t) = fromIntegral (t `div` (1000000 * 60 * 60))

asDays :: TimeSpan -> Double
asDays (TimeSpan t) = (fromIntegral t) / (1000000 * 60 * 60 * 24)

diffTimeSpan :: TimeSpan -> TimeSpan -> TimeSpan
diffTimeSpan (TimeSpan t1) (TimeSpan t0) = TimeSpan (t1 - t0)

plusTimeSpan :: TimeSpan -> TimeSpan -> TimeSpan
plusTimeSpan (TimeSpan t1) (TimeSpan t0) = TimeSpan (t1 + t0)

multiplyTimeSpan :: Real a => a -> TimeSpan -> TimeSpan
multiplyTimeSpan x (TimeSpan t) = TimeSpan . round $ toRational x * toRational t
