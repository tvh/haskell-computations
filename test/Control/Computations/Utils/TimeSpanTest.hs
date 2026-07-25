{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Utils.TimeSpanTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Fail
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Test.Framework

test_showTimeSpan :: IO ()
test_showTimeSpan =
  do
    assertEqual "1ms1us" (show $ TimeSpan 1001)
    assertEqual "1days1us" (show $ days 1 `plusTimeSpan` microseconds 1)

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
