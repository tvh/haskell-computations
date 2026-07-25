{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Demos.Hospital.Tests (htf_thisModulesTests) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Demos.Hospital.Config
import Control.Computations.Utils.Fail
import Control.Computations.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------
import Control.Monad (unless)
import Data.List (isInfixOf)
import qualified Data.ByteString as BS
import Test.Framework

-- | The demo config shipped at "hospital-config/demo.cfg" is the one
-- config file this repo ships and the only thing exercising
-- 'Config'\'s 'FromJSON' instance end to end. Pin its parsed value here so
-- a change to the timespan parser (e.g. the migration off
-- "Control.Computations.Utils.Parser"'s 'Read'-based parsing onto
-- 'parseTimeSpan') is caught if it ever stops decoding this file.
test_parseConfigDemoFile :: IO ()
test_parseConfigDemoFile =
  do
    contents <- BS.readFile "hospital-config/demo.cfg"
    assertEqual (Ok expected) (parseConfig contents)
 where
  expected =
    Config
      { c_recentTimeSpan = hours 12
      , c_visibleAfterDischarge = minutes 5
      , c_visibleBeforeAdmission = minutes 5
      }

-- | A malformed timespan in a config value must fail with a message that
-- actually points at what's wrong, not just "invalid timespan".
test_parseConfigMalformedTimeSpan :: IO ()
test_parseConfigMalformedTimeSpan =
  do
    subAssert $ checkFails "{\"recentTime\": \"not-a-timespan\"}" "unexpected 'n'"
    subAssert $ checkFails "{\"recentTime\": \"3bogus\"}" "invalid unit: bogus"
 where
  checkFails input expectedFragment =
    case parseConfig input of
      Ok cfg -> assertFailure ("Expected a parse failure, got " ++ show cfg)
      Fail err ->
        unless (expectedFragment `isInfixOf` err) $
          assertFailure
            ("Error message " ++ show err ++ " should mention " ++ show expectedFragment)
