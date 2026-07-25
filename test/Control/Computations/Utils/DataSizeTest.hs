{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Utils.DataSizeTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.DataSize

----------------------------------------
-- EXTERNAL
----------------------------------------

import Test.Framework

test_displayDataSize :: IO ()
test_displayDataSize =
  do
    assertEqual "1024B" (displayDataSize $ bytes 1024)
    assertEqual "2047B" (displayDataSize $ bytes 2047)
    assertEqual "2KiB" (displayDataSize $ bytes 2048)
    assertEqual "56.8KiB" (displayDataSize $ bytes 58182)
    assertEqual "58.6KiB" (displayDataSize $ bytes 59960)
