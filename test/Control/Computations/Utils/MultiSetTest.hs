{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Utils.MultiSetTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.MultiSet

----------------------------------------
-- EXTERNAL
----------------------------------------

import Test.Framework

test_basics :: IO ()
test_basics =
  do
    assertEqual "aa" (toList ((singleton 'a') `union` (singleton 'a')))
    assertEqual "aa" (toList (fromList "aa"))
    assertEqual "aabbcc" (toList (fromList "abaccb"))
    assertEqual "aabbcc" (toList ((fromList "aab") `union` (fromList "bcc")))
    assertEqual "abc" (distinctElems (fromList "aaabcc"))
    assertEqual "b" (toList (deleteAll 'a' (fromList "aaab")))
    assertEqual (Just 3) (count 'b' (fromList "aabbbc"))
    assertEqual Nothing (count 'd' (fromList "aabbbc"))
    assertEqual "abbc" (toList (delete 'b' (fromList "abbbc")))
    assertEqual "abbb" (toList (delete 'c' (fromList "abbbc")))
