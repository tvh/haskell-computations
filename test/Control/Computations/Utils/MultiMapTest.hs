{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Utils.MultiMapTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.MultiMap
import qualified Control.Computations.Utils.MultiSet as MSet

----------------------------------------
-- EXTERNAL
----------------------------------------

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Test.Framework
import Prelude hiding (filter, lookup)

test_filterWithKey :: IO ()
test_filterWithKey =
  do assertEqual r1 $ filterWithKey f mmap1
 where
  f :: a -> Bool -> Bool
  f _ v = v

  r1 :: MultiMap Int Bool
  r1 =
    (MultiMap . HashMap.fromList)
      [ (1, HashSet.singleton True)
      , (3, HashSet.singleton True)
      ]

  mmap1 :: MultiMap Int Bool
  mmap1 =
    fromList
      [ (1, True)
      , (2, False)
      , (3, True)
      , (3, False)
      ]

mmap1 :: MultiMap Int Int
mmap1 =
  fromList
    [ (1, 1)
    , (1, 2)
    , (1, 3)
    , (2, 3)
    ]

test_numberOfKeysAndValues :: IO ()
test_numberOfKeysAndValues =
  do
    assertEqual 2 $ numberOfKeys mmap1
    assertEqual 4 $ numberOfKeyValuePairs mmap1

test_elems :: IO ()
test_elems =
  do
    assertEqual (MSet.fromList [1, 2, 3, 3]) $ MSet.fromList $ elems mmap1
    assertEqual (MSet.fromList [1, 2, 3]) $ MSet.fromList $ elems mmap2
    assertEqual (MSet.fromList [1, 2, 3]) $ MSet.fromList $ elems mmap3
    assertEqual (MSet.fromList [1, 1, 1]) $ MSet.fromList $ elems mmap4
    assertEqual [] $ elems $ fromList ([] :: [(Int, Int)])
 where
  mmap2 :: MultiMap Int Int
  mmap2 =
    fromList
      [ (1, 1)
      , (2, 2)
      , (2, 3)
      ]

  mmap3 :: MultiMap Int Int
  mmap3 =
    fromList
      [ (1, 1)
      , (1, 2)
      , (1, 3)
      ]

  mmap4 :: MultiMap Int Int
  mmap4 =
    fromList
      [ (1, 1)
      , (2, 1)
      , (3, 1)
      ]
