{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Utils.IOUtilsTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.IOUtils

----------------------------------------
-- EXTERNAL
----------------------------------------

import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import Test.Framework

test_listDirectoryRecursive :: IO ()
test_listDirectoryRecursive =
  withSysTempDir $ \rootDir ->
    do
      BS.writeFile (rootDir </> "x") "foo"
      createDirectory (rootDir </> "A")
      BS.writeFile (rootDir </> "A/x") "bar"
      createDirectory (rootDir </> "A/B")
      BS.writeFile (rootDir </> "A/B/x") "baz"
      createDirectoryLink
        (rootDir </> "A") -- target
        (rootDir </> "A/B/c") -- name of link
      createFileLink
        (rootDir </> "x") -- target
        (rootDir </> "A/B/y") -- name of link
      l <- listDirectoryRecursive rootDir
      assertListsEqualAsSets
        (map (rootDir </>) ["x", "A", "A/x", "A/B", "A/B/x", "A/B/c", "A/B/y"])
        (map fst l)
