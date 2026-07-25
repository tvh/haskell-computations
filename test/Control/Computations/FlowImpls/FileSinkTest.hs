{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.FlowImpls.FileSinkTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.FlowImpls.FileSink
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.HashSet as HashSet
import System.FilePath
import Test.Framework

test_basics :: IO ()
test_basics =
  withSysTempDir $ \rootDir ->
    do
      sink <- makeFileSink "ident" rootDir
      void $ executeImpl sink (WriteFile "x" "1")
      (out2, Ok ()) <- executeImpl sink (MakeDirs "d1/d2")
      void $ executeImpl sink (WriteFile "d1/d2/y" "2")
      subAssert $
        assertContent
          sink
          [("x", File), ("d1", Dir), ("d1/d2", Dir), ("d1/d2/y", File)]
          [("x", "1"), ("d1/d2/y", "2")]
      deleteImpl sink out2
      deleteImpl sink out2 -- no error
      subAssert $
        assertContent
          sink
          [("x", File), ("d1", Dir)]
          [("x", "1")]
      void $ executeImpl sink (WriteFile "d1" "3")
      subAssert $
        assertContent
          sink
          [("x", File), ("d1", File)]
          [("x", "1"), ("d1", "3")]
      void $ executeImpl sink (MakeDirs "x")
      subAssert $
        assertContent
          sink
          [("x", Dir), ("d1", File)]
          [("d1", "3")]
 where
  assertContent :: FileSink -> [(FilePath, FileOrDir)] -> [(FilePath, BS.ByteString)] -> IO ()
  assertContent sink outs fileContents =
    do
      realOuts <- listExistingOutputsImpl sink
      outs' <- forM outs $ \(p, t) ->
        do
          outP <- mkOutPath p
          pure (FileSinkOut outP t)
      assertEqual (HashSet.fromList outs') realOuts
      subAssert $ forM_ fileContents (assertFileContent sink)
  assertFileContent :: FileSink -> (FilePath, BS.ByteString) -> IO ()
  assertFileContent sink (p, c) =
    do
      let fullP = unCanonPath (fcs_root sink) </> p
      bs <- BS.readFile fullP
      assertEqual c bs
