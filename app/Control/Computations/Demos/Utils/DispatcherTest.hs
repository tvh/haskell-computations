{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.Demos.Utils.DispatcherTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Demos.Utils.Dispatcher
import Control.Computations.Utils.Clock
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import Control.Concurrent.Async
import Control.Concurrent.STM
import Test.Framework

test_basics :: IO ()
test_basics = do
  changesVar <- newTVarIO None
  let emitChange i = atomically $ writeTVar changesVar (Some i)
  withDispatcher (wait changesVar) $ \disp -> do
    l1 <- mkListener disp
    v1 <- newTVarIO []
    withAsync (listen v1 l1) $ \_ -> do
      emitChange 1
      l2 <- mkListener disp
      v2 <- newTVarIO []
      withAsync (listen v2 l2) $ \_ -> do
        sleep
        emitChange 2
        sleep
        emitChange 3
        sleep
        list1 <- readTVarIO v1
        assertEqual [3, 2, 1] list1
        list2 <- readTVarIO v2
        assertEqual [3, 2, 1] list2
 where
  sleep = c_sleep realClock (milliseconds 5)
  listen :: TVar [Int] -> Listener Int -> IO ()
  listen v l = do
    x <- atomically $ waitListener l
    atomically $ modifyTVar' v $ \list -> x : list
    listen v l
  wait :: TVar (Option Int) -> STM Int
  wait v = do
    x <- readTVar v
    case x of
      None -> retry
      Some y -> do
        writeTVar v None
        pure y
