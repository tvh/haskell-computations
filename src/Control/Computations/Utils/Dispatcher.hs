-- | Dispatches changes from one source to multiple listeners.
module Control.Computations.Utils.Dispatcher (
  Dispatcher,
  Listener,
  initDispatcher,
  closeDispatcher,
  withDispatcher,
  mkListener,
  waitListener,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Data.Strict.Tuple (Pair (..), (:!:))
import qualified Data.Strict.Tuple as S

data Dispatcher a = Dispatcher
  { d_thread :: Async ()
  , d_var :: TVar (Option (Int :!: a))
  }

initDispatcher :: STM a -> IO (Dispatcher a)
initDispatcher wait = do
  v <- newTVarIO None
  t <- async (waitLoop 0 v)
  pure (Dispatcher t v)
 where
  waitLoop !i v = do
    x <- atomically wait
    atomically $ writeTVar v (Some (i :!: x))
    waitLoop (i + 1) v

closeDispatcher :: Dispatcher a -> IO ()
closeDispatcher d = cancel (d_thread d)

withDispatcher :: STM a -> (Dispatcher a -> IO b) -> IO b
withDispatcher wait = bracket (initDispatcher wait) closeDispatcher

data Listener a = Listener
  { l_prev :: TVar Int
  , l_var :: TVar (Option (Int :!: a))
  }

mkListener :: Dispatcher a -> IO (Listener a)
mkListener d = do
  cur <- readTVarIO (d_var d)
  start <- newTVarIO $ (option 0 S.fst cur) - 1
  pure (Listener start (d_var d))

waitListener :: Listener a -> STM a
waitListener l = do
  prev <- readTVar (l_prev l)
  x <- readTVar (l_var l)
  case x of
    None -> retry
    Some (cur :!: y) ->
      if prev == cur
        then retry
        else do
          writeTVar (l_prev l) cur
          pure y
