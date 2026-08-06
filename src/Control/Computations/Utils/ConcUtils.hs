module Control.Computations.Utils.ConcUtils (
  ignoreThreadKilled,
  ignoreThreadKilled',
  timeout,
  timeoutFail,
  trySync,
  forkTracked,
  cancelAllTracked,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Clock
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent
import qualified Control.Concurrent.Async as Async
import Control.Exception
import Control.Monad.Catch
import qualified Control.Monad.Catch as Catch
import Control.Monad.IO.Class
import Data.Functor (void)
import Data.IORef
import qualified System.Timeout as Sys

ignoreThreadKilled :: (MonadIO m, MonadCatch m) => m () -> m ()
ignoreThreadKilled = ignoreThreadKilled' ()

ignoreThreadKilled' :: (MonadIO m, MonadCatch m) => a -> m a -> m a
ignoreThreadKilled' onKilled action =
  Catch.catchJust predicate action handler
 where
  handler msg =
    do
      tid <- liftIO myThreadId
      liftIO $ logDebug (msg tid)
      return onKilled
  predicate exc
    | Just ThreadKilled <- fromException exc =
        Just $ \tid -> "Ignoring ThreadKilled exception for " ++ (show tid)
    | Just Async.AsyncCancelled <- fromException exc =
        Just $ \tid -> "Ignoring AsyncCanceled exception for " ++ (show tid)
    | otherwise = Nothing

timeout :: TimeoutFun a
timeout t = Sys.timeout (asMicroseconds t)

timeoutFail :: String -> TimeSpan -> IO a -> IO a
timeoutFail name ts action =
  do
    res <- timeout ts action
    case res of
      Nothing -> fail (name ++ " timed out after " ++ show ts)
      Just v -> pure v

{- | Run @action@, catching a __synchronous__ exception into a 'Left' rather
 than letting it propagate -- but re-throwing an __asynchronous__ one
 (anything caught that 'fromException's to 'SomeAsyncException', which
 includes both this module's 'Control.Exception.AsyncException' and
 "Control.Concurrent.Async"'s own 'Async.AsyncCancelled' -- see the
 'Exception' instances those two define, both of which route through
 'asyncExceptionToException'\/'asyncExceptionFromException' precisely so a
 catch-all like this one can tell them apart from an ordinary synchronous
 exception).

 Written for "Control.Computations.CompEngine.Impl"'s permit-pool forks
 ('Control.Computations.CompEngine.Impl.prepEvalLeaf's eval-leaf forks):
 each must be able to report its own job's exception back to its caller
 without ever swallowing a genuine 'Async.cancel' (or 'killThread') aimed
 at the fork itself -- silently catching that would leave the fork running
 arbitrary user code (ultimately a cap's own body) after whatever cancelled
 it believes the pool is gone.
-}
trySync :: IO a -> IO (Either SomeException a)
trySync action =
  Control.Exception.try action >>= \case
    Left e
      | Just (SomeAsyncException _) <- fromException e -> throwIO e
      | otherwise -> pure (Left e)
    Right v -> pure (Right v)

{- | Fork @action@ onto its own 'Async.Async', appending an existentially
 result-erased copy of the handle to @pendingRef@ so a caller can guarantee
 -- via 'cancelAllTracked' -- that every fork it ever started during some
 dynamic-width unit of work (a batch's eval-leaf forks, in
 "Control.Computations.CompEngine.Impl"'s @doSuspended@ machinery -- see
 'Control.Computations.CompEngine.Impl.prepEvalLeaf', its only current
 caller) is accounted for even if the caller never gets around to
 individually joining it (e.g. an earlier sibling leaf's exception means the
 run phase never reaches this one at all -- see 'cancelAllTracked').

 The returned handle keeps its real result type @a@, for the caller's own
 individual 'Control.Concurrent.Async.wait'; only the copy filed into
 @pendingRef@ is erased to @()@ (via 'Async.Async'\'s own 'Functor'
 instance, not a new thread or a new action -- @void h@ is the same
 underlying 'Async.Async' with a different phantom result type), which is
 all blanket cancellation ever needs to know about it.
-}
forkTracked :: IORef [Async.Async ()] -> IO a -> IO (Async.Async a)
forkTracked pendingRef action = do
  h <- Async.async action
  atomicModifyIORef' pendingRef (\hs -> (void h : hs, ()))
  pure h

{- | Cancel every fork accumulated in @pendingRef@ by 'forkTracked' -- correct
 whether or not each one was already individually joined:
 'Async.cancel' on an 'Async.Async' whose action has already completed (or
 already been cancelled) is a documented no-op. Meant to run in a 'finally'
 around the forking phase it guards, so it fires on both the success path
 (every fork already joined, so this is a no-op sweep) and the exception
 path (some forks never individually joined, so this is what actually tears
 them down) -- see "Control.Computations.CompEngine.Impl"'s @doSuspended@
 (wraps 'enginePhase'), its only current caller.
-}
cancelAllTracked :: IORef [Async.Async ()] -> IO ()
cancelAllTracked pendingRef = readIORef pendingRef >>= mapM_ Async.cancel
