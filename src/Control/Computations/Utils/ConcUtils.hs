module Control.Computations.Utils.ConcUtils (
  ignoreThreadKilled,
  ignoreThreadKilled',
  timeout,
  timeoutFail,
  trySync,
  dispatchJobs,
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

 Written for 'dispatchJobs': a worker there must be able to report its own
 job's exception back to its caller without ever swallowing a genuine
 'Async.cancel' (or 'killThread') aimed at the worker itself -- silently
 catching that would leave the worker running arbitrary user
 'Control.Computations.CompEngine.CompSrc.compSrcExecute' code after
 whatever cancelled it believes the whole pool is gone.
-}
trySync :: IO a -> IO (Either SomeException a)
trySync action =
  Control.Exception.try action >>= \case
    Left e
      | Just (SomeAsyncException _) <- fromException e -> throwIO e
      | otherwise -> pure (Left e)
    Right v -> pure (Right v)

{- | Run @jobs@ against a fixed pool of at most @width@ concurrent workers,
 then block until every job has finished. Callers must not let this overlap
 whatever runs after it -- see
 "Control.Computations.CompEngine.Impl"'s @SrcJobs@\/@Prep@, the only
 current caller, for why its own dispatch-then-drain discipline depends on
 that.

 A worker repeatedly pops the next job off a shared queue (an 'IORef',
 popped via 'atomicModifyIORef'') rather than being handed one job each, so
 @width@ stays fixed no matter how many jobs there are: a ten-thousand-job
 batch still spawns at most @width@ workers, not ten thousand threads.

 Built on 'Async.replicateConcurrently_' rather than a bare 'forkIO' per
 worker: it is 'Async.withAsync'-scoped, so an asynchronous exception that
 unwinds the caller -- in particular 'Async.cancel' reaching the thread that
 called this -- tears down every still-running worker with it. A bare
 'forkIO' worker would survive that unwind and keep running arbitrary job
 code (ultimately a caller's own 'Control.Computations.CompEngine.CompSrc.compSrcExecute')
 after whatever owns the pool is already gone.

 Each @job@ is expected to catch its own synchronous exceptions (e.g. via
 'trySync') and record the outcome itself -- this just runs @[IO ()]@, it
 does not interpret what a job does or report back anything beyond \"all
 jobs have run\".
-}
dispatchJobs :: Int -> [IO ()] -> IO ()
dispatchJobs width jobs = do
  ref <- newIORef jobs
  let pop [] = ([], Nothing)
      pop (x : xs) = (xs, Just x)
      worker = atomicModifyIORef' ref pop >>= maybe (pure ()) (\j -> j >> worker)
  Async.replicateConcurrently_ (min width (length jobs)) worker
