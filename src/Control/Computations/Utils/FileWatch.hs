{-# LANGUAGE DeriveAnyClass #-}

module Control.Computations.Utils.FileWatch (
  FileWatch,
  initFileWatch,
  closeFileWatch,
  withFileWatch,
  watchFile,
  unwatchFile,
  FileChange (..),
  waitForFileChanges,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.Utils.Clock
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Logging
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import GHC.Generics (Generic)

data FileWatch = FileWatch
  { fw_thread :: Async ()
  , fw_watchedFiles :: TVar (Map CanonPath (Option FileStatus))
  -- ^ All files being watched
  , fw_notifications :: TVar (Map CanonPath FileChange)
  -- ^ Set of pending notifications
  }

initFileWatch :: Clock -> TimeSpan -> IO FileWatch
initFileWatch clock waitInterval = do
  filesVar <- newTVarIO Map.empty
  notifyVar <- newTVarIO Map.empty
  let loop = do
        c_sleep clock waitInterval
        fileWatchCheck filesVar notifyVar
        loop
  thread <- async loop
  pure (FileWatch thread filesVar notifyVar)

closeFileWatch :: FileWatch -> IO ()
closeFileWatch fw = cancel (fw_thread fw)

withFileWatch :: Clock -> TimeSpan -> (FileWatch -> IO a) -> IO a
withFileWatch clock waitInterval action =
  bracket (initFileWatch clock waitInterval) closeFileWatch action

fileWatchCheck :: TVar (Map CanonPath (Option FileStatus)) -> TVar (Map CanonPath FileChange) -> IO ()
fileWatchCheck filesVar notifyVar = do
  logTrace "Doing file watch check ..."
  old <- readTVarIO filesVar
  newChanges <-
    fmap (Map.fromList . catMaybes) $ forM (Map.toList old) $ \(path, oldStatus) -> do
      newStatus <- safeGetFileStatus (unCanonPath path)
      pure $
        if oldStatus == newStatus
          then Nothing
          else Just (path, FileChange path oldStatus newStatus)
  if null newChanges
    then logTrace "No new changes"
    else logDebug ("New changes: " ++ show newChanges)
  let newFiles = Map.map fc_new newChanges
  -- update list of watched vars. Do not use old to avoid race conditions. Prefer
  -- new when building the union.
  atomically $ modifyTVar' filesVar $ \oldFiles -> Map.union newFiles oldFiles
  -- Update the set of notifications. We need to keep and update existing notifications
  -- because these notifications have not been collect yet.
  atomically $ modifyTVar' notifyVar $ \oldChanges ->
    Map.unionWith combineChange newChanges oldChanges
 where
  combineChange newChange oldChange =
    FileChange
      { fc_path = fc_path newChange
      , fc_old = fc_old oldChange
      , fc_new = fc_new newChange
      }

watchFile :: FileWatch -> FilePath -> IO CanonPath
watchFile fw path = do
  normPath <- canonPath path
  logDebug ("Watching file " ++ path)
  atomically $ modifyTVar' (fw_watchedFiles fw) $ Map.insertWith (\old _new -> old) normPath None
  pure normPath

unwatchFile :: FileWatch -> CanonPath -> IO ()
unwatchFile fw path = do
  logDebug ("No longer watching file " ++ unCanonPath path)
  atomically $ modifyTVar' (fw_watchedFiles fw) $ Map.delete path

data FileChange = FileChange
  { fc_path :: CanonPath
  , fc_old :: Option FileStatus
  , fc_new :: Option FileStatus
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

waitForFileChanges :: FileWatch -> STM (HashSet FileChange)
waitForFileChanges fw = do
  changes <- readTVar (fw_notifications fw)
  if Map.null changes
    then retry
    else do
      writeTVar (fw_notifications fw) Map.empty
      logDebugSTM ("Got file changes: " ++ show changes)
      pure (HashSet.fromList (Map.elems changes))
