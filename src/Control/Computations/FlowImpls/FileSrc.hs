{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

{- | A 'Control.Computations.CompEngine.CompSrc.CompSrc' backed by the local
 filesystem: read a file ('ReadFile'\/'ReadTextFile') or list a directory
 ('ListDir'), and the source tracks the underlying path's modification time
 so the engine knows when to rerun anything that depended on it. Build one
 with 'withFileSrc' (or 'initFileSrc'\/'closeFileSrc' to manage the lifetime
 by hand), configured via 'defaultFileSrcConfig'.
-}
module Control.Computations.FlowImpls.FileSrc (
  FileSrcReq (..),
  FileSrcConfig (..),
  defaultFileSrcConfig,
  DirEntry (..),
  FileSrc,
  FileKey (..),
  FileVer (..),
  initFileSrc,
  closeFileSrc,
  withFileSrc,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine.CompSrc
import Control.Computations.Utils.Clock
import Control.Computations.Utils.FileWatch
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.TimeSpan
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.LargeHashable as LH
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import System.Directory
import System.FilePath

-- | A request against a 'FileSrc': read a file as bytes or text, or list a
-- directory's entries.
data FileSrcReq a where
  ReadFile :: FilePath -> FileSrcReq BS.ByteString
  ReadTextFile :: FilePath -> FileSrcReq String
  ListDir :: FilePath -> FileSrcReq (HashSet DirEntry)

-- | One entry returned by 'ListDir': a name and whether it's a file,
-- directory, etc.
data DirEntry = DirEntry
  { de_name :: String
  , de_type :: FileType
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Configuration for a 'FileSrc'. Build one with 'defaultFileSrcConfig' and
-- override individual fields as needed.
data FileSrcConfig = FileSrcConfig
  { fcsc_ident :: CompSrcInstanceId
  , fcsc_pollInterval :: TimeSpan
  , fcsc_clock :: Clock
  , fcsc_rootDir :: Option FilePath
  }

-- | A 'FileSrcConfig' with a 100ms poll interval, 'realClock', and no root
-- directory (paths are used as-is); only @fcsc_ident@ needs supplying.
defaultFileSrcConfig :: T.Text -> FileSrcConfig
defaultFileSrcConfig i =
  FileSrcConfig
    { fcsc_ident = CompSrcInstanceId i
    , fcsc_pollInterval = milliseconds 100
    , fcsc_clock = realClock
    , fcsc_rootDir = None
    }

-- | A running file source handle, holding the background file-watching
-- state. Register it with a 'Control.Computations.CompEngine.CompFlowRegistry.CompFlowRegistry'
-- to make it available to comp bodies.
data FileSrc = FileSrc
  { fcs_config :: FileSrcConfig
  , fcs_fileWatch :: FileWatch
  }

-- | Start a 'FileSrc' from a 'FileSrcConfig'. Prefer 'withFileSrc' unless
-- you need to manage its lifetime by hand.
initFileSrc :: FileSrcConfig -> IO FileSrc
initFileSrc cfg = do
  watch <- initFileWatch (fcsc_clock cfg) (fcsc_pollInterval cfg)
  pure (FileSrc cfg watch)

-- | Stop a 'FileSrc' started with 'initFileSrc', releasing its
-- file-watching resources.
closeFileSrc :: FileSrc -> IO ()
closeFileSrc fcs = do
  closeFileWatch (fcs_fileWatch fcs)

-- | Start a 'FileSrc', run @action@, and close it afterwards even if
-- @action@ throws.
withFileSrc :: FileSrcConfig -> (FileSrc -> IO a) -> IO a
withFileSrc cfg =
  bracket (initFileSrc cfg) closeFileSrc

-- | The canonicalized path used to identify a tracked file across polls.
newtype FileKey = FileKey {unFileKey :: CanonPath}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, LH.LargeHashable)

-- | A file's modification time, used to detect whether its content may have
-- changed since it was last read.
newtype FileVer = FileVer {unFileVer :: POSIXTime}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, LH.LargeHashable)

type FileDep = Dep FileKey (Option FileVer)

instance CompSrc FileSrc where
  type CompSrcReq FileSrc = FileSrcReq
  type CompSrcKey FileSrc = FileKey
  type CompSrcVer FileSrc = Option FileVer
  compSrcInstanceId = fcsc_ident . fcs_config
  compSrcExecute = executeImpl
  compSrcUnregister = unregisterImpl
  compSrcWaitChanges = waitChangesImpl

fileVerFromStatus :: FileStatus -> FileVer
fileVerFromStatus = FileVer . fs_mtime

waitChangesImpl :: FileSrc -> STM (HashSet FileDep)
waitChangesImpl fcs = do
  changes <- waitForFileChanges (fcs_fileWatch fcs)
  pure (HashSet.map mkInput changes)
 where
  mkInput :: FileChange -> FileDep
  mkInput c = Dep (FileKey (fc_path c)) (fmap fileVerFromStatus (fc_new c))

executeImpl
  :: forall a
   . FileSrc
  -> FileSrcReq a
  -> IO (HashSet FileDep, Fail a)
executeImpl fcs req =
  case req of
    ReadFile (qualify -> path) -> doWork path BS.readFile
    ReadTextFile (qualify -> path) -> doWork path readFile
    ListDir (qualify -> path) -> doWork path $ \p -> do
      l <- listDirectory p
      l2 <- forM l $ \name ->
        do
          s <- getFileStatus (path </> name)
          pure (DirEntry name (fs_type s))
      pure (HashSet.fromList l2)
 where
  qualify p =
    case fcsc_rootDir (fcs_config fcs) of
      None -> p
      Some d -> d </> p
  doWork :: FilePath -> (FilePath -> IO a) -> IO (HashSet FileDep, Fail a)
  doWork path getResult = do
    canonP <- watchFile (fcs_fileWatch fcs) path
    let action = do
          res <- getResult path
          status <- getFileStatus path
          pure (Ok res, Some (fileVerFromStatus status))
    (res, version) <- catch action $ \(e :: IOException) -> pure (Fail (show e), None)
    let deps = HashSet.singleton (Dep (FileKey canonP) version)
    pure (deps, res)

unregisterImpl :: FileSrc -> HashSet FileKey -> IO ()
unregisterImpl fcs keys = do
  forM_ (HashSet.toList keys) $ \k -> unwatchFile (fcs_fileWatch fcs) (unFileKey k)
