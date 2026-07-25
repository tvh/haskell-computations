{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

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

data FileSrcReq a where
  ReadFile :: FilePath -> FileSrcReq BS.ByteString
  ReadTextFile :: FilePath -> FileSrcReq String
  ListDir :: FilePath -> FileSrcReq (HashSet DirEntry)

data DirEntry = DirEntry
  { de_name :: String
  , de_type :: FileType
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

data FileSrcConfig = FileSrcConfig
  { fcsc_ident :: CompSrcInstanceId
  , fcsc_pollInterval :: TimeSpan
  , fcsc_clock :: Clock
  , fcsc_rootDir :: Option FilePath
  }

defaultFileSrcConfig :: T.Text -> FileSrcConfig
defaultFileSrcConfig i =
  FileSrcConfig
    { fcsc_ident = CompSrcInstanceId i
    , fcsc_pollInterval = milliseconds 100
    , fcsc_clock = realClock
    , fcsc_rootDir = None
    }

data FileSrc = FileSrc
  { fcs_config :: FileSrcConfig
  , fcs_fileWatch :: FileWatch
  }

initFileSrc :: FileSrcConfig -> IO FileSrc
initFileSrc cfg = do
  watch <- initFileWatch (fcsc_clock cfg) (fcsc_pollInterval cfg)
  pure (FileSrc cfg watch)

closeFileSrc :: FileSrc -> IO ()
closeFileSrc fcs = do
  closeFileWatch (fcs_fileWatch fcs)

withFileSrc :: FileSrcConfig -> (FileSrc -> IO a) -> IO a
withFileSrc cfg =
  bracket (initFileSrc cfg) closeFileSrc

newtype FileKey = FileKey {unFileKey :: CanonPath}
  deriving stock (Eq, Ord, Show)
  deriving newtype (Hashable, LH.LargeHashable)

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
