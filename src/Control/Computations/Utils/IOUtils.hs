{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | Small filesystem helpers layered on "System.Directory"\/"System.Posix":
 recursive directory listing, atomic file writes, a scratch temp directory
 for demos\/tests ('withSysTempDir'), and 'FileStatus'\/'CanonPath' -- the
 file metadata and canonical-path types 'Control.Computations.FlowImpls.FileSrc'
 tracks files by.
-}
module Control.Computations.Utils.IOUtils (
  listDirectoryWithQualifiedNames,
  listDirectoryRecursive,
  withSysTempDir,
  FileStatus (..),
  FileType (..),
  getFileStatus,
  safeGetFileStatus,
  CanonPath,
  unCanonPath,
  canonPath,
  writeFileAtomically,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.LargeHashable as LH
import Data.Maybe
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import System.Directory hiding (canonicalizePath)
import qualified System.Directory as Dir
import System.FilePath
import System.IO.Temp
import qualified System.Posix as Posix
import qualified System.Posix.Types as PosixTypes

listDirectoryWithQualifiedNames :: FilePath -> IO [FilePath]
listDirectoryWithQualifiedNames path = do
  content <- listDirectory path
  pure (map (path </>) content)

type ListRecAcc = (HashSet Posix.FileID, [(FilePath, FileStatus)])

listDirectoryRecursive :: FilePath -> IO [(FilePath, FileStatus)]
listDirectoryRecursive path =
  do
    (_, l) <- loop (HashSet.empty, []) path
    pure (reverse l)
 where
  loop acc path =
    do
      content <- listDirectory path
      foldM (handle path) acc content
  handle :: FilePath -> ListRecAcc -> String -> IO ListRecAcc
  handle dir (inodes, content) name =
    do
      let p = dir </> name
      stat <- getFileStatus p
      let newAcc = (HashSet.insert (fs_inode stat) inodes, (p, stat) : content)
      case fs_type stat of
        DirectoryFileType ->
          if (fs_inode stat `HashSet.member` inodes)
            then pure newAcc
            else loop newAcc p
        _ -> pure newAcc

-- | Create a fresh temporary directory under @\/tmp@, run @action@ with its
-- path, and remove it afterwards. Handy for demos and tests that need a
-- scratch directory to wire a 'Control.Computations.FlowImpls.FileSrc.FileSrc'\/
-- 'Control.Computations.FlowImpls.FileSink.FileSink' pair against.
withSysTempDir :: (FilePath -> IO a) -> IO a
withSysTempDir = withTempDirectory "/tmp" "IncComp_"

instance Hashable PosixTypes.CIno where
  hashWithSalt s (PosixTypes.CIno w64) = hashWithSalt s w64

instance Hashable PosixTypes.COff where
  hashWithSalt s (PosixTypes.COff i64) = hashWithSalt s i64

-- | The subset of a file's metadata this library tracks changes by: inode,
-- size, modification\/status-change time, and type. Deliberately excludes
-- access time, since that would make every read look like a change.
data FileStatus = FileStatus
  { fs_inode :: Posix.FileID
  , fs_size :: Posix.FileOffset
  , fs_mtime :: POSIXTime
  , fs_ctime :: POSIXTime
  , fs_type :: FileType
  -- no access time because we use FileStatus for tracking changes
  }
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | What kind of filesystem entry a path is.
data FileType
  = SocketFileType
  | NamedPipeFileType
  | SymbolicLinkFileType
  | BlockDeviceFileType
  | CharDeviceFileType
  | DirectoryFileType
  | RegularFileType
  | UnknownFileType
  deriving (Eq, Ord, Show, Generic, Hashable)

-- | Read a path's 'FileStatus'; throws like the underlying
-- 'Posix.getFileStatus' does if the path doesn't exist.
getFileStatus :: FilePath -> IO FileStatus
getFileStatus path = do
  s <- Posix.getFileStatus path
  pure $
    FileStatus
      { fs_inode = Posix.fileID s
      , fs_size = Posix.fileSize s
      , fs_mtime = Posix.modificationTimeHiRes s
      , fs_ctime = Posix.statusChangeTimeHiRes s
      , fs_type = getFileType s
      }
 where
  getFileType s =
    fromMaybe UnknownFileType $
      lookup
        True
        [ (Posix.isBlockDevice s, BlockDeviceFileType)
        , (Posix.isCharacterDevice s, CharDeviceFileType)
        , (Posix.isNamedPipe s, NamedPipeFileType)
        , (Posix.isRegularFile s, RegularFileType)
        , (Posix.isDirectory s, DirectoryFileType)
        , (Posix.isSymbolicLink s, SymbolicLinkFileType)
        , (Posix.isSocket s, SocketFileType)
        ]

safeGetFileStatus :: FilePath -> IO (Option FileStatus)
safeGetFileStatus path =
  do
    stat <- getFileStatus path
    pure (Some stat)
    `catch` \(_ :: IOException) -> pure None

-- | An absolute path in canonical form.
newtype CanonPath = CanonPath FilePath
  deriving stock (Show)
  deriving newtype (Hashable, Eq, Ord, LH.LargeHashable)

unCanonPath :: CanonPath -> FilePath
unCanonPath (CanonPath p) = p

canonPath :: FilePath -> IO CanonPath
canonPath p =
  do
    cp <- Dir.canonicalizePath p
    pure (CanonPath cp)

writeFileAtomically :: FilePath -> BS.ByteString -> IO ()
writeFileAtomically p bs =
  withTempFile (takeDirectory p) "IncComps" $ \tmp h -> do
    BS.hPutStr h bs
    renameFile tmp p
