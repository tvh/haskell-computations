module Control.Computations.Demos.DirSync.Main (
  syncDirs,
  syncDirs',
  RunStats (..),
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine
import Control.Computations.FlowImpls.CompLogging
import Control.Computations.FlowImpls.FileSink
import Control.Computations.FlowImpls.FileSrc
import Control.Computations.FlowImpls.IOSink
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Concurrent.STM
import Control.Monad
import qualified Data.ByteString as BS
import Data.HashSet (HashSet)
import System.Directory
import System.FilePath
import Prelude hiding (readFile, writeFile)

readFile :: TypedCompSrcId FileSrc -> FilePath -> CompM BS.ByteString
readFile fileSrcId p = compSrcReq fileSrcId (ReadFile p)

writeFile :: TypedCompSinkId FileSink -> FilePath -> BS.ByteString -> CompM ()
writeFile fileSinkId p bs = compSinkReq fileSinkId (WriteFile p bs)

listDir :: TypedCompSrcId FileSrc -> FilePath -> CompM (HashSet DirEntry)
listDir fileSrcId p = compSrcReq fileSrcId (ListDir p)

mkdir :: TypedCompSinkId FileSink -> FilePath -> CompM ()
mkdir fileSinkId p = compSinkReq fileSinkId (MakeDirs p)

fileSyncCompDef
  :: TypedCompSrcId FileSrc -> TypedCompSinkId FileSink -> FilePath -> CompDef FilePath ()
fileSyncCompDef fileSrcId fileSinkId src = defineComp "fileSyncComp" fullCaching $ \path ->
  do
    bs <- readFile fileSrcId (src </> path)
    writeFile fileSinkId path bs

dirSyncCompDef
  :: TypedCompSrcId FileSrc
  -> TypedCompSinkId FileSink
  -> FilePath
  -> Comp FilePath ()
  -> Comp FilePath ()
  -> CompDef FilePath ()
dirSyncCompDef fileSrcId fileSinkId src fileComp dirComp = defineComp "dirSyncComp" fullCaching $ \path ->
  do
    mkdir fileSinkId path
    contents <- listDir fileSrcId (src </> path)
    forM_ contents $ \entry ->
      case de_type entry of
        RegularFileType -> void $ evalComp fileComp (path </> de_name entry)
        DirectoryFileType ->
          do
            let subdir = path </> de_name entry
            void $ evalComp dirComp subdir
        t ->
          logDebugC
            ( "Ignoring directory entry "
                ++ de_name entry
                ++ " of "
                ++ path
                ++ " with type "
                ++ show t
            )

-- | Register the already-built flow instances (see 'syncDirs''), then hand
-- control to the given action.
withCompFlows :: FileSrc -> FileSink -> CompFlowRegistry -> IO a -> IO a
withCompFlows fileSrc fileSink reg action = do
  registerCompSrc reg fileSrc
  registerCompSink reg fileSink
  registerCompSink reg ioSink
  action

-- | Wired against the *actual* source\/sink ids (see 'syncDirs''), derived
-- from the live instances with 'typedCompSrcIdOf'\/'typedCompSinkIdOf'.
wireComps :: TypedCompSrcId FileSrc -> TypedCompSinkId FileSink -> FilePath -> CompWireM (Comp FilePath ())
wireComps fileSrcId fileSinkId src = do
  fileComp <- wireComp (fileSyncCompDef fileSrcId fileSinkId src)
  defineRecursiveComp (dirSyncCompDef fileSrcId fileSinkId src fileComp)

syncDirs :: FilePath -> FilePath -> IO ()
syncDirs src tgt =
  do
    runVar <- newTVarIO None
    syncDirs' runVar src tgt

-- | The FileSrc\/FileSink instances are built once here, up front -- their
-- ids (used by 'wireComps') and their registration (used by
-- 'withCompFlows') both derive from the very same instances, so each
-- instance's name is written down exactly once.
syncDirs' :: TVar (Option RunStats) -> FilePath -> FilePath -> IO ()
syncDirs' runVar src' tgt' = do
  src <- canonicalizePath src'
  tgt <- canonicalizePath tgt'
  withFileSrc (defaultFileSrcConfig "fileSrc") $ \fileSrc -> do
    fileSink <- makeFileSink "fileSink" tgt
    compDriver'
      runVar
      (withCompFlows fileSrc fileSink)
      (wireComps (typedCompSrcIdOf fileSrc) (typedCompSinkIdOf fileSink) src)
      "."
