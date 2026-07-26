module Control.Computations.Demos.Simple.Main (
  simpleMain,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine
import Control.Computations.FlowImpls.FileSink
import Control.Computations.FlowImpls.FileSrc
import Control.Computations.FlowImpls.IOSink
import Control.Computations.Utils.IOUtils
import Control.Computations.Utils.Logging

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad
import System.FilePath
import Prelude hiding (readFile, writeFile)

numberOfLinesCompDef :: TypedCompSrcId FileSrc -> CompDef FilePath Int
numberOfLinesCompDef fileSrcId =
  defineComp "numberOfLines" fullCaching $ \p -> do
    string <- compSrcReq fileSrcId (ReadTextFile p)
    pure (length (lines string))

sumCompDef
  :: TypedCompSrcId FileSrc
  -> Comp FilePath Int
  -> CompDef FilePath Int
sumCompDef fileSrcId c = defineComp "sum" fullCaching $ \p -> do
  string <- compSrcReq fileSrcId (ReadTextFile p)
  list <- mapM (evalCompOrFail c) (lines string)
  pure (sum list)

storeCompDef :: TypedCompSinkId FileSink -> Comp FilePath Int -> CompDef () ()
storeCompDef fileSinkId c = do
  defineComp "store" fullCaching $ \() -> do
    i <- evalCompOrFail c "file_list.txt"
    compSinkReq fileSinkId (WriteTextFile "output.txt" ("number of lines: " ++ show i))

-- | Wired against the *actual* source\/sink ids (see 'simpleMain'), derived
-- from the live instances with 'typedCompSrcIdOf'\/'typedCompSinkIdOf' --
-- there is no separately-typed-out instance name here that could drift
-- from what 'withCompFlows' registers.
wireAllComps :: TypedCompSrcId FileSrc -> TypedCompSinkId FileSink -> CompWireM (Comp () ())
wireAllComps fileSrcId fileSinkId = do
  numberOfLinesC <- wireComp (numberOfLinesCompDef fileSrcId)
  sumC <- wireComp (sumCompDef fileSrcId numberOfLinesC)
  wireComp (storeCompDef fileSinkId sumC)

-- | Register the already-built flow instances (see 'simpleMain'), then hand
-- control to the given action.
withCompFlows :: FileSrc -> FileSink -> CompFlowRegistry -> IO () -> IO ()
withCompFlows fileSrc fileSink reg action = do
  registerCompSrc reg fileSrc
  registerCompSink reg fileSink
  registerCompSink reg ioSink
  action

-- | Drive the wired computation graph forward until it settles. The
-- FileSrc\/FileSink instances are built once here, up front -- their ids
-- (used by 'wireAllComps') and their registration (used by
-- 'withCompFlows') both derive from the very same instances, so each
-- instance's name is written down exactly once.
simpleMain :: IO ()
simpleMain = withSysTempDir $ \tgt ->
  withFileSrc (defaultFileSrcConfig "fileSrc") $ \fileSrc -> do
    fileSink <- makeFileSink "fileSink" tgt
    logNote ("Target directory: " ++ tgt)
    compDriver
      (withCompFlows fileSrc fileSink)
      (wireAllComps (typedCompSrcIdOf fileSrc) (typedCompSinkIdOf fileSink))
      ()
