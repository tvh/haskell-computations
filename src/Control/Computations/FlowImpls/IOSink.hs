{-# LANGUAGE TypeFamilies #-}

{- | A 'Control.Computations.CompEngine.CompSink.CompSink' that runs an
 arbitrary 'IO' action and reports whether it threw. Register 'ioSink' with a
 'Control.Computations.CompEngine.CompFlowRegistry.CompFlowRegistry' and call
 'unsafeCompIO' from a comp body to run non-deterministic or side-effecting
 'IO' that shouldn't be cached like a normal computation result.
-}
module Control.Computations.FlowImpls.IOSink (
  unsafeCompIO,
  ioSink,
) where

----------------------------------------
-- LOCAL
----------------------------------------
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.Types
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Exception
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Proxy
import Data.Void

-- | Run an arbitrary 'IO' action from within a comp body via the 'ioSink'
-- sink, catching any 'IOException' it throws as a 'Fail'. Marked "unsafe"
-- because, unlike a normal source request, the engine has no way to know
-- whether @action@ is deterministic or side-effecting -- use it deliberately
-- for the cases (logging, metrics, ad hoc side effects) where that's fine.
unsafeCompIO :: IO a -> CompM a
unsafeCompIO action = compSinkReq i (IOSinkReq action)
 where
  i = typedCompSinkId (Proxy @IOSink) ioSinkId

ioSinkId :: CompSinkInstanceId
ioSinkId = CompSinkInstanceId "IOSink"

-- | The 'IOSink' handle; register it once with a 'CompFlowRegistry'
-- (there's nothing to configure), then use 'unsafeCompIO' from comp bodies.
ioSink :: IOSink
ioSink = IOSink

data IOSink = IOSink

data IOSinkReq a where
  IOSinkReq :: IO a -> IOSinkReq a

instance CompSink IOSink where
  type CompSinkReq IOSink = IOSinkReq
  type CompSinkOut IOSink = Void
  compSinkInstanceId _ = ioSinkId
  compSinkExecute = executeWriteImpl
  compSinkDeleteOutputs _ _ = pure ()
  compSinkListExistingOutputs _ = Some (pure HashSet.empty)

executeWriteImpl :: IOSink -> IOSinkReq a -> IO (HashSet Void, Fail a)
executeWriteImpl IOSink (IOSinkReq action) = do
  res <-
    try action >>= \case
      Left (e :: IOException) -> pure (Fail (show e))
      Right x -> pure (Ok x)
  pure (HashSet.empty, res)
