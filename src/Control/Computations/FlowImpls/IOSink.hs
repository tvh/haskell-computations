{-# LANGUAGE TypeFamilies #-}

{- | The sanctioned escape hatch from 'CompM' into 'IO'.

 'CompM' deliberately has no @MonadIO@\/@liftIO@ instance, and the public
 "Control.Computations.CompEngine" facade re-exports it /abstractly/ (hiding
 its constructor), precisely so that a comp body cannot construct a 'CompM'
 value by hand and smuggle arbitrary 'IO' into the engine.
 'unsafeCompIO', running through this module's 'ioSink'
 'Control.Computations.CompEngine.CompSink.CompSink', is the one sanctioned
 route through that wall.

 Register 'ioSink' with a
 'Control.Computations.CompEngine.CompFlowRegistry.CompFlowRegistry' --
 there is nothing to configure beyond a call to
 'Control.Computations.CompEngine.CompFlowRegistry.registerCompSink':

 > registerCompSink reg ioSink

 -- before any comp body calls 'unsafeCompIO'. See 'unsafeCompIO' for what
 "unsafe" actually costs and why registration is not optional.
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

{- | Run an arbitrary 'IO' action from within a comp body via the 'ioSink'
 sink, catching any 'IOException' it throws as a 'Fail'.

 Marked "unsafe" for three reasons, each worth knowing before reaching for
 it:

 * __It is invisible to the engine's dependency tracking and output GC.__
   Unlike a 'Control.Computations.CompEngine.CompSrc.CompSrc' request, the
   engine has no way to know whether @action@ is deterministic, what it
   read, or what it produced, so it can neither invalidate nor reclaim
   anything on its account. That's the right tradeoff for fire-and-forget
   effects (logging, metrics, notifications); it is the wrong tool if the
   effect produces state a later computation needs to depend on -- model
   that as a proper 'Control.Computations.CompEngine.CompSrc.CompSrc'\/
   'Control.Computations.CompEngine.CompSink.CompSink' pair instead.

 * __Effects are subject to early cutoff.__ If a computation doesn't rerun
   because its inputs are unchanged, nothing inside its body runs --
   including this call. The absence of an effect therefore means "this
   computation did not rerun", not "the effect was skipped while the
   computation still ran". This tends to surprise people using
   'unsafeCompIO' to trace behaviour.

 * __Registration is mandatory, not optional.__ Calling this without having
   first registered 'ioSink'
   (@'Control.Computations.CompEngine.CompFlowRegistry.registerCompSink' reg
   'ioSink'@) fails the computation outright:
   'Control.Computations.CompEngine.CompFlowRegistry.withCompSinkId' returns
   a 'Fail' when the sink id isn't in the registry, it does not silently
   no-op. Every in-repo demo registers 'ioSink' once at startup; do the same
   before any comp body calls 'unsafeCompIO'.
-}
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
