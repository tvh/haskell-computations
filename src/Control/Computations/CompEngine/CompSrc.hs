{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Computations.CompEngine.CompSrc (
  IsDep (..),
  IsDepConstraints,
  Dep (..),
  CompSrcDep,
  CompSrcDeps,
  CompSrc (..),
  SrcFetch (..),
  FlowConcurrency (..),
  CompSrcId,
  TypedCompSrcId,
  unTypedCompSrcId,
  compSrcId,
  typedCompSrcIdOf,
  unsafeMkTypedCompSrcId,
  instanceIdFromTypedCompSrcId,
  instTextFromTypedCompSrcId,
  CompSrcInstanceId (..),
  AnyCompSrcDep,
  SomeCompSrcDep (..),
  AnyCompSrcKey,
  SomeCompSrcKey (..),
  AnyCompSrcVer,
  SomeCompSrcVer (..),
  AnyCompSrc (..),
  wrapCompSrcDep,
) where

----------------------------------------
-- LOCAL
---------------------------------------

import Control.Computations.CompEngine.CompFlow
import Control.Computations.Utils.ConcUtils (trySync)
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
---------------------------------------
import Control.Concurrent.STM
import Control.Exception (SomeException)
import Control.Monad (forM_)
import Data.Data
import Data.HashSet
import Data.Hashable
import Data.Kind
import qualified Data.LargeHashable as LH
import Data.String
import qualified Data.Text as T
import GHC.Generics (Generic)

type IsDepConstraints a = (Show a, Eq a, Hashable a)

{- | A dependency @a@ that can be split into a key (identifying what's
 depended on) and a version (what state it was observed in).

 Load-bearing for this module's own 'Dep'\/'SomeCompSrcDep'\/'AnyCompSrcDep'
 instances below and for "Control.Computations.CompEngine.Types"'s
 'CompDep'\/'CompEngDep' instances.
-}
class
  ( IsDepConstraints a
  , IsDepConstraints (DepKey a)
  , IsDepConstraints (DepVer a)
  ) =>
  IsDep a
  where
  type DepKey a
  type DepVer a
  depKey :: a -> DepKey a
  depVer :: a -> DepVer a

data Dep a b = Dep
  { dep_key :: a
  , dep_ver :: b
  }
  deriving (Eq, Ord, Data, Typeable, Generic)

-- Hand-rolled rather than Generic-derived (anyclass): CompDep = Dep
-- CompDepKey CompDepVer sits on the DepSet hot path (CompEngDepComp wraps
-- CompDep, and CompDep's own Hashable instance newtype-delegates straight
-- to this one), so the Generic-representation walk here mattered too.
instance (Hashable a, Hashable b) => Hashable (Dep a b) where
  hashWithSalt s (Dep k v) = s `hashWithSalt` k `hashWithSalt` v

instance (Show k, Show v) => Show (Dep k v) where
  showsPrec p (Dep k v) = showHelper2 p "Dep" k v

instance (IsDepConstraints a, IsDepConstraints b) => IsDep (Dep a b) where
  type DepKey (Dep a b) = a
  type DepVer (Dep a b) = b
  depKey = dep_key
  depVer = dep_ver

type CompSrcDep s = Dep (CompSrcKey s) (CompSrcVer s)
type CompSrcDeps s = HashSet (CompSrcDep s)

newtype CompSrcInstanceId = CompSrcInstanceId {unCompSrcInstanceId :: T.Text}
  deriving (Eq, Ord, Generic)

instance Show CompSrcInstanceId where
  showsPrec p (CompSrcInstanceId t) = showHelper1 p "CompSrcInstanceId" t

instance Hashable CompSrcInstanceId
instance LH.LargeHashable CompSrcInstanceId

instance IsString CompSrcInstanceId where
  fromString = CompSrcInstanceId . T.pack

-- | Whether the engine may have several 'compSrcExecute' calls to a given
-- 'CompSrc' instance in flight at once.
data FlowConcurrency = FlowSerial | FlowConcurrent
  deriving (Eq, Show)

{- | One request queued as part of a same-instance batch (see
 'compSrcExecuteBatch'), paired with the callback that must receive its own
 result. A batch mixes together requests with different result types --
 'CompSrcReq s a' is indexed by @a@, so a plain @['CompSrcReq' s a]@ would not
 typecheck for a batch spanning more than one @a@ -- so, following Haxl's
 @BlockedFetch@ (Marlow et al, "There is no Fork", ICFP'14), each request's
 own @a@ is hidden behind this existential, alongside the callback that
 already knows how to consume that same @a@. The callback takes an
 'Either' 'SomeException' rather than a bare result so a batched
 implementation can report one request's failure without having to fail
 (or fake a result for) every other request sharing the call -- see
 'compSrcExecuteBatch's default for the base case, and
 "Control.Computations.CompEngine.Impl" for how the engine uses this to
 keep its leftmost-failing-leaf-wins guarantee across a bundled group.
-}
data SrcFetch s
  = forall a. SrcFetch (CompSrcReq s a) (Either SomeException (CompSrcDeps s, Fail a) -> IO ())

class (Typeable s, IsCompFlowData (CompSrcKey s), IsCompFlowData (CompSrcVer s)) => CompSrc s where
  type CompSrcReq s :: Type -> Type
  type CompSrcKey s :: Type
  type CompSrcVer s :: Type
  compSrcInstanceId :: s -> CompSrcInstanceId
  compSrcExecute :: s -> CompSrcReq s a -> IO (CompSrcDeps s, Fail a)
  compSrcUnregister :: s -> HashSet (CompSrcKey s) -> IO ()
  compSrcWaitChanges :: s -> STM (CompSrcDeps s)

  -- | Whether the engine may have several 'compSrcExecute' calls to this
  -- instance in flight at once. Defaults to 'FlowSerial', under which the
  -- engine runs this source's requests on its own thread, one at a time.
  compSrcConcurrency :: s -> FlowConcurrency
  compSrcConcurrency _ = FlowSerial

  {- | Execute every request the engine has bundled together against this
   one instance -- everything a comp body's applicative batch asked of this
   source, gathered into a single round trip -- rather than one
   'compSrcExecute' call per request. Defaults to running each request in
   turn via 'compSrcExecute', catching its own synchronous exception into
   its own callback (so a request's failure never prevents its siblings in
   the same batch from reporting their own results -- see 'SrcFetch'):
   exactly the pre-batching behaviour, so an existing 'CompSrc' instance
   defined outside this repo keeps compiling, and keeps behaving exactly as
   it always has, without ever needing to know this method exists.

   Override this when a genuinely cheaper batched round trip is available --
   one @IN (...)@ query instead of N point lookups against a real database,
   one lock acquisition instead of N against an in-memory store, one
   simulated service call instead of N -- see
   "Control.Computations.FlowImpls.HashMapFlow" for a worked (in-memory)
   example, and "Control.Computations.Demos.Bench.SystemSrc" for one with
   simulated latency, where collapsing N delays into one is the entire
   point.

   The engine bundles every 'CompLeafSrc' leaf that resolves to the same
   instance within one applicative batch and calls this exactly once for
   the whole group -- see "Control.Computations.CompEngine.Impl"'s
   @SrcGroup@\/@runGroupOnce@ -- regardless of concurrency width: bundling
   needs no worker thread, so it applies even at width 1, and even to a
   'FlowSerial' instance (which still only ever sees one caller of this
   method at a time, exactly as it does for 'compSrcExecute').
  -}
  compSrcExecuteBatch :: s -> [SrcFetch s] -> IO ()
  compSrcExecuteBatch s fs =
    forM_ fs $ \(SrcFetch req write) -> trySync (compSrcExecute s req) >>= write

data CompSrcId = CompSrcId
  { csi_type :: TypeId
  , csi_instance :: CompSrcInstanceId
  }
  deriving (Eq, Generic)

instance Show CompSrcId where
  showsPrec p (CompSrcId (TypeId t) (CompSrcInstanceId i)) =
    showHelper2 p "CompSrcId" t i

instance Hashable CompSrcId
instance LH.LargeHashable CompSrcId

newtype TypedCompSrcId a = TypedCompSrcId {unTypedCompSrcId :: CompSrcId}
  deriving (Eq, Generic)

instance Show (TypedCompSrcId a) where
  showsPrec p (TypedCompSrcId (CompSrcId (TypeId t) (CompSrcInstanceId i))) =
    showHelper2 p "TypedCompSrcId" t i

instanceIdFromTypedCompSrcId :: TypedCompSrcId a -> CompSrcInstanceId
instanceIdFromTypedCompSrcId = csi_instance . unTypedCompSrcId

instTextFromTypedCompSrcId :: TypedCompSrcId a -> T.Text
instTextFromTypedCompSrcId = unCompSrcInstanceId . instanceIdFromTypedCompSrcId

-- | Not exported from the public "Control.Computations.CompEngine" facade
-- (see that module's import of this one) -- kept here, and still exported
-- from this internal module, purely as a shared helper for the ~15 call
-- sites elsewhere in the library (logging, 'CompFlowRegistry' hash-map
-- keys) that need the untyped id of a live instance. Library-external code
-- should go through 'typedCompSrcIdOf' and keep the typed id, or
-- 'unTypedCompSrcId' it themselves if the untyped form is genuinely needed.
compSrcId :: forall s. CompSrc s => s -> CompSrcId
compSrcId = unTypedCompSrcId . typedCompSrcIdOf

{- | Derive a source's typed id directly from a live instance.

 This is the preferred way to get a 'TypedCompSrcId' whenever an instance
 is already in scope: unlike 'unsafeMkTypedCompSrcId', the id can't drift
 from what the instance itself reports via 'compSrcInstanceId', because
 it's computed from that instance.
-}
typedCompSrcIdOf :: forall s. CompSrc s => s -> TypedCompSrcId s
typedCompSrcIdOf s = unsafeMkTypedCompSrcId (Proxy @s) (compSrcInstanceId s)

{- | Build a source's typed id from a bare instance name, with no instance
 to check it against.

 __Unsafe__: the name is unchecked. A typo, or an instance name that has
 simply drifted from what the actual 'CompSrc' instance will report via
 'compSrcInstanceId' (e.g. because the config passed to that instance's
 constructor was given a different string), still typechecks fine here --
 it only surfaces later, at runtime, as a registry-lookup miss (a 'Fail')
 when a comp body sends a request against the resulting id. Prefer
 'typedCompSrcIdOf' whenever a live instance is in scope; reach for this
 only when the id is genuinely needed before any instance exists, e.g. a
 top-level id referenced by a comp body at wiring time.
-}
unsafeMkTypedCompSrcId :: CompSrc s => Proxy s -> CompSrcInstanceId -> TypedCompSrcId s
unsafeMkTypedCompSrcId p instId =
  let i = CompSrcId (identifyProxy p) instId
   in TypedCompSrcId i

newtype SomeCompSrcDep s = SomeCompSrcDep {unSomeCompSrcDep :: (CompSrcDep s)}
deriving newtype instance CompSrc s => Show (SomeCompSrcDep s)
deriving newtype instance CompSrc s => Eq (SomeCompSrcDep s)
deriving newtype instance CompSrc s => Hashable (SomeCompSrcDep s)

-- deriving newtype instance CompSrc s => LH.LargeHashable (SomeCompSrcDep s)
type AnyCompSrcDep = ForAnyCompFlow CompSrc CompSrcId SomeCompSrcDep

newtype SomeCompSrcKey s = SomeCompSrcKey {unSomeCompSrcKey :: (CompSrcKey s)}
deriving newtype instance CompSrc s => Show (SomeCompSrcKey s)
deriving newtype instance CompSrc s => Eq (SomeCompSrcKey s)
deriving newtype instance CompSrc s => Hashable (SomeCompSrcKey s)

-- deriving newtype instance CompSrc s => LH.LargeHashable (SomeCompSrcKey s)
type AnyCompSrcKey = ForAnyCompFlow CompSrc CompSrcId SomeCompSrcKey

newtype SomeCompSrcVer s = SomeCompSrcVer {unSomeCompSrcVer :: (CompSrcVer s)}
deriving newtype instance CompSrc s => Show (SomeCompSrcVer s)
deriving newtype instance CompSrc s => Eq (SomeCompSrcVer s)
deriving newtype instance CompSrc s => Hashable (SomeCompSrcVer s)

-- deriving newtype instance CompSrc s => LH.LargeHashable (SomeCompSrcVer s)
type AnyCompSrcVer = ForAnyCompFlow CompSrc CompSrcId SomeCompSrcVer

instance CompSrc s => IsDep (SomeCompSrcDep s) where
  type DepKey (SomeCompSrcDep s) = SomeCompSrcKey s
  type DepVer (SomeCompSrcDep s) = SomeCompSrcVer s
  depKey (SomeCompSrcDep d) = SomeCompSrcKey (dep_key d)
  depVer (SomeCompSrcDep d) = SomeCompSrcVer (dep_ver d)

instance IsDep AnyCompSrcDep where
  type DepKey AnyCompSrcDep = AnyCompSrcKey
  type DepVer AnyCompSrcDep = AnyCompSrcVer
  depKey (ForAnyCompFlow i p d) = ForAnyCompFlow i p (depKey d)
  depVer (ForAnyCompFlow i p d) = ForAnyCompFlow i p (depVer d)

wrapCompSrcDep :: forall s. CompSrc s => s -> CompSrcDep s -> AnyCompSrcDep
wrapCompSrcDep s d =
  ForAnyCompFlow (compSrcId s) (Proxy @s) (SomeCompSrcDep d)

data AnyCompSrc = forall s. CompSrc s => AnyCompSrc s
