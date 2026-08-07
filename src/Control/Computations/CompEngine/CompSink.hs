{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Control.Computations.CompEngine.CompSink (
  CompSinkOuts,
  CompSink (..),
  CompSinkInstanceId (..),
  CompSinkId,
  TypedCompSinkId,
  unTypedCompSinkId,
  compSinkId,
  typedCompSinkIdOf,
  unsafeMkTypedCompSinkId,
  instanceIdFromTypedCompSinkId,
  instTextFromTypedCompSinkId,
  SomeCompSinkOuts (..),
  AnyCompSinkOuts,
  AnyCompSinkOutsMap (..),
  AnyCompSink (..),
  nullAnyOuts,
  sizeAnyOuts,
  nullAnyOutsMap,
  sizeAnyOutsMap,
  mapAnyOutsMap,
  anyOutsMap,
  diffAnyOutsMap,
  filterAnyOutsMap,
  compSinkOutsFromAny,
  wrapCompSinkOuts,
  unionAnyCompSinkOutsMap,
  unionsAnyCompSinkOutsMap,
  emptyAnyCompOutSinksMap,
  anyOutsMapToList,
) where

----------------------------------------
-- LOCAL
---------------------------------------

import Control.Computations.CompEngine.CompFlow
import Control.Computations.CompEngine.CompSrc (FlowConcurrency (..))
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
---------------------------------------

import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import Data.Kind
import qualified Data.LargeHashable as LH
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.String
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics (Generic)

type CompSinkOuts s = HashSet (CompSinkOut s)

newtype CompSinkInstanceId = CompSinkInstanceId {unCompSinkInstanceId :: T.Text}
  deriving (Eq, Ord, Generic)

instance Show CompSinkInstanceId where
  showsPrec p (CompSinkInstanceId t) = showHelper1 p "CompSinkInstanceId" t

instance Hashable CompSinkInstanceId
instance LH.LargeHashable CompSinkInstanceId

instance IsString CompSinkInstanceId where
  fromString = CompSinkInstanceId . T.pack

{- | The 'SomeCompSinkOuts' superclass below is redundant for *correctness*
 with the 'CompSinkOut' one right next to it -- 'SomeCompSinkOuts'\'s
 'Show'\/'Eq'\/'Hashable' instances (declared further down via
 @deriving newtype@) hold precisely when 'CompSinkOuts'\'s do. It is not
 redundant for *performance*, and that is the only reason it exists: see
 docs/benchmark-notes.md Stage 16, which diagnosed the identical shape in
 "Control.Computations.CompEngine.CompSrc" and fixed it the same way.

 Without it, the only way to prove 'IsCompFlowData (SomeCompSinkOuts s)' at
 a use site such as 'ForAnyCompFlow'\'s constructor (reached via
 'wrapCompSinkOuts') is to apply the @deriving newtype@ dictionary function
 to the 'CompSink s' dictionary already in scope there. That application
 can't float to top level -- it's applied to a lambda-bound argument, not a
 CAF -- so it reruns at every wrap site, and GeneralizedNewtypeDeriving
 builds its result record one method at a time, leaving one unforced thunk
 per class method behind on every call. Declaring the constraint as a
 superclass instead turns it into a dictionary *field*: resolving it is a
 single record selection returning the same shared object every time,
 built once when the concrete @instance CompSink Foo@ dictionary CAF
 itself is built. A future reader who sees this next to the 'CompSinkOut'
 superclass and assumes it's a duplicate should delete it only after
 re-reading Stage 16.
-}
class (Typeable s, IsCompFlowData (CompSinkOut s), IsCompFlowData (SomeCompSinkOuts s)) => CompSink s where
  type CompSinkReq s :: Type -> Type
  type CompSinkOut s :: Type
  compSinkInstanceId :: s -> CompSinkInstanceId
  compSinkExecute :: s -> CompSinkReq s a -> IO (CompSinkOuts s, Fail a)
  compSinkDeleteOutputs :: s -> CompSinkOuts s -> IO ()

  -- not all sinks support listing the existing outputs
  compSinkListExistingOutputs :: s -> Option (IO (CompSinkOuts s))

  {- | Whether the engine may have several 'compSinkExecute' calls to this
   instance in flight at once. Defaults to 'FlowSerial', mirroring
   'Control.Computations.CompEngine.CompSrc.compSrcConcurrency'\'s own
   default exactly (see that method's haddock): under it, the engine runs
   this sink's requests one at a time. A defaulted method, so no existing
   'CompSink' instance defined outside this repo needs to know this exists
   in order to keep compiling and keep behaving exactly as it always has.

   Unlike 'Control.Computations.CompEngine.CompSrc.CompSrc', there is no
   batching concept for sinks -- every 'compSinkExecute' call is its own
   round trip -- so this is the only concurrency knob a 'CompSink' instance
   has to declare. See "Control.Computations.CompEngine.CompFlowRegistry"'s
   @withSinkInstLock@ for where this is actually consulted, and only when
   parallel eval is enabled at all (a width-1 engine never even looks at
   this).
  -}
  compSinkConcurrency :: s -> FlowConcurrency
  compSinkConcurrency _ = FlowSerial

data CompSinkId = CompSinkId
  { csi_type :: TypeId
  , csi_instance :: CompSinkInstanceId
  }
  deriving (Eq, Ord, Generic)

instance Show CompSinkId where
  showsPrec p (CompSinkId (TypeId t) (CompSinkInstanceId i)) =
    showHelper2 p "CompSinkId" t i

instance Hashable CompSinkId
instance LH.LargeHashable CompSinkId

newtype TypedCompSinkId a = TypedCompSinkId {unTypedCompSinkId :: CompSinkId}
  deriving (Eq, Generic)

instance Show (TypedCompSinkId a) where
  showsPrec p (TypedCompSinkId (CompSinkId (TypeId t) (CompSinkInstanceId i))) =
    showHelper2 p "TypedCompSinkId" t i

-- | Not exported from the public "Control.Computations.CompEngine" facade
-- (see that module's import of this one) -- kept here, and still exported
-- from this internal module, purely as a shared helper for the handful of
-- call sites elsewhere in the library (logging, 'CompFlowRegistry' hash-map
-- keys) that need the untyped id of a live instance. Library-external code
-- should go through 'typedCompSinkIdOf' and keep the typed id, or
-- 'unTypedCompSinkId' it themselves if the untyped form is genuinely
-- needed.
compSinkId :: forall s. CompSink s => s -> CompSinkId
compSinkId = unTypedCompSinkId . typedCompSinkIdOf

{- | Derive a sink's typed id directly from a live instance.

 This is the preferred way to get a 'TypedCompSinkId' whenever an instance
 is already in scope: unlike 'unsafeMkTypedCompSinkId', the id can't drift
 from what the instance itself reports via 'compSinkInstanceId', because
 it's computed from that instance.
-}
typedCompSinkIdOf :: forall s. CompSink s => s -> TypedCompSinkId s
typedCompSinkIdOf s = unsafeMkTypedCompSinkId (Proxy @s) (compSinkInstanceId s)

{- | Build a sink's typed id from a bare instance name, with no instance to
 check it against.

 __Unsafe__: the name is unchecked. A typo, or an instance name that has
 simply drifted from what the actual 'CompSink' instance will report via
 'compSinkInstanceId' (e.g. because the config passed to that instance's
 constructor was given a different string), still typechecks fine here --
 it only surfaces later, at runtime, as a registry-lookup miss (a 'Fail')
 when a comp body sends a request against the resulting id. Prefer
 'typedCompSinkIdOf' whenever a live instance is in scope; reach for this
 only when the id is genuinely needed before any instance exists, e.g. a
 top-level id referenced by a comp body at wiring time.
-}
unsafeMkTypedCompSinkId :: CompSink s => Proxy s -> CompSinkInstanceId -> TypedCompSinkId s
unsafeMkTypedCompSinkId p instId =
  let i = CompSinkId (identifyProxy p) instId
   in TypedCompSinkId i

instanceIdFromTypedCompSinkId :: TypedCompSinkId a -> CompSinkInstanceId
instanceIdFromTypedCompSinkId = csi_instance . unTypedCompSinkId

instTextFromTypedCompSinkId :: TypedCompSinkId a -> T.Text
instTextFromTypedCompSinkId = unCompSinkInstanceId . instanceIdFromTypedCompSinkId

data AnyCompSink = forall s. CompSink s => AnyCompSink s

newtype SomeCompSinkOuts s = SomeCompSinkOuts {unSomeCompSinkOut :: (CompSinkOuts s)}
deriving newtype instance Show (CompSinkOuts s) => Show (SomeCompSinkOuts s)
deriving newtype instance Eq (CompSinkOuts s) => Eq (SomeCompSinkOuts s)
deriving newtype instance Hashable (CompSinkOuts s) => Hashable (SomeCompSinkOuts s)

map2SomeCompSinkOuts
  :: (CompSinkOuts s -> CompSinkOuts s -> CompSinkOuts s)
  -> SomeCompSinkOuts s
  -> SomeCompSinkOuts s
  -> SomeCompSinkOuts s
map2SomeCompSinkOuts f (SomeCompSinkOuts x) (SomeCompSinkOuts y) =
  SomeCompSinkOuts (f x y)

type AnyCompSinkOuts = ForAnyCompFlow CompSink CompSinkId SomeCompSinkOuts

newtype AnyCompSinkOutsMap = AnyCompSinkOutsMap {unAnyCompSinkOutsMap :: Map CompSinkId AnyCompSinkOuts}
  deriving (Eq, Show)

anyOutsMapToList :: AnyCompSinkOutsMap -> [(CompSinkId, AnyCompSinkOuts)]
anyOutsMapToList (AnyCompSinkOutsMap m) = Map.toList m

unionAnyCompSinkOutsMap :: AnyCompSinkOutsMap -> AnyCompSinkOutsMap -> AnyCompSinkOutsMap
unionAnyCompSinkOutsMap (AnyCompSinkOutsMap m1) (AnyCompSinkOutsMap m2) =
  AnyCompSinkOutsMap $ Map.unionWith f m1 m2
 where
  f :: AnyCompSinkOuts -> AnyCompSinkOuts -> AnyCompSinkOuts
  f (ForAnyCompFlow i1 p1 (SomeCompSinkOuts s1)) (ForAnyCompFlow _i2 p2 (SomeCompSinkOuts s2)) =
    case cast s2 of
      Just s2' -> ForAnyCompFlow i1 p1 (SomeCompSinkOuts (HashSet.union s1 s2'))
      Nothing ->
        error
          ( "There are comp sinks with the same ID "
              ++ show i1
              ++ " but of different types "
              ++ show p1
              ++ " and "
              ++ show p2
          )

unionsAnyCompSinkOutsMap :: [AnyCompSinkOutsMap] -> AnyCompSinkOutsMap
unionsAnyCompSinkOutsMap = mconcat

instance Semigroup AnyCompSinkOutsMap where
  (<>) = unionAnyCompSinkOutsMap

instance Monoid AnyCompSinkOutsMap where
  mempty = emptyAnyCompOutSinksMap

emptyAnyCompOutSinksMap :: AnyCompSinkOutsMap
emptyAnyCompOutSinksMap = AnyCompSinkOutsMap Map.empty

nullAnyOuts :: AnyCompSinkOuts -> Bool
nullAnyOuts (ForAnyCompFlow _ _ (SomeCompSinkOuts set)) = HashSet.null set

nullAnyOutsMap :: AnyCompSinkOutsMap -> Bool
nullAnyOutsMap (AnyCompSinkOutsMap m) = all nullAnyOuts (Map.elems m)

sizeAnyOuts :: AnyCompSinkOuts -> Int
sizeAnyOuts (ForAnyCompFlow _ _ (SomeCompSinkOuts set)) = HashSet.size set

sizeAnyOutsMap :: AnyCompSinkOutsMap -> Int
sizeAnyOutsMap (AnyCompSinkOutsMap m) = sum $ map sizeAnyOuts (Map.elems m)

anyOutsMap
  :: (forall s. CompSink s => Proxy s -> CompSinkOut s -> Bool)
  -> AnyCompSinkOutsMap
  -> Bool
anyOutsMap pred m =
  let bools = mapAnyOutsMap pred m
   in or bools

mapAnyOutsMap
  :: forall a
   . (forall s. CompSink s => Proxy s -> CompSinkOut s -> a)
  -> AnyCompSinkOutsMap
  -> [a]
mapAnyOutsMap f (AnyCompSinkOutsMap m) =
  concatMap (\(ForAnyCompFlow _ _ x) -> g x) (Map.elems m)
 where
  g :: forall s. CompSink s => SomeCompSinkOuts s -> [a]
  g (SomeCompSinkOuts outs) =
    map (f (Proxy :: Proxy s)) (HashSet.toList outs)

diffAnyOutsMap :: AnyCompSinkOutsMap -> AnyCompSinkOutsMap -> AnyCompSinkOutsMap
diffAnyOutsMap (AnyCompSinkOutsMap map1) (AnyCompSinkOutsMap map2) =
  AnyCompSinkOutsMap $ Map.filter (not . nullAnyOuts) combinedMap
 where
  combinedMap = Map.difference map1 map2 `Map.union` Map.intersectionWith diff map1 map2
  diff :: AnyCompSinkOuts -> AnyCompSinkOuts -> AnyCompSinkOuts
  diff x@(ForAnyCompFlow i _ _) y =
    applyForEqualIds x y $ \p outs1 outs2 ->
      ForAnyCompFlow i p (map2SomeCompSinkOuts HashSet.difference outs1 outs2)

filterAnyOutsMap
  :: (forall s. CompSink s => Proxy s -> CompSinkId -> CompSinkOut s -> Bool)
  -> AnyCompSinkOutsMap
  -> AnyCompSinkOutsMap
filterAnyOutsMap pred (AnyCompSinkOutsMap m) =
  AnyCompSinkOutsMap $
    Map.mapMaybe (\(ForAnyCompFlow i p x) -> fmap (ForAnyCompFlow i p) (g i x)) m
 where
  g :: forall s. CompSink s => CompSinkId -> SomeCompSinkOuts s -> Maybe (SomeCompSinkOuts s)
  g i (SomeCompSinkOuts outs) =
    let newSet = HashSet.filter (pred (Proxy :: Proxy s) i) outs
     in if HashSet.null newSet
          then Nothing
          else Just (SomeCompSinkOuts newSet)

compSinkOutsFromAny
  :: forall s. CompSink s => Proxy s -> CompSinkId -> AnyCompSinkOutsMap -> Maybe (CompSinkOuts s)
compSinkOutsFromAny pExpected i (AnyCompSinkOutsMap m) =
  case Map.lookup i m of
    Nothing -> Nothing
    Just (ForAnyCompFlow _ pReal someOuts) ->
      case cast someOuts of
        Just (someOuts' :: SomeCompSinkOuts s) -> Just (unSomeCompSinkOut someOuts')
        Nothing ->
          error
            ( "AnyCompSinkOutsMap contains an entry for comp sink "
                ++ show i
                ++ " of unexpected type "
                ++ show pReal
                ++ ". Excepted type: "
                ++ show pExpected
            )

wrapCompSinkOuts :: forall s. CompSink s => s -> CompSinkOuts s -> AnyCompSinkOutsMap
wrapCompSinkOuts s xs =
  AnyCompSinkOutsMap $
    Map.singleton key (ForAnyCompFlow key (Proxy @s) (SomeCompSinkOuts xs))
 where
  key = compSinkId s
