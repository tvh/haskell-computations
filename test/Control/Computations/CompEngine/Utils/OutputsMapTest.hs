{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Control.Computations.CompEngine.Utils.OutputsMapTest (
  htf_thisModulesTests,
) where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompFlow
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.Utils.OutputsMap
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Data.Foldable
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable
import qualified Data.Map.Strict as Map
import Data.Proxy
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics (Generic)
import Test.Framework
import qualified Test.QuickCheck as QC
import Prelude hiding (lookup)

newtype CompSinkForTestingOutputs = CompSinkForTestingOutputs
  { csfto_ident :: CompSinkInstanceId
  }
  deriving (Show, Eq, Ord, Generic, Typeable)

instance Hashable CompSinkForTestingOutputs

data CompSinkForTestingOutputsReq a

instance CompSink CompSinkForTestingOutputs where
  type CompSinkReq CompSinkForTestingOutputs = CompSinkForTestingOutputsReq
  type CompSinkOut CompSinkForTestingOutputs = T.Text
  compSinkInstanceId = csfto_ident
  compSinkExecute _ _ = fail "CompSinkForTestingOutputs actions do not exist"
  compSinkDeleteOutputs _ _ = pure ()
  compSinkListExistingOutputs _ = None

data ToyOutput = ToyOutput
  { to_dataIf :: CompSinkForTestingOutputs
  , to_key :: T.Text
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable ToyOutput

instance QC.Arbitrary ToyOutput where
  arbitrary = QC.elements allPossibleToyOutputs

type ToyForwardMap k = HashMap.HashMap k (HashSet ToyOutput)

-- Computationally inefficient but simple enough to be obviously correct implementation of
-- (a special case with simpler types of) an output map.
newtype ToyOutputsMap k = ToyOutputsMap
  { tom_forward :: ToyForwardMap k
  }
  deriving (Eq, Show)

toy_forwardMap :: ToyOutputsMap k -> ToyForwardMap k
toy_forwardMap = tom_forward

toy_lookup :: (Hashable k) => k -> ToyOutputsMap k -> Maybe (HashSet ToyOutput)
toy_lookup key = HashMap.lookup key . toy_forwardMap

toy_lookupOutputKey :: (Hashable k) => ToyOutput -> ToyOutputsMap k -> HashSet k
toy_lookupOutputKey outputKey tom =
  HashSet.fromList $
    map fst $
      filter (any (\output -> output == outputKey) . snd) $
        HashMap.toList $
          toy_forwardMap tom

toy_fromForwardMap :: ToyForwardMap k -> ToyOutputsMap k
toy_fromForwardMap m = ToyOutputsMap{tom_forward = m}

toy_insert :: (Hashable k) => k -> HashSet ToyOutput -> ToyOutputsMap k -> ToyOutputsMap k
toy_insert key outputs tom =
  ToyOutputsMap{tom_forward = HashMap.insert key outputs (tom_forward tom)}

toy_delete :: (Hashable k) => k -> ToyOutputsMap k -> ToyOutputsMap k
toy_delete key tom =
  ToyOutputsMap{tom_forward = HashMap.delete key (tom_forward tom)}

testOutputs :: CompSinkForTestingOutputs -> [T.Text] -> AnyCompSinkOuts
testOutputs sink outputs =
  let i = unTypedCompSinkId (typedCompSinkIdOf sink)
      outputSet :: SomeCompSinkOuts CompSinkForTestingOutputs
      outputSet = SomeCompSinkOuts (HashSet.fromList outputs)
   in ForAnyCompFlow i (Proxy @CompSinkForTestingOutputs) outputSet

toy_outputsToReal :: HashSet ToyOutput -> AnyCompSinkOutsMap
toy_outputsToReal toyOutputs =
  let toyOutputsMap =
        Map.fromListWith HashSet.union $
          map (\toyOutput -> (to_dataIf toyOutput, HashSet.singleton (to_key toyOutput))) $
            HashSet.toList toyOutputs
   in AnyCompSinkOutsMap $
        Map.fromList $
          map
            ( \(dataIf, deps) ->
                (unTypedCompSinkId (typedCompSinkIdOf dataIf), testOutputs dataIf (HashSet.toList deps))
            )
            $ Map.toList toyOutputsMap

toy_outputsMapToReal :: (Ord k, Hashable k) => ToyOutputsMap k -> OutputsMap k
toy_outputsMapToReal =
  fromForwardMap . fmap toy_outputsToReal . toy_forwardMap

allPossibleToyOutputKeys :: [ToyOutput]
allPossibleToyOutputKeys =
  ToyOutput
    <$> allPossibleCompSinks
    <*> possibleOutputKeys
 where
  possibleOutputKeys =
    do
      prefix <- ["foo", "bar", "blub"]
      index <- [1 .. 4] :: [Int]
      pure $ prefix <> showText index

allPossibleToyOutputs :: [ToyOutput]
allPossibleToyOutputs = allPossibleToyOutputKeys

allPossibleCompSinks :: [CompSinkForTestingOutputs]
allPossibleCompSinks =
  map CompSinkForTestingOutputs ["foo", "bar"]

toyToRealOutputKey :: ToyOutput -> (Proxy CompSinkForTestingOutputs, CompSinkId, T.Text)
toyToRealOutputKey tok =
  (Proxy, unTypedCompSinkId (typedCompSinkIdOf (to_dataIf tok)), to_key tok)

newtype ToyKey = ToyKey
  { _unToyKey :: T.Text
  }
  deriving (Show, Eq, Ord, Hashable)

allPossibleToyKeys :: [ToyKey]
allPossibleToyKeys =
  map (\i -> ToyKey ("capKey" <> showText i)) ([1 .. 20] :: [Int])

instance QC.Arbitrary ToyKey where
  arbitrary =
    QC.elements allPossibleToyKeys

instance (QC.Arbitrary k, Hashable k) => QC.Arbitrary (ToyOutputsMap k) where
  arbitrary =
    toy_fromForwardMap . HashMap.fromList
      <$> QC.listOf (liftA2 (,) arbitrary genToyHashSet)
   where
    genToyHashSet =
      HashSet.fromList <$> QC.listOf (QC.elements allPossibleToyOutputs)
    liftA2 f x = (<*>) (fmap f x)

checkLookup :: (Hashable k) => ToyOutputsMap k -> OutputsMap k -> k -> QC.Property
checkLookup tom om key =
  (toy_outputsToReal <$> toy_lookup key tom) QC.=== lookup key om

checkLookupOutputKey
  :: (Show k, Ord k, Hashable k)
  => ToyOutputsMap k
  -> OutputsMap k
  -> ToyOutput
  -> QC.Property
checkLookupOutputKey tom om tok =
  let tomResult = toy_lookupOutputKey tok tom
      omResult = lookupOutputKey (toyToRealOutputKey tok) om
   in tomResult QC.=== HashSet.fromList (toList omResult)

checkLookupAllKeys :: ToyOutputsMap ToyKey -> OutputsMap ToyKey -> QC.Property
checkLookupAllKeys tom om =
  QC.conjoin $ map (checkLookup tom om) allPossibleToyKeys

prop_lookup :: ToyOutputsMap ToyKey -> QC.Property
prop_lookup tom =
  checkLookupAllKeys tom (toy_outputsMapToReal tom)

checkLookupAllOutputKeys :: ToyOutputsMap ToyKey -> OutputsMap ToyKey -> QC.Property
checkLookupAllOutputKeys tom om =
  QC.conjoin $ map (checkLookupOutputKey tom om) allPossibleToyOutputKeys

prop_lookupOutputKey :: ToyOutputsMap ToyKey -> QC.Property
prop_lookupOutputKey tom =
  checkLookupAllOutputKeys tom (toy_outputsMapToReal tom)

prop_insert :: ToyKey -> [ToyOutput] -> ToyOutputsMap ToyKey -> QC.Property
prop_insert key outsList tom =
  let outsSet = HashSet.fromList outsList
      om = toy_outputsMapToReal tom
      realAnyCompSinkOutsMap = toy_outputsToReal outsSet
      omAfterInsert = insert key realAnyCompSinkOutsMap om
      tomAfterInsert = toy_insert key outsSet tom
   in (lookup key omAfterInsert QC.=== Just realAnyCompSinkOutsMap)
        QC..&. checkLookupAllOutputKeys tomAfterInsert omAfterInsert
        QC..&. checkLookupAllKeys tomAfterInsert omAfterInsert

prop_delete :: ToyKey -> ToyOutputsMap ToyKey -> QC.Property
prop_delete key tom =
  let om = toy_outputsMapToReal tom
      omAfterDelete = delete key om
      tomAfterDelete = toy_delete key tom
   in (lookup key omAfterDelete QC.=== Nothing)
        QC..&. checkLookupAllOutputKeys tomAfterDelete omAfterDelete
        QC..&. checkLookupAllKeys tomAfterDelete omAfterDelete
