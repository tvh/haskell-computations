{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Control.Computations.CompEngine.SimpleStateIf (
  SimpleStateIf (..),
  SifState (..),
  mkSimpleCompEngineStateIf,
  initialSifState,
  validateSifState,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompFlow
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.SifCache (SifCache)
import qualified Control.Computations.CompEngine.SifCache as SifCache
import Control.Computations.CompEngine.Types
import Control.Computations.CompEngine.Utils.DepMap
import qualified Control.Computations.CompEngine.Utils.DepMap as DepMap
import Control.Computations.CompEngine.Utils.Intern (InternTable)
import qualified Control.Computations.CompEngine.Utils.Intern as Intern
import qualified Control.Computations.CompEngine.Utils.OutputsMap as OM
import qualified Control.Computations.CompEngine.Utils.PriorityAgingQueue as Paq
import Control.Computations.Utils.Logging
import qualified Control.Computations.Utils.StrictList as SL
import Control.Computations.Utils.Tuple
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad
import Data.Foldable (foldr')
import qualified Data.Foldable as F
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntSet as IntSet
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set

newtype SimpleStateIf m = SimpleStateIf
  { ssif_withState :: forall a. (SifState -> (a, SifState)) -> m a
  }

{- | The state layer interns every 'AnyCompAp' cap identity to a dense 'Int'
 id (see "Control.Computations.CompEngine.Utils.Intern") and re-keys its
 five bulk containers (cache, vermap, the dep map's two indices, outputs)
 on that id instead. 'InternedDep'/'InternedDepKey' are the corresponding
 interned form of the public 'CompEngDep'/'CompEngDepKey': the src half is
 untouched (its key was never 'AnyCompAp'), the comp half carries the
 target's interned id instead of the target's full 'AnyCompAp'.

 This is deliberately private to this module: 'DepSet'/'CompEngDep' (what
 'CompM' and "Control.Computations.CompEngine.Impl" actually manipulate)
 are untouched -- interning only happens at the point a dep set is handed
 to the state layer, and reversed (via 'Intern.resolve') only when a
 result crosses back out through the 'CompEngineStateIf' boundary.
-}
data InternedDep
  = InternedDepSrc !AnyCompSrcDep
  | InternedDepComp !Int !CompDepVer
  deriving (Eq, Show)

data InternedDepKey
  = InternedDepKeySrc !AnyCompSrcKey
  | InternedDepKeyComp !Int
  deriving (Eq, Show, Generic)

instance Hashable InternedDepKey

instance Hashable InternedDep where
  hashWithSalt s dep =
    case dep of
      InternedDepSrc x -> s `hashWithSalt` (0 :: Int) `hashWithSalt` x
      InternedDepComp i v -> s `hashWithSalt` (1 :: Int) `hashWithSalt` i `hashWithSalt` v

instance IsDep InternedDep where
  type DepKey InternedDep = InternedDepKey
  type DepVer InternedDep = CompEngDepVer
  depKey (InternedDepSrc x) = InternedDepKeySrc (depKey x)
  depKey (InternedDepComp i _) = InternedDepKeyComp i
  depVer (InternedDepSrc x) = CompEngDepVerSrc (depVer x)
  depVer (InternedDepComp _ v) = CompEngDepVerComp v

-- | Owners are interned cap ids; see the module haddock above.
type SifDeps = DepMap Int InternedDep
type SifVerMap = IntMap CompDepVer
type SifOutputs = OM.OutputsMap Int
type SifPendingOutputs = Map AnyCompAp AnyCompSinkOutsMap
type SifStale = Paq.PriorityAgingQueue AnyCompAp ()
type SifPendingCaps = Set AnyCompAp

data SifState = SifState
  { sifs_deps :: SifDeps
  , sifs_cache :: SifCache
  , sifs_vermap :: SifVerMap
  , sifs_outputs :: SifOutputs
  , sifs_pendingOutputs :: SifPendingOutputs
  , sifs_pendingCaps :: SifPendingCaps
  -- ^ caps for which calculation has been started but not finished
  , sifs_stale :: SifStale
  , sifs_intern :: InternTable AnyCompAp
  -- ^ the cap identity intern table; see the module haddock above
  }
  deriving (Show)

{- | Checks invariants about the SifState.

 Under interning, the old invariant here (that every container which
 stores an 'AnyCompAp' key stores the *same* shared thunk for that key,
 checked with 'reallyUnsafePtrEquality#' after a 'Set.lookupLE') is moot:
 the re-keyed containers no longer store 'AnyCompAp' at all, they store
 plain 'Int's, so there is nothing left to "not be the same thunk". Its
 replacement checks the invariant interning actually depends on: every id
 referenced by any of the five re-keyed containers must be currently live
 in the intern table (i.e. it was interned and not since released by GC).
 A dangling id here means a lifecycle bug -- an id used after 'runGc'
 released it, or a container that never got told about a fresh id.
-}
validateSifState :: SifState -> IO ()
validateSifState state =
  do
    let live = Intern.liveIds (sifs_intern state)
        checkIds what ids =
          let dangling = IntSet.difference ids live
           in unless (IntSet.null dangling) $
                fail
                  ( "When validating "
                      ++ what
                      ++ ": found ids not present in the intern table: "
                      ++ show (IntSet.toList dangling)
                  )
        depOwnerIds = IntSet.fromList $ HashSet.toList $ DepMap.keys (sifs_deps state)
        depTargetIds =
          IntSet.fromList $
            flip mapMaybe (HashSet.toList $ DepMap.depKeys (sifs_deps state)) $ \case
              InternedDepKeySrc _ -> Nothing
              InternedDepKeyComp i -> Just i
        cacheIds = SifCache.keysSet (sifs_cache state)
        vermapIds = IntMap.keysSet (sifs_vermap state)
        outputIds = IntSet.fromList $ HashMap.keys $ OM.forwardMap $ sifs_outputs state
    checkIds "sifs_deps (owners)" depOwnerIds
    checkIds "sifs_deps (dep targets)" depTargetIds
    checkIds "sifs_cache" cacheIds
    checkIds "sifs_vermap" vermapIds
    checkIds "sifs_outputs" outputIds

initialSifState :: SifState
initialSifState =
  SifState
    { sifs_deps = DepMap.empty
    , sifs_cache = emptyCache
    , sifs_vermap = IntMap.empty
    , sifs_outputs = OM.empty
    , sifs_pendingOutputs = mempty
    , sifs_pendingCaps = mempty
    , sifs_stale = Paq.empty
    , sifs_intern = Intern.empty
    }

emptyCache :: SifCache
emptyCache = SifCache.empty

mkSimpleCompEngineStateIf
  :: (Monad m)
  => SimpleStateIf m
  -> CompEngineStateIf m
mkSimpleCompEngineStateIf sif =
  CompEngineStateIf
    { lookupCapResult = lookupCapResultImpl sif
    , capEvaluationStarted = capEvaluationStartedImpl sif
    , capEvaluationFinished = putCapResultImpl sif
    , dequeueGivenCap = dequeueGivenCapImpl sif
    , staleQueueSize = staleQueueSizeImpl sif
    , dequeueNextCap = dequeueNextCapImpl sif
    , enqueueStaleCaps = enqueueStaleCapsImpl sif
    , trackOutput = trackOutputImpl sif
    , getCompSinkOuts = getDataIfOutputsImpl sif
    , getQueue = getQueueImpl sif
    }

--
-- Interning helpers
--

-- | Intern (get-or-create) the id for a cap.
internCap :: AnyCompAp -> SifState -> (Int, SifState)
internCap cap st =
  let (!i, !tbl') = Intern.intern cap (sifs_intern st)
   in (i, st{sifs_intern = tbl'})

-- | Intern a single public dependency edge into its private, id-based form.
-- This is the replacement for the old @normalizeDep@: interning a dep's
-- target *is* canonicalizing it, there's nothing further to normalize.
internDep :: CompEngDep -> SifState -> (InternedDep, SifState)
internDep dep st =
  case dep of
    CompEngDepSrc x -> (InternedDepSrc x, st)
    CompEngDepComp (CompDep (Dep (CompDepKey capAp) ver)) ->
      let (i, st') = internCap capAp st
       in (InternedDepComp i ver, st')

-- | Intern an entire (public) 'DepSet' into its private, id-based form.
internDepSet :: DepSet -> SifState -> (HashSet InternedDep, SifState)
internDepSet deps st0 =
  F.foldl' step (HashSet.empty, st0) deps
 where
  step (!acc, !st) d =
    let (!d', !st') = internDep d st
     in (HashSet.insert d' acc, st')

-- | Resolve a set of interned ids (as returned by 'DepMap.stale' et al, a
-- @HashSet Int@) back to their (public) 'AnyCompAp' form.
resolveSet :: HashSet Int -> SifState -> HashSet AnyCompAp
resolveSet ids st =
  HashSet.map (\i -> Intern.resolve i (sifs_intern st)) ids

-- | Look up (without creating) the interned form of a public dep, used
-- only to *query* staleness (see 'enqueueStaleCapsImpl'). 'Nothing' means
-- the dep's target has never been interned, so nothing can currently
-- depend on it.
queryInternedDep :: CompEngDep -> InternTable AnyCompAp -> Maybe InternedDep
queryInternedDep dep tbl =
  case dep of
    CompEngDepSrc x -> Just (InternedDepSrc x)
    CompEngDepComp (CompDep (Dep (CompDepKey capAp) ver)) ->
      (\i -> InternedDepComp i ver) <$> Intern.lookupId capAp tbl

--
-- CompEngineStateIf implementation
--

getQueueImpl :: Monad m => SimpleStateIf m -> m [AnyCompAp]
getQueueImpl sif =
  do
    (Paq.PaqView rtq xq rq bq) <- fromSifState sif (Paq.view . sifs_stale)
    return $! map Paq.paqe_key $ concat [rtq, xq, rq, bq]

staleQueueSizeImpl :: SimpleStateIf m -> m Int
staleQueueSizeImpl sif =
  fromSifState sif $ \st -> Paq.size (sifs_stale st)

trackOutputImpl
  :: (Monad m, IsCompResult a)
  => SimpleStateIf m
  -> CompAp a
  -> AnyCompSinkOutsMap
  -> m ()
trackOutputImpl sif cap outputs
  | nullAnyOutsMap outputs = return ()
  | otherwise =
      withSifState sif $ \st ->
        let outputsStr = unlines (map ("  - " ++) (lines (show outputs)))
            msg = (show (capId cap) ++ " produced the following outputs:\n" ++ outputsStr)
            newOutputs = Map.insertWith mappend (AnyCompAp cap) outputs (sifs_pendingOutputs st)
            newSt = st{sifs_pendingOutputs = newOutputs}
         in pureDebug msg ((), newSt)

commitPendingOutputsForKey :: SifState -> Int -> AnyCompAp -> SifState
commitPendingOutputsForKey st keyId capAny =
  case Map.lookup capAny (sifs_pendingOutputs st) of
    Nothing -> st{sifs_outputs = OM.insert keyId mempty (sifs_outputs st)}
    Just anyOuts ->
      let warnMsgs =
            catMaybes $
              do
                ForAnyCompFlow ident p (SomeCompSinkOuts outs) <-
                  Map.elems (unAnyCompSinkOutsMap anyOuts)
                output <- F.toList outs
                let revLookupResult =
                      OM.lookupOutputKey
                        (p, ident, output)
                        (sifs_outputs st)
                case SL.filter (/= keyId) revLookupResult of
                  SL.Nil -> pure Nothing
                  otherKeyIds ->
                    pure $
                      Just
                        ( "MULTIPLE_COMPAPS_ONE_OUTPUT: The output "
                            ++ show output
                            ++ " has just been generated by the comp ap "
                            ++ showAnyCompApDetails capAny
                            ++ ". Before, this output has been generated by the comp ap(s) "
                            ++ intercalate
                              ", "
                              (map (showAnyCompApDetails . flip Intern.resolve (sifs_intern st)) (SL.toList otherKeyIds))
                            ++ ". "
                            ++ " This 'travelling' of outputs of one comp ap "
                            ++ "to another is outlawed since it can lead to outdated "
                            ++ "outputs. FIX THIS!"
                        )
          doWarnings =
            if F.null warnMsgs
              then id
              else pureWarn (unlines warnMsgs)
       in doWarnings $
            st
              { sifs_outputs = OM.insert keyId anyOuts (sifs_outputs st)
              , sifs_pendingOutputs = Map.delete capAny (sifs_pendingOutputs st)
              }

getDataIfOutputsImpl
  :: forall s m
   . (CompSink s)
  => SimpleStateIf m
  -> s
  -> m (CompSinkOuts s)
getDataIfOutputsImpl sif s =
  fromSifState sif $ \st ->
    foldr'
      (\out set -> unionOuts (anyOutputsToCompSinkOutputs out) set)
      HashSet.empty
      (HashMap.elems $ OM.forwardMap $ sifs_outputs st)
 where
  anyOutputsToCompSinkOutputs :: AnyCompSinkOutsMap -> Maybe (CompSinkOuts s)
  anyOutputsToCompSinkOutputs m = compSinkOutsFromAny (Proxy @s) (compSinkId s) m
  unionOuts Nothing set = set
  unionOuts (Just set1) set2 = set1 `HashSet.union` set2

lookupCapResultImpl
  :: IsCompResult a
  => SimpleStateIf m
  -> CompAp a
  -> m (CapLookup (CapResult (CapCached a)))
lookupCapResultImpl sif cap =
  withSifState sif $ \sifState ->
    let (_, res, sifState') = lookupCapResultImpl' cap sifState
     in (res, sifState')

lookupCapResultImpl'
  :: IsCompResult r
  => CompAp r
  -> SifState
  -> (Int, CapLookup (CapResult (CapCached r)), SifState)
lookupCapResultImpl' cap st =
  let (keyId, st') = internCap (AnyCompAp cap) st
      res = SifCache.lookup keyId (sifs_cache st')
      result =
        case res of
          Nothing -> CapNotFound
          Just CapFailure -> CapFound CapFailure
          Just (CapSuccess (AnyCompCacheValue ccvAnyType)) ->
            let optionCvc = fmap capCached (castCompCacheValue ccvAnyType)
             in CapFound (optionToCapResult optionCvc)
   in (keyId, result, st')
 where
  capCached ccv =
    case ccv_payload ccv of
      Some value -> CapValueCached (CompApResult value ccv)
      None -> CapMetaCached (ccv_meta ccv)

fromSifState :: SimpleStateIf m -> (SifState -> a) -> m a
fromSifState (SimpleStateIf{..}) f = ssif_withState (\x -> (f x, x))

withSifState :: SimpleStateIf m -> (SifState -> (a, SifState)) -> m a
withSifState (SimpleStateIf{..}) = ssif_withState

dequeueNextCapImpl :: Monad m => SimpleStateIf m -> m (Maybe (AnyCompAp))
dequeueNextCapImpl sif =
  do
    res <- withSifState sif nextStaleCap
    return res
 where
  nextStaleCap :: SifState -> (Maybe AnyCompAp, SifState)
  nextStaleCap ges =
    case Paq.dequeue curQ of
      None ->
        pureDebug
          ("Cache has " ++ show instanceCount ++ " entries.")
          (Nothing, ges)
      Some (e :!: newQ) ->
        pureDebug
          ( "Dequeued "
              ++ show (Paq.paqe_key e)
              ++ " with "
              ++ show (Paq.size newQ)
              ++ " stale caps remaining."
          )
          $ let newSt =
                  ges
                    { sifs_stale = newQ
                    }
             in (Just (Paq.paqe_key e), newSt)
   where
    curQ = sifs_stale ges
    cache = sifs_cache ges
    instanceCount = SifCache.totalInstanceCount cache

capEvaluationStartedImpl :: IsCompResult a => SimpleStateIf m -> CompAp a -> m ()
capEvaluationStartedImpl sif cap =
  withSifState sif $ \s ->
    let newPendingCaps = Set.insert (AnyCompAp cap) (sifs_pendingCaps s)
     in ((), s{sifs_pendingCaps = newPendingCaps})

putCapResultImpl
  :: SimpleStateIf m
  -> CompAp a
  -> DepSet
  -> Maybe a
  -> m (HashSet AnyCompAp, Garbage)
putCapResultImpl sif cap deps mres =
  withSifState sif (putCapRes cap deps (maybeToOption mres))

runGc
  :: String
  -> SifState
  -> Garbage
  -> HashSet InternedDepKey
  -> (SifState, Garbage)
runGc capIdStr !origS del@(Garbage _delCaps _delDeps delOutsMap) garbage =
  case HashSet.toList garbage of
    [] ->
      let delOuts = unionsAnyCompSinkOutsMap $ HashMap.elems delOutsMap
       in ( origS
          , if not (nullAnyOutsMap delOuts)
              then
                pureInfo
                  ( "Change of "
                      ++ capIdStr
                      ++ " unreferenced "
                      ++ ( if sizeAnyOutsMap delOuts == 1
                            then "one output: "
                            else show (sizeAnyOutsMap delOuts) ++ " outputs:\n"
                         )
                      ++ ( intercalate "\n" $
                            map (("- " ++) . show) (anyOutsMapToList delOuts)
                         )
                  )
                  del
              else del
          )
    x@(InternedDepKeyComp capIntId) : _ ->
      let capAny = Intern.resolve capIntId (sifs_intern origS)
          (dm', garbage') = DepMap.delete capIntId (sifs_deps origS)
          oldOuts = sifs_outputs origS
          newOuts = OM.delete capIntId oldOuts
          newS =
            origS
              { sifs_deps = dm'
              , sifs_cache = SifCache.delete capIntId (sifs_cache origS)
              , sifs_vermap = IntMap.delete capIntId (sifs_vermap origS)
              , sifs_outputs = newOuts
              , sifs_stale = Paq.delete capAny (sifs_stale origS)
              , sifs_intern = Intern.release capIntId (sifs_intern origS)
              }
          newGarbage = HashSet.delete x garbage `HashSet.union` garbage'
          garbageOutputs =
            maybe mempty (OM.filterUnreferencedOutputs newOuts) $
              OM.lookup capIntId oldOuts
          moreDel =
            emptyGarbage
              { garbage_caps = HashSet.singleton capAny
              , garbage_outputs =
                  if garbageOutputs == mempty
                    then mempty
                    else HashMap.singleton capAny garbageOutputs
              }
          del' = del `mappend` moreDel
       in runGc capIdStr newS del' newGarbage
    x@(InternedDepKeySrc srcDepKey) : _ ->
      let moreDel =
            emptyGarbage{garbage_deps = HashSet.singleton srcDepKey}
       in runGc capIdStr origS (del <> moreDel) (HashSet.delete x garbage)

insertCapWithDeps
  :: Int
  -> AnyCompAp
  -> HashSet InternedDep
  -> SifState
  -> (SifState, Maybe (HashSet InternedDep), Garbage)
insertCapWithDeps capIntId capAny deps sIn =
  let capIdStr = show (anyCapId capAny)
      mOldOutputs = OM.lookup capIntId (sifs_outputs sIn)
      oldOutputs = fromMaybe mempty mOldOutputs
      newOutputs = Map.findWithDefault mempty capAny (sifs_pendingOutputs sIn)
      addOutputs = newOutputs `diffAnyOutsMap` oldOutputs
      delOutputs = oldOutputs `diffAnyOutsMap` newOutputs
      addCount = sizeAnyOutsMap addOutputs
      delCount = sizeAnyOutsMap delOutputs
      changeCount = addCount + delCount
      outputsChanged = newOutputs /= oldOutputs
      s = commitPendingOutputsForKey sIn capIntId capAny
      garbOutputs = OM.filterUnreferencedOutputs (sifs_outputs s) delOutputs
      logOutputs x =
        logMsg1 msg1 $!
          logMsg2 msg2 $!
            x
       where
        logMsg1
          | changeCount > 0 && isJust mOldOutputs = pureInfo
          | changeCount > 0 = pureDebug
          | outputsChanged = pureDebug
          | nullAnyOutsMap oldOutputs && nullAnyOutsMap newOutputs = pureNoLog
          | otherwise = pureDebug
        logMsg2
          | changeCount > 0 = pureDebug
          | outputsChanged = pureDebug
          | otherwise = pureNoLog
        msg1 =
          ( capIdStr
              ++ " now has "
              ++ show (sizeAnyOutsMap newOutputs)
              ++ " outputs. "
              ++ "It previously had "
              ++ show (sizeAnyOutsMap oldOutputs)
              ++ " (+"
              ++ show addCount
              ++ "/-"
              ++ show delCount
              ++ ")"
              ++ ( if changeCount == 0
                    then ". No keys changed."
                    else if changeCount > 1 then ":\n" else ": "
                 )
              ++ ( intercalate
                    "\n"
                    ( mapAnyOutsMap (\_ -> ("+ " ++) . show) addOutputs
                        ++ mapAnyOutsMap (\_ -> ("- " ++) . show) delOutputs
                    )
                 )
          )
        msg2 =
          ( "Outputs of "
              ++ show capAny
              ++ " changed:\n"
              ++ "Old outputs: "
              ++ show oldOutputs
              ++ "\n"
              ++ "New outputs: "
              ++ show newOutputs
          )
      (tempDepMap, mOldDeps, directGarbage) = DepMap.insert' capIntId deps (sifs_deps s)
      tempS = s{sifs_deps = tempDepMap}
      (newS, allGarbageCaps) = runGc capIdStr tempS emptyGarbage directGarbage
      allGarbage =
        if nullAnyOutsMap garbOutputs
          then allGarbageCaps
          else
            allGarbageCaps
              `mappend` (mempty{garbage_outputs = HashMap.singleton capAny garbOutputs})
      notify (x, d, g) =
        if g == mempty
          then (x, d, g)
          else
            pureDebug
              ( "Insertion of "
                  ++ capIdStr
                  ++ " led to garbage: "
                  ++ show allGarbage
              )
              (x, d, g)
      msg = ("insertCapWithDeps " ++ capIdStr ++ " <- " ++ show (HashSet.toList deps))
   in pureDebug msg $!
        logOutputs $!
          notify $!
            ( newS
            , mOldDeps
            , allGarbage
            )

putCapRes
  :: CompAp a
  -> DepSet
  -> Option a
  -> SifState
  -> ((HashSet AnyCompAp, Garbage), SifState)
putCapRes gap@(CompAp{cap_comp = Comp{comp_caching = CompCacheBehavior{..}}}) deps res s0 =
  let capAny = AnyCompAp gap
      -- Intern this cap (also fetches its current cache entry, if any) and
      -- every dep it references. This is the replacement for the old
      -- `normalizeDep`/sharing-trick: the interned id already is the
      -- canonical identity, there's no separate "canonical shared object"
      -- left to look up.
      (capIntId, oldCcv, s0a) = lookupCapResultImpl' gap s0
      (internedDeps, s1) = internDepSet deps s0a

      ccv = fmap ccb_memcache res
      anyCcv = fmap AnyCompCacheValue ccv
      newVer = CompDepVer (fmap ccv_largeHash ccv)
      key = anyCapId capAny
      oldVer = fmap capResultToVer oldCcv
      staleInt = DepMap.stale (InternedDepComp capIntId newVer) (sifs_deps s1)
      (s2, oldDeps, garbage) = insertCapWithDeps capIntId capAny internedDeps s1
      s3 =
        -- first delete current cap from queue - if it's stale it should be enqueued at the end
        s2{sifs_stale = Paq.delete capAny (sifs_stale s2)}
      s4 =
        let prevS = s3
            cache =
              SifCache.insert capIntId capAny (optionToCapResult anyCcv) (sifs_cache prevS)
            vermap = IntMap.insert capIntId newVer (sifs_vermap prevS)
         in s3
              { sifs_cache = cache
              , sifs_vermap = vermap
              }
      (newStaleCaps, s5)
        | oldDeps == Just internedDeps
            && oldVer /= CapNotFound
            && oldVer /= CapFound newVer =
            pureError
              ( "Impure cap "
                  ++ show gap
                  ++ " returned different result than previously "
                  ++ " although inputs didn't change.  Not marking anything as stale."
                  ++ " New result:\n"
                  ++ show ccv
                  ++ "\nOld result:\n"
                  ++ show oldCcv
                  ++ "\nDeps are: "
                  ++ concatMap (("\n - " ++) . show) deps
              )
              (HashSet.empty, s4)
        | otherwise =
            let staleAny = resolveSet staleInt s4
             in (if HashSet.null staleAny then pureNoLog else pureDebug)
                  ( show key
                      ++ " invalidates "
                      ++ show (HashSet.size staleAny)
                      ++ ( if HashSet.size staleAny >= 5
                            then " caps:" ++ concatMap (("\n " ++) . show) staleAny
                            else " caps " ++ show (HashSet.toList staleAny)
                         )
                  )
                  (invalidate staleAny s4)
      -- finally remove current cap from stack of pending caps (must be done after invalidate
      -- because invalidate only enqueues caps that are not currently being calculated)
      s6 =
        case sifs_pendingCaps s5 of
          xs
            | capAny `Set.member` xs ->
                s5{sifs_pendingCaps = Set.delete capAny xs}
            | otherwise ->
                pureError
                  ( "Tried to pop "
                      ++ show key
                      ++ " from pending set but "
                      ++ "it's not contained:\n"
                      ++ concatMap (("\n " ++) . show) xs
                  )
                  s5
      s7 =
        -- now store the current version of the cap in the verpmap and check if it's stale
        let changedDeps = mapMaybe checkIfDepChanged (HashSet.toList internedDeps)
            checkIfDepChanged idep =
              case idep of
                InternedDepSrc{} -> Nothing
                InternedDepComp i observedVer ->
                  case IntMap.lookup i (sifs_vermap s6) of
                    Nothing ->
                      pureError
                        ( "Cap "
                            ++ show key
                            ++ " depends on "
                            ++ show (Intern.resolve i (sifs_intern s6))
                            ++ " but "
                            ++ "it's not contained in the vermap."
                        )
                        Nothing
                    Just curver
                      | observedVer /= curver -> Just (i, curver)
                      | otherwise -> Nothing
            sNew =
              s6{sifs_stale = snd $ Paq.enqueue (mkPaqEntry capAny) (sifs_stale s6)}
         in case changedDeps of
              [] -> s6
              [(i, cur)] ->
                pureInfo
                  ( "Cap "
                      ++ show key
                      ++ " depends on the old "
                      ++ show (Intern.resolve i (sifs_intern s6))
                      ++ " but new version is "
                      ++ show cur
                      ++ ".  Invalidating current cap!"
                  )
                  sNew
              xs ->
                pureInfo
                  ( "Cap "
                      ++ show key
                      ++ " depends on old caps. Invalidating current cap!"
                      ++ concatMap
                        (\(i, cur) -> "\n " ++ show (Intern.resolve i (sifs_intern s6)) ++ " --> " ++ show cur)
                        xs
                  )
                  sNew
   in ((newStaleCaps, garbage), s7)

dequeueGivenCapImpl :: IsCompResult a => SimpleStateIf m -> CompAp a -> m Bool
dequeueGivenCapImpl sif cap =
  withSifState sif $ \sifs0 ->
    let
      -- first remove the cap from the queue of stale caps
      (didDequeueCap, sifs1) =
        case Paq.deleteView (AnyCompAp cap) (sifs_stale sifs0) of
          Some (_ :!: q) ->
            pureDebug
              ( "Dequeued given cap "
                  ++ show key
                  ++ ", "
                  ++ show (Paq.size q)
                  ++ " stale caps remaining."
              )
              (True, sifs0{sifs_stale = q})
          None ->
            (False, sifs0)
     in
      (didDequeueCap, sifs1)
 where
  key = capId cap

mkPaqEntry :: AnyCompAp -> Paq.PaqEntry (AnyCompAp) ()
mkPaqEntry key =
  Paq.PaqEntry (Paq.PaqTime 0) (anyCompApPriority key) key ()

enqueueStaleCapsImpl
  :: (Foldable t)
  => SimpleStateIf m
  -> t (CompEngDep)
  -> m (EnqueueInfo)
enqueueStaleCapsImpl sif deps =
  withSifState sif $ \gesOld ->
    let (depsWithStaleCaps, gesNew) = notifyDepChanges deplist gesOld
        affectedStaleCaps = mkRecompInfoMap depsWithStaleCaps
        enqueueInfo =
          EnqueueInfo
            { ei_affectedCaps = affectedStaleCaps
            , ei_currentQueueSize = Paq.size (sifs_stale gesNew)
            }
     in (enqueueInfo, gesNew)
 where
  deplist = F.toList deps
  notifyDepChanges deps state =
    F.foldl'
      ( \(xs, state) dep ->
          let (set, newState) = notifyDepChange dep state
           in ((dep, set) : xs, newState)
      )
      ([], state)
      deps

  notifyDepChange dep oldS =
    let staleAny =
          case queryInternedDep dep (sifs_intern oldS) of
            Nothing -> HashSet.empty
            Just idep -> resolveSet (DepMap.stale idep (sifs_deps oldS)) oldS
        msg = "Change " ++ show dep ++ " invalidates " ++ show staleAny
        maybeLog
          | HashSet.null staleAny =
              pureDebug $
                "Nothing depends on (an old version of) " ++ show dep
          | otherwise = pureDebug msg
        (_addedKeys, newS) = invalidate staleAny oldS
     in maybeLog (staleAny, newS)

{- | Enqueues all given caps in the queue of stale caps an returns those that were not already
 enqueued.
-}
invalidate :: HashSet AnyCompAp -> SifState -> (HashSet AnyCompAp, SifState)
invalidate stale ges =
  let foldFun s@(newStaleCaps, curQ) x
        | x `Set.member` sifs_pendingCaps ges =
            -- don't enqueue cap that has already been dequeued for computation
            s
        | otherwise =
            case Paq.enqueue (mkPaqEntry x) curQ of
              (Paq.EnqueueAddedNewEntry, nextQ) -> (HashSet.insert x newStaleCaps, nextQ)
              (Paq.EnqueueUpdatedEntry, nextQ) -> (newStaleCaps, nextQ)
      (enqueuedCaps, newQ) = F.foldl' foldFun (HashSet.empty, sifs_stale ges) stale
   in (enqueuedCaps, ges{sifs_stale = newQ})
