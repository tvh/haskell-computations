{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- | Per-definition struct-of-arrays state layer: this module represents the
 engine's mutable state as per-definition columnar tables
 ("Control.Computations.CompEngine.Utils.DefTable") rather than a handful of
 boxed, "AnyCompAp"-then-@Int@-keyed persistent containers shared across
 every definition. Row identity is a packed @('DT.DefIdx', 'DT.RowIdx')@
 ('DT.DefRef'); a def's own hash/flags/edge/typed-value columns live in its
 own 'DT.DefTable', reached from a 'CompId' via 'sifs_defIndex' + 'sifs_defs'.
 'DT.DefRef', 'DT.DefIdx' and 'DT.RowIdx' are real newtypes (not @Int@
 aliases) precisely so this module's own bookkeeping -- which juggles a def
 index, a row index, and a packed ref side by side in almost every function
 below -- can't transpose them without a type error; the few spots that must
 still see a raw @Int@ (this module's two @Data.IntMap.Strict@-keyed
 fields, @sifs_defs@ and @sifs_srcEntries@, which mandate a literal @Int@
 key) unwrap explicitly via 'DT.unDefIdx'\/'SI.unSrcKeyId' right at that
 boundary, not before. See the individual functions below and
 DefTable.hs's/SrcIndex.hs's module haddocks for the row-lifecycle and
 column-layout rationale.

 = Existential plumbing

 A cap's parameter type @p@ never appears in the public
 'Control.Computations.CompEngine.Core.CompEngineStateIf' interface (only
 the result type @a@ does), so it can't be named in any of this module's
 exported-shaped functions either -- but 'DT.DefTable' needs it. 'withRow'
 (and 'withDefFor', which it's built on) recover @p@ by pattern-matching
 the 'CompAp' argument via its @CompAp@ pattern synonym and hand it to a
 rank-2-polymorphic continuation; every helper /downstream/ of that (taking
 an already-resolved @'DT.DefTable' p a@ as a plain argument) is ordinary
 first-rank code, since instantiating a polymorphic function at a local
 skolem is just application, not escape.

 = Two design choices in this state representation

 * __No multi-version reverse-dep buckets.__ Every row's dependents
   (@rdeps@) are a flat, unversioned list; invalidation walks that list
   whenever a row's result actually changes (a \"changed bit\": the new
   result hash compared against what was there before). Detecting an
   /impure/ cap -- one re-run with an unchanged, same-version dependency
   set that nonetheless produced a different result -- still needs
   per-edge /observed version/ information to tell that apart from an
   ordinary recompute where the dependency set has the same *targets* but
   a *new* observed version -- see 'DT.dt_compDeps's haddock for why that
   column carries a version alongside each target ref.
 * __'CompCacheMeta' carries only a hash__ (see "Types.hs"'s haddock for
   why): nothing in this state layer needs a pre-rendered log string or a
   per-'CompId' size tally, so neither exists here either. A row's result
   hash, gated by its result-state flag, doubles as its version -- there
   is no separate version map. Whether a row is currently mid-evaluation
   is a flag bit on the row itself, not membership in a separate pending
   set. Interning is intrinsic to 'DT.lookupOrInsertRow' (per-def, keyed
   by param hash) rather than a separate global table. The output
   containers ("Control.Computations.CompEngine.Utils.OutputsMap") are
   keyed by packed refs; a cap absent from the map means \"no outputs\"
   via 'OM.lookup's 'Nothing' case, so a row with no current outputs is
   deleted from the map rather than given an empty entry.

 = Concurrency

 Genuinely mutable columns are incompatible with running state transitions
 as pure @SifState -> (a, SifState)@ functions inside an STM @atomically@
 block: an STM transaction can retry, and retrying arbitrary in-place
 vector mutation is unsound. 'SimpleStateIf' instead serializes access to
 one persistent, internally-mutable 'SifState' with a plain IO action -- see
 "Control.Computations.CompEngine.Run"'s @MVar@-guarded
 @setupSimpleStateIf@. This gives up the free, lock-free snapshots a
 @TVar@ would offer, but 'stepCompEngine' is sequential, so nothing today
 needs one.
-}
module Control.Computations.CompEngine.SimpleStateIf (
  SimpleStateIf (..),
  SifState,
  mkSimpleCompEngineStateIf,
  newSifState,
  validateSifState,
  debugTotalSrcDepInternLiveCount,
  debugSrcKeyInternLiveCount,
  debugSrcKeyInternAssignedCount,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompFlow
import Control.Computations.CompEngine.CompSink
import Control.Computations.CompEngine.CompSrc
import Control.Computations.CompEngine.Core
import Control.Computations.CompEngine.Types
import Control.Computations.CompEngine.Utils.DefTable (
  DefRef,
  DefTable,
  ResultState (..),
 )
import qualified Control.Computations.CompEngine.Utils.DefTable as DT
import qualified Control.Computations.CompEngine.Utils.OutputsMap as OM
import Control.Computations.CompEngine.Utils.PriorityAgingQueue (PaqPriority)
import qualified Control.Computations.CompEngine.Utils.PriorityAgingQueue as Paq
import qualified Control.Computations.CompEngine.Utils.SrcIndex as SI
import Control.Computations.Utils.Hash (Hash128)
import Control.Computations.Utils.Logging
import Control.Computations.Utils.Types

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Foldable as F
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.IORef
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Proxy (Proxy (..))
import Data.Strict.Tuple (Pair (..))
import Data.Type.Equality ((:~:) (Refl))
import Data.Typeable (Typeable, eqT)
import qualified Data.Vector.Unboxed as VU
import Data.Word (Word64)
import qualified StrictList as SL

--
-- Existential per-definition entry: a Comp p a (needed to reconstruct an
-- AnyCompAp from a bare row -- see resolveRefToAny) alongside its DefTable
-- p a. p/a are recovered at each use via eqT (see castDefEntry), the same
-- one-cast-per-lookup idea "Types.hs"'s castCompCacheValue already uses --
-- not a new category of cost, just moved here.
--
data SomeDefEntry
  = forall p a.
    (IsCompParam p, IsCompResult a) =>
    SomeDefEntry (Comp p a) (DefTable p a)

newtype SimpleStateIf m = SimpleStateIf
  { ssif_withState :: forall a. (SifState -> m a) -> m a
  }

{- | The reverse source index: for every source key currently depended on by
 at least one row, the flat set of dependent rows, each with the version
 /that specific row/ last observed for this key. Per-dependent version
 tracking (rather than one shared "current version" per key) is required,
 not optional: a test (@test_modifcationWhileWorkingOnQueue@) exercises
 exactly the case where two rows depend on the same key but have observed
 *different* versions of it (one caught up to a newer notification before
 the other), and a notification must invalidate only the row(s) whose
 recorded version doesn't match -- a single flat "last known version" dedup
 per key can't distinguish that from "everyone's stale", and gets it
 wrong.

 Storage is columnar (see "Utils/SrcIndex.hs"'s module haddock for the full
 design rationale, mirroring "Utils/DefTable.hs"'s forward-side src-dep
 interning): 'sifs_srcKeyIntern' interns each 'AnyCompSrcKey' to a dense
 'Int', and 'sifs_srcEntries' maps that id to a 'SI.SrcKeyArena' holding the
 key's current @(DefRef, AnyCompSrcVer)@ dependent list as an unboxed
 'DefRef' column plus a parallel boxed 'AnyCompSrcVer' column, rather than
 a @HashMap AnyCompSrcKey (HashMap DefRef AnyCompSrcVer)@ (a boxed
 existential key hashed/compared on every access, nested inside a second
 boxed HAMT). A key's arena is created on its first dependent and deleted
 on its last -- see 'addSrcDependent'\/'removeSrcDependent'.
-}
data SifState = SifState
  { sifs_defIndex :: !(IORef (HashMap CompId DT.DefIdx))
  , sifs_defs :: !(IORef (IntMap SomeDefEntry))
  -- ^ keyed by the raw 'Int' 'DT.unDefIdx' of a 'DT.DefIdx' --
  -- 'Data.IntMap.Strict' mandates a literal 'Int' key, so every lookup\/
  -- insert against this field unwraps a 'DT.DefIdx' at exactly this
  -- boundary (see 'DT.unDefIdx''s haddock).
  , sifs_nextDefIdx :: !(IORef Int)
  -- ^ the raw counter 'withDefFor' wraps into a fresh 'DT.DefIdx' via
  -- 'DT.mkDefIdx' when it hands one out.
  , sifs_srcKeyIntern :: !SI.KeyIntern
  , sifs_srcEntries :: !(IORef (IntMap SI.SrcKeyArena))
  -- ^ keyed by the raw 'Int' 'SI.unSrcKeyId' of a 'SI.SrcKeyId', for the
  -- same 'IntMap'-mandates-literal-'Int' reason as 'sifs_defs'.
  , sifs_outputs :: !(IORef (OM.OutputsMap DT.DefRef))
  , sifs_pendingOutputs :: !(IORef (Map DT.DefRef AnyCompSinkOutsMap))
  , sifs_stale :: !(IORef (Paq.PriorityAgingQueue DT.DefRef ()))
  }

newSifState :: IO SifState
newSifState =
  SifState
    <$> newIORef HashMap.empty
    <*> newIORef IntMap.empty
    <*> newIORef 0
    <*> SI.newKeyIntern
    <*> newIORef IntMap.empty
    <*> newIORef OM.empty
    <*> newIORef Map.empty
    <*> newIORef Paq.empty

{- | Checks invariants about the SifState: every def index referenced by
 the def registry has a table entry, every alive row's forward
 (@compDeps@) or reverse (@rdeps@) edges point only at other alive rows, and
 every 'SI.SrcKeyArena' entry in 'sifs_srcEntries' points at an alive row.
 Dangling references are always a lifecycle bug -- a row's edges and every
 arena entry pointing at it must be scrubbed before the row itself is
 freed, so this check should never fail on correct code.
-}
validateSifState :: SifState -> IO ()
validateSifState st = do
  defs <- readIORef (sifs_defs st)
  forM_ (IntMap.toList defs) $ \(defIdx, SomeDefEntry _ dt) -> do
    len <- DT.rowCount dt
    forM_ (DT.rowIndices len) $ \row -> do
      flags <- DT.readFlags dt row
      when (DT.flagsAlive flags) $ do
        cd <- DT.readCompDeps dt row
        rd <- DT.readRdeps dt row
        checkRefs defs ("compDeps of def " ++ show defIdx ++ " row " ++ show row) (DT.compDepTargets cd)
        checkRefs defs ("rdeps of def " ++ show defIdx ++ " row " ++ show row) rd
  entries <- readIORef (sifs_srcEntries st)
  forM_ (IntMap.toList entries) $ \(keyId, arena) -> do
    pairs <- SI.skaToList arena
    checkRefs defs ("srcIndex key " ++ show keyId) (VU.fromList (map (DT.unDefRef . fst) pairs))
 where
  checkRefs :: IntMap SomeDefEntry -> String -> VU.Vector Int -> IO ()
  checkRefs defs what refs =
    forM_ (VU.toList refs) $ \refInt -> do
      let (targetDef, targetRow) = DT.unpackRef (DT.mkDefRefUnsafe refInt)
      case IntMap.lookup (DT.unDefIdx targetDef) defs of
        Nothing -> fail (what ++ " points at unknown def " ++ show targetDef)
        Just (SomeDefEntry _ dt) -> do
          flags <- DT.readFlags dt targetRow
          unless (DT.flagsAlive flags) $
            fail (what ++ " points at dead row " ++ show (targetDef, targetRow))

{- | Debug/test-only: total number of currently-live interned src-dep ids
 summed across every def's own 'DT.DefTable' (i.e. 'DT.srcDepInternLiveCount'
 per def, added). Exists purely so an engine-level test
 (Tests/TestStateIf.hs) can assert that src-dep refcounting stays correct
 through the real @capEvaluation@/@capEvaluationFinished@ API -- not part
 of 'CompEngineStateIf', never called by "Impl.hs" or "Run.hs".
-}
debugTotalSrcDepInternLiveCount :: SifState -> IO Int
debugTotalSrcDepInternLiveCount st = do
  defs <- readIORef (sifs_defs st)
  sum <$> mapM (\(SomeDefEntry _ dt) -> DT.srcDepInternLiveCount dt) (IntMap.elems defs)

{- | Debug/test-only: number of currently-interned source keys (i.e. keys
 with at least one dependent right now) -- the reverse-index analogue of
 'debugTotalSrcDepInternLiveCount', exercised by the churn test in
 Tests/TestStateIf.hs.
-}
debugSrcKeyInternLiveCount :: SifState -> IO Int
debugSrcKeyInternLiveCount st = SI.kiLiveCount (sifs_srcKeyIntern st)

{- | Debug/test-only: total source-key ids ever assigned (not decremented on
 release) -- lets a churn test confirm ids are actually being recycled
 rather than growing once per intern/release cycle.
-}
debugSrcKeyInternAssignedCount :: SifState -> IO Int
debugSrcKeyInternAssignedCount st = SI.kiAssignedCount (sifs_srcKeyIntern st)

mkSimpleCompEngineStateIf
  :: (MonadIO m)
  => SimpleStateIf m
  -> CompEngineStateIf m
mkSimpleCompEngineStateIf sif =
  CompEngineStateIf
    { lookupCapResult = lookupCapResultImpl sif
    , capEvaluationStarted = capEvaluationStartedImpl sif
    , capEvaluationFinished = capEvaluationFinishedImpl sif
    , dequeueGivenCap = dequeueGivenCapImpl sif
    , staleQueueSize = staleQueueSizeImpl sif
    , dequeueNextCap = dequeueNextCapImpl sif
    , enqueueStaleCaps = enqueueStaleCapsImpl sif
    , trackOutput = trackOutputImpl sif
    , getCompSinkOuts = getDataIfOutputsImpl sif
    , getQueue = getQueueImpl sif
    }

withSifState :: SimpleStateIf m -> (SifState -> m a) -> m a
withSifState = ssif_withState

--
-- Def/row resolution. See the module haddock's "Existential plumbing"
-- section for why withDefFor/withRow are CPS-shaped.
--

withDefFor
  :: forall p a r
   . (IsCompParam p, IsCompResult a)
  => SifState
  -> Comp p a
  -> (DT.DefIdx -> DefTable p a -> IO r)
  -> IO r
withDefFor st comp k = do
  let cid = comp_name comp
  idxMap <- readIORef (sifs_defIndex st)
  case HashMap.lookup cid idxMap of
    Just defIdx -> do
      defs <- readIORef (sifs_defs st)
      case IntMap.lookup (DT.unDefIdx defIdx) defs of
        Just entry -> castDefEntry cid entry >>= k defIdx
        Nothing ->
          error
            ( "SimpleStateIf.withDefFor: defIndex "
                ++ show defIdx
                ++ " for "
                ++ show cid
                ++ " has no table entry -- a lifecycle bug, not a data problem"
            )
    Nothing -> do
      dt <- DT.new
      defIdxInt <- readIORef (sifs_nextDefIdx st)
      writeIORef (sifs_nextDefIdx st) (defIdxInt + 1)
      let defIdx = DT.mkDefIdx defIdxInt
      modifyIORef' (sifs_defIndex st) (HashMap.insert cid defIdx)
      modifyIORef' (sifs_defs st) (IntMap.insert defIdxInt (SomeDefEntry comp dt))
      k defIdx dt

-- | One cast per def resolution -- the same idea "Types.hs"'s
-- 'castCompCacheValue' already applies per cache read, just moved to the
-- def-lookup step; a 'CompId' maps to exactly one @(p, a)@ pair for the
-- lifetime of the engine (comp names are unique, checked at wiring time by
-- "CompDef.hs"'s @insertComp@), so the fallback branch below can only mean
-- a genuine bug, not a legitimate runtime occurrence.
castDefEntry :: forall p a. (Typeable p, Typeable a) => CompId -> SomeDefEntry -> IO (DefTable p a)
castDefEntry cid (SomeDefEntry (_ :: Comp p' a') dt) =
  case (eqT :: Maybe (p :~: p'), eqT :: Maybe (a :~: a')) of
    (Just Refl, Just Refl) -> pure dt
    _ ->
      error
        ( "SimpleStateIf.castDefEntry: type mismatch resolving "
            ++ show cid
            ++ " -- a CompId must map to exactly one (param, result) type pair"
        )

-- | Get-or-create the def table for a cap's computation and its row within
-- that table, then run a continuation with typed access to both. The
-- continuation is rank-2 polymorphic in the cap's (otherwise never named)
-- parameter type -- see the module haddock.
withRow
  :: forall a r
   . IsCompResult a
  => SifState
  -> CompAp a
  -> (forall p. IsCompParam p => DT.DefIdx -> DefTable p a -> DT.RowIdx -> Bool -> IO r)
  -> IO r
withRow st cap k =
  case cap of
    CompAp _ comp param ->
      withDefFor st comp $ \defIdx dt -> do
        (row, fresh) <- DT.lookupOrInsertRow dt (cap_hash cap) param
        k defIdx dt row fresh

-- | Reconstruct an 'AnyCompAp' from a packed ref -- needed only at the
-- boundary where the public interface hands back an 'AnyCompAp' (stale
-- sets, GC reports, dequeue results) for a row this call didn't already
-- have the typed 'CompAp' for in hand. Not on the hot inner path: one
-- 'IntMap' lookup plus one param-column read plus (via 'mkCompAp')
-- recomputing that cap's hash, deterministically identical to the hash
-- already stored in its param-hash column.
resolveRefToAny :: SifState -> DefRef -> IO AnyCompAp
resolveRefToAny st ref = do
  let (defIdx, row) = DT.unpackRef ref
  defs <- readIORef (sifs_defs st)
  case IntMap.lookup (DT.unDefIdx defIdx) defs of
    Just (SomeDefEntry comp dt) -> do
      -- Every ref flowing through the state layer (stale queue, rdeps,
      -- outputs) must originate from a row that is still alive -- garbage
      -- collection (freeRowCascade) is responsible for scrubbing a dying
      -- row out of every container that could hand its ref back here
      -- before freeing it. Hitting a dead row is therefore always a
      -- lifecycle bug, never a legitimate runtime occurrence -- fail
      -- loudly instead of silently reading whatever garbage (or a
      -- recycled occupant's data) happens to be sitting in the freed row's
      -- columns.
      alive <- DT.isAlive dt row
      unless alive $
        error
          ( "SimpleStateIf.resolveRefToAny: ref "
              ++ show ref
              ++ " (def "
              ++ show defIdx
              ++ ", row "
              ++ show row
              ++ ") is not (or no longer) alive -- a lifecycle bug in the caller"
          )
      p <- DT.readParam dt row
      pure (AnyCompAp (mkCompAp comp p))
    Nothing ->
      error ("SimpleStateIf.resolveRefToAny: unknown def index " ++ show defIdx ++ " (bug)")

resolveRefsToAny :: SifState -> HashSet DefRef -> IO (HashSet AnyCompAp)
resolveRefsToAny st refs = HashSet.fromList <$> mapM (resolveRefToAny st) (HashSet.toList refs)

--
-- CompDepVer <-> unboxed pair, for the compDeps column's observed-version
-- slot (see DefTable.hs's dt_compDeps haddock).
--

encodeVer :: CompDepVer -> (Word64, Word64)
encodeVer (CompDepVer None) = DT.noResultSentinel
encodeVer (CompDepVer (Some h)) = DT.hashToPair h

decodeVer :: (Word64, Word64) -> CompDepVer
decodeVer pair
  | pair == DT.noResultSentinel = CompDepVer None
  | otherwise = CompDepVer (Some (DT.pairToHash pair))

--
-- CompEngineStateIf implementation
--

getQueueImpl :: MonadIO m => SimpleStateIf m -> m [AnyCompAp]
getQueueImpl sif = withSifState sif $ \st -> liftIO $ do
  q <- readIORef (sifs_stale st)
  let (Paq.PaqView rtq xq rq bq) = Paq.view q
      refs = map Paq.paqe_key (concat [rtq, xq, rq, bq])
  mapM (resolveRefToAny st) refs

staleQueueSizeImpl :: MonadIO m => SimpleStateIf m -> m Int
staleQueueSizeImpl sif = withSifState sif $ \st -> liftIO $ Paq.size <$> readIORef (sifs_stale st)

trackOutputImpl
  :: forall m a
   . (MonadIO m, IsCompResult a)
  => SimpleStateIf m
  -> CompAp a
  -> AnyCompSinkOutsMap
  -> m ()
trackOutputImpl sif cap outputs
  | nullAnyOutsMap outputs = pure ()
  | otherwise = withSifState sif $ \st -> liftIO $
      withRow st cap $ \defIdx _dt row _fresh -> do
        let ref = DT.packRef defIdx row
        logDebug (show (capId cap) ++ " produced the following outputs:\n" ++ indentedShow outputs)
        modifyIORef' (sifs_pendingOutputs st) (Map.insertWith mappend ref outputs)
 where
  indentedShow x = unlines (map ("  - " ++) (lines (show x)))

{- | Commit a row's pending outputs into the durable outputs map, warning
 about (but not preventing) the same output being claimed by more than one
 cap, and reporting any outputs this row *previously* produced but no
 longer does (and that nothing else currently produces either) as garbage.
 This runs unconditionally on every finish, not just when there are
 pending outputs, since a row that produced outputs last time and none
 this time still needs its old ones diffed out. A row with no *current*
 outputs gets 'OM.delete'd rather than given an empty 'OM.insert':
 'OM.lookup's 'Nothing' case already means "no outputs" without needing an
 entry to say so, so an empty entry would just be dead weight in the map.
-}
commitPendingOutputsForKey :: SifState -> DefRef -> AnyCompAp -> IO Garbage
commitPendingOutputsForKey st ref capAny = do
  pending <- readIORef (sifs_pendingOutputs st)
  outs <- readIORef (sifs_outputs st)
  let newOutputs = fromMaybe mempty (Map.lookup ref pending)
      oldOutputs = fromMaybe mempty (OM.lookup ref outs)
      delOutputs = oldOutputs `diffAnyOutsMap` newOutputs
  -- The existential `s` inside each ForAnyCompFlow can't escape a shared
  -- list (different sink outputs may carry different s), so each entry's
  -- outputs (including the IO resolve step) are processed within their
  -- own forM iteration rather than folded together in one comprehension.
  warnMsgLists <-
    forM (Map.elems (unAnyCompSinkOutsMap newOutputs)) $
      \(ForAnyCompFlow ident p (SomeCompSinkOuts outsSet)) ->
        fmap catMaybes $
          forM (F.toList outsSet) $ \output -> do
            let revLookupResult = OM.lookupOutputKey (p, ident, output) outs
            case SL.filter (/= ref) revLookupResult of
              SL.Nil -> pure Nothing
              otherRefs -> do
                otherCaps <- mapM (resolveRefToAny st) (F.toList otherRefs)
                pure $
                  Just
                    ( "MULTIPLE_COMPAPS_ONE_OUTPUT: The output "
                        ++ show output
                        ++ " has just been generated by the comp ap "
                        ++ showAnyCompApDetails capAny
                        ++ ". Before, this output has been generated by the comp ap(s) "
                        ++ intercalate ", " (map showAnyCompApDetails otherCaps)
                        ++ ". "
                        ++ " This 'travelling' of outputs of one comp ap "
                        ++ "to another is outlawed since it can lead to outdated "
                        ++ "outputs. FIX THIS!"
                    )
  let warnMsgs = concat warnMsgLists
  unless (null warnMsgs) (logWarn (unlines warnMsgs))
  let outs' =
        if nullAnyOutsMap newOutputs
          then OM.delete ref outs
          else OM.insert ref newOutputs outs
  -- Forced to WHNF at the write site -- see 'colWrite''s haddock in
  -- "Utils/DefTable.hs" for why a bare 'writeIORef' of a computed value
  -- gives no strictness guarantee on its own ('OutputsMap'\'s own fields
  -- are now strict too, see "Utils/OutputsMap.hs", so WHNF here reaches
  -- both maps).
  writeIORef (sifs_outputs st) $! outs'
  modifyIORef' (sifs_pendingOutputs st) (Map.delete ref)
  let garbOutputs = OM.filterUnreferencedOutputs outs' delOutputs
  pure $
    if nullAnyOutsMap garbOutputs
      then mempty
      else mempty{garbage_outputs = HashMap.singleton capAny garbOutputs}

getDataIfOutputsImpl
  :: forall s m
   . (MonadIO m, CompSink s)
  => SimpleStateIf m
  -> s
  -> m (CompSinkOuts s)
getDataIfOutputsImpl sif s = withSifState sif $ \st -> liftIO $ do
  outs <- readIORef (sifs_outputs st)
  pure $
    F.foldr'
      (\out set -> unionOuts (anyOutputsToCompSinkOutputs out) set)
      HashSet.empty
      (HashMap.elems $ OM.forwardMap outs)
 where
  anyOutputsToCompSinkOutputs :: AnyCompSinkOutsMap -> Maybe (CompSinkOuts s)
  anyOutputsToCompSinkOutputs m = compSinkOutsFromAny (Proxy @s) (compSinkId s) m
  unionOuts Nothing set = set
  unionOuts (Just set1) set2 = set1 `HashSet.union` set2

lookupCapResultImpl
  :: forall m a
   . (MonadIO m, IsCompResult a)
  => SimpleStateIf m
  -> CompAp a
  -> m (CapLookup (CapResult (CapCached a)))
lookupCapResultImpl sif cap = withSifState sif $ \st -> liftIO $
  withRow st cap $ \_defIdx dt row _fresh -> do
    flags <- DT.readFlags dt row
    case DT.flagsResultState flags of
      NoResult -> pure CapNotFound
      ResultFailure -> pure (CapFound CapFailure)
      ResultMetaOnly -> do
        h <- DT.readResultHash dt row
        pure (CapFound (CapSuccess (CapMetaCached (CompCacheMeta h))))
      ResultValue -> do
        h <- DT.readResultHash dt row
        v <- DT.readValue dt row
        let ccv = CompCacheValue (Some v) (CompCacheMeta h)
        pure (CapFound (CapSuccess (CapValueCached (CompApResult v ccv))))

capEvaluationStartedImpl :: (MonadIO m, IsCompResult a) => SimpleStateIf m -> CompAp a -> m ()
capEvaluationStartedImpl sif cap = withSifState sif $ \st -> liftIO $
  withRow st cap $ \_defIdx dt row _fresh -> DT.setPending dt row True

dequeueGivenCapImpl :: (MonadIO m, IsCompResult a) => SimpleStateIf m -> CompAp a -> m Bool
dequeueGivenCapImpl sif cap = withSifState sif $ \st -> liftIO $
  withRow st cap $ \defIdx _dt row _fresh -> do
    let ref = DT.packRef defIdx row
    q <- readIORef (sifs_stale st)
    case Paq.deleteView ref q of
      Some (_ :!: q') -> do
        writeIORef (sifs_stale st) q'
        logDebug
          ( show (capId cap)
              ++ " dequeued, "
              ++ show (Paq.size q')
              ++ " stale caps remaining."
          )
        pure True
      None -> pure False

dequeueNextCapImpl :: MonadIO m => SimpleStateIf m -> m (Maybe AnyCompAp)
dequeueNextCapImpl sif = withSifState sif $ \st -> liftIO $ do
  q <- readIORef (sifs_stale st)
  case Paq.dequeue q of
    None -> pure Nothing
    Some (e :!: q') -> do
      writeIORef (sifs_stale st) q'
      Just <$> resolveRefToAny st (Paq.paqe_key e)

mkPaqEntry :: PaqPriority -> DefRef -> Paq.PaqEntry DefRef ()
mkPaqEntry prio ref = Paq.PaqEntry (Paq.PaqTime 0) prio ref ()

-- | Enqueue a set of refs as stale, skipping any that are currently
-- pending (mid-evaluation): a row already mid-evaluation will re-check its
-- own staleness when it finishes ('finishCap'\'s @selfStale@ check), so
-- enqueueing it again here would be redundant. Priority comes from each
-- ref's own def (@compId_priority@, via the def's 'Comp'). Returns exactly
-- the refs that were *newly* added to the queue (excludes both
-- pending-skipped refs and refs that were already queued) -- the set
-- 'finishCap'\/'notifyDepChange' report back through the public interface.
enqueueRefs :: SifState -> HashSet DefRef -> IO (HashSet DefRef)
enqueueRefs st refs = fmap (HashSet.fromList . catMaybes) $ forM (HashSet.toList refs) $ \ref -> do
  let (defIdx, row) = DT.unpackRef ref
  defs <- readIORef (sifs_defs st)
  case IntMap.lookup (DT.unDefIdx defIdx) defs of
    Nothing -> pure Nothing -- resolved to a dead def; nothing to enqueue
    Just (SomeDefEntry comp dt) -> do
      flags <- DT.readFlags dt row
      if DT.flagsPending flags
        then pure Nothing
        else do
          q <- readIORef (sifs_stale st)
          -- Unlike 'dequeueGivenCapImpl'/'dequeueNextCapImpl' above (whose
          -- 'Paq.deleteView'/'Paq.dequeue' results come through the strict
          -- ':!:' pair, forcing the new queue at the pattern match itself),
          -- 'Paq.enqueue' returns a plain lazy @(,)@ -- this @let@ binds
          -- @q'@ to an unforced thunk that a bare 'writeIORef' would not
          -- force either. Force explicitly (see 'colWrite''s haddock in
          -- "Utils/DefTable.hs"); the queue's strict fields (this project
          -- builds with @-XStrictData@ on by default -- see
          -- "Utils/PriorityAgingQueue.hs") mean WHNF here reaches the
          -- sub-queue actually being mutated, not just the outer record.
          let (how, q') = Paq.enqueue (mkPaqEntry (compId_priority (comp_name comp)) ref) q
          writeIORef (sifs_stale st) $! q'
          pure $ case how of
            Paq.EnqueueAddedNewEntry -> Just ref
            Paq.EnqueueUpdatedEntry -> Nothing

removeFromStale :: SifState -> DefRef -> IO ()
removeFromStale st ref = modifyIORef' (sifs_stale st) (Paq.delete ref)

--
-- Dependency bookkeeping: splitting a public DepSet into comp-dep
-- (target -> observed version) and src-deps, interning any not-yet-seen
-- comp-dep target along the way.
--

splitDeps :: SifState -> DepSet -> IO (IntMap CompDepVer, HashSet AnyCompSrcDep)
splitDeps st deps = foldM step (IntMap.empty, HashSet.empty) (HashSet.toList deps)
 where
  step (!comps, !srcs) dep =
    case dep of
      CompEngDepSrc x -> pure (comps, HashSet.insert x srcs)
      CompEngDepComp (CompDep (Dep (CompDepKey (AnyCompAp targetCap)) observedVer)) -> do
        ref <- withRow st targetCap (\defIdx _dt row _fresh -> pure (DT.packRef defIdx row))
        pure (IntMap.insert (DT.unDefRef ref) observedVer comps, srcs)

--
-- Edge (rdeps/src-index) bookkeeping and garbage collection. None of these
-- need the CPS treatment: each resolves its own SomeDefEntry locally and
-- only ever touches p/a-oblivious columns (flags, hashes, edges).
--

addRdep :: SifState -> DefRef -> DefRef -> IO ()
addRdep st targetRef ref = do
  let (tDef, tRow) = DT.unpackRef targetRef
      refInt = DT.unDefRef ref
  defs <- readIORef (sifs_defs st)
  case IntMap.lookup (DT.unDefIdx tDef) defs of
    Nothing -> pure () -- shouldn't happen: target must be alive, we just depended on it
    Just (SomeDefEntry _ dt) -> do
      rd <- DT.readRdeps dt tRow
      unless (VU.elem refInt rd) $ DT.writeRdeps dt tRow (VU.snoc rd refInt)

-- | Remove @ref@ from a target's rdeps; if the target's rdeps become
-- empty as a result (nothing depends on it any more), cascade-free it --
-- unless it's currently pending (mid-evaluation), in which case it's left
-- alone: something further up the active call stack still has an
-- in-flight, not-yet-recorded intent to depend on it.
removeRdep :: SifState -> DefRef -> DefRef -> IO Garbage
removeRdep st targetRef ref = do
  let (tDef, tRow) = DT.unpackRef targetRef
      refInt = DT.unDefRef ref
  defs <- readIORef (sifs_defs st)
  case IntMap.lookup (DT.unDefIdx tDef) defs of
    Nothing -> pure mempty
    Just (SomeDefEntry _ dt) -> do
      rd <- DT.readRdeps dt tRow
      let rd' = VU.filter (/= refInt) rd
      DT.writeRdeps dt tRow rd'
      if VU.null rd'
        then do
          flags <- DT.readFlags dt tRow
          if DT.flagsPending flags
            then pure mempty
            else freeRowCascade st tDef tRow
        else pure mempty

freeRowCascade :: SifState -> DT.DefIdx -> DT.RowIdx -> IO Garbage
freeRowCascade st defIdx row = do
  defs <- readIORef (sifs_defs st)
  case IntMap.lookup (DT.unDefIdx defIdx) defs of
    Nothing -> pure mempty
    Just (SomeDefEntry _ dt) -> do
      let ref = DT.packRef defIdx row
      capAny <- resolveRefToAny st ref
      paramHash <- DT.readParamHash dt row
      myCompDeps <- DT.compDepTargets <$> DT.readCompDeps dt row
      mySrcDeps <- DT.readSrcDeps dt row
      outs <- readIORef (sifs_outputs st)
      let mOldOutputs = OM.lookup ref outs
          oldOutputs = fromMaybe mempty mOldOutputs
      DT.freeRow dt paramHash row
      let outs' = OM.delete ref outs
      writeIORef (sifs_outputs st) $! outs'
      removeFromStale st ref
      compGarbage <- fmap mconcat $ forM (VU.toList myCompDeps) $ \t -> removeRdep st (DT.mkDefRefUnsafe t) ref
      srcGarbageKeys <- fmap catMaybes $ forM (HashSet.toList mySrcDeps) $ \dep -> removeSrcDependent st dep ref
      -- Only outputs nothing *else* still claims are actually garbage --
      -- e.g. two rows both writing to sink key "output": one dying must
      -- not delete a key the other is still producing. Filtered against
      -- outs' (this row's own entry already removed).
      let garbOutputs = OM.filterUnreferencedOutputs outs' oldOutputs
          outsGarbage =
            if nullAnyOutsMap garbOutputs
              then mempty
              else mempty{garbage_outputs = HashMap.singleton capAny garbOutputs}
      pure
        ( mempty
            { garbage_caps = HashSet.singleton capAny
            , garbage_deps = HashSet.fromList srcGarbageKeys
            }
            <> compGarbage
            <> outsGarbage
        )

-- | Add @ref@ (observed at @dep@'s version) as a dependent of @dep@'s
-- source key, interning the key on first use and creating its arena. A
-- second add for a @ref@ already present in this key's arena is not
-- deduplicated -- see 'SI.skaAppend's haddock for why the real call path
-- never hits that case.
addSrcDependent :: SifState -> AnyCompSrcDep -> DefRef -> IO ()
addSrcDependent st dep ref = do
  let key = depKey dep
      ver = depVer dep
  keyId <- SI.kiIntern (sifs_srcKeyIntern st) key
  entries <- readIORef (sifs_srcEntries st)
  arena <- case IntMap.lookup (SI.unSrcKeyId keyId) entries of
    Just a -> pure a
    Nothing -> do
      a <- SI.newSrcKeyArena
      writeIORef (sifs_srcEntries st) $! IntMap.insert (SI.unSrcKeyId keyId) a entries
      pure a
  SI.skaAppend arena ref ver

-- | Remove @ref@ from a source key's dependent set; if that empties it,
-- drop the key's arena and release its interned id, reporting it as a
-- garbage dep (for 'Control.Computations.CompEngine.Run' to tell the
-- source to unregister it), returning 'Just' that key.
removeSrcDependent :: SifState -> AnyCompSrcDep -> DefRef -> IO (Maybe AnyCompSrcKey)
removeSrcDependent st dep ref = do
  let key = depKey dep
  mKeyId <- SI.kiLookup (sifs_srcKeyIntern st) key
  case mKeyId of
    Nothing -> pure Nothing
    Just keyId -> do
      entries <- readIORef (sifs_srcEntries st)
      case IntMap.lookup (SI.unSrcKeyId keyId) entries of
        Nothing -> pure Nothing
        Just arena -> do
          _ <- SI.skaRemove arena ref
          empty <- SI.skaNull arena
          if empty
            then do
              writeIORef (sifs_srcEntries st) $! IntMap.delete (SI.unSrcKeyId keyId) entries
              SI.kiRelease (sifs_srcKeyIntern st) key
              pure (Just key)
            else pure Nothing

-- | Update a row's forward edge columns (comp-deps with their observed
-- versions, and src-deps) from old to new, diffing to add/remove the
-- corresponding rdeps/src-index entries, cascading garbage collection for
-- anything that becomes unreferenced as a result.
updateEdges
  :: DefTable p a
  -> SifState
  -> DefRef
  -> VU.Vector DT.CompDepEdge
  -- ^ old comp-deps (with observed versions)
  -> IntMap CompDepVer
  -- ^ new comp-deps (target -> observed version)
  -> HashSet AnyCompSrcDep
  -> HashSet AnyCompSrcDep
  -> IO Garbage
updateEdges dt st row oldCompVer newCompVerMap oldSrc newSrc = do
  let oldCompTargets = IntSet.fromList (VU.toList (DT.compDepTargets oldCompVer))
      newCompTargets = IntMap.keysSet newCompVerMap
      addedComp = IntSet.difference newCompTargets oldCompTargets
      removedComp = IntSet.difference oldCompTargets newCompTargets
      addedSrc = HashSet.difference newSrc oldSrc
      removedSrc = HashSet.difference oldSrc newSrc
      newEdges =
        VU.fromList
          [DT.CompDepEdge t (encodeVer v) | (t, v) <- IntMap.toList newCompVerMap]

  DT.writeCompDeps dt (DT.refRow row) newEdges
  DT.writeSrcDeps dt (DT.refRow row) newSrc

  -- Removes before adds, deliberately: removedSrc/addedSrc are diffed on
  -- the *full* (key, version) pair, so a row observing the same source key
  -- at a new version produces both a removal (old key@version) and an
  -- addition (new key@version) for the *same* row against the *same*
  -- se_dependents entry -- but addSrcDependent/removeSrcDependent both key
  -- off the row alone (se_dependents doesn't have room for "the same row,
  -- twice, at two versions"). Adding first and removing second would let
  -- the stale removal wipe the fresh add right back out.
  compGarbage <- fmap mconcat $ forM (IntSet.toList removedComp) $ \t -> removeRdep st (DT.mkDefRefUnsafe t) row
  forM_ (IntSet.toList addedComp) $ \t -> addRdep st (DT.mkDefRefUnsafe t) row

  srcGarbageKeys <- fmap catMaybes $ forM (HashSet.toList removedSrc) $ \dep -> removeSrcDependent st dep row
  forM_ (HashSet.toList addedSrc) $ \dep -> addSrcDependent st dep row

  pure (compGarbage <> mempty{garbage_deps = HashSet.fromList srcGarbageKeys})

checkSelfStale :: SifState -> IntMap CompDepVer -> IO Bool
checkSelfStale st comps = or <$> mapM checkOne (IntMap.toList comps)
 where
  checkOne (refInt, observedVer) = do
    let (tDef, tRow) = DT.unpackRef (DT.mkDefRefUnsafe refInt)
    defs <- readIORef (sifs_defs st)
    case IntMap.lookup (DT.unDefIdx tDef) defs of
      Nothing -> pure False
      Just (SomeDefEntry _ dt) -> do
        curVer <- currentVer dt tRow
        pure (observedVer /= curVer)

currentVer :: DefTable p a -> DT.RowIdx -> IO CompDepVer
currentVer dt row = do
  flags <- DT.readFlags dt row
  case DT.flagsResultState flags of
    ResultFailure -> pure (CompDepVer None)
    NoResult -> pure (CompDepVer None)
    _ -> CompDepVer . Some <$> DT.readResultHash dt row

writeFinishedRow :: DefTable p a -> DT.RowIdx -> ResultState -> Maybe Hash128 -> Maybe a -> IO ()
writeFinishedRow dt row rs mh mres = do
  flags <- DT.readFlags dt row
  DT.writeFlags dt row (DT.mkFlags (DT.flagsAlive flags) (DT.flagsPending flags) rs)
  case mh of
    Just h -> DT.writeResultHash dt row h
    Nothing -> DT.clearResultHash dt row
  case (rs, mres) of
    (ResultValue, Just v) -> DT.writeValue dt row v
    _ -> pure ()

--
-- Finishing a cap's evaluation. See the module haddock for the changed-bit
-- semantics this implements.
--

capEvaluationFinishedImpl
  :: forall m a
   . (MonadIO m, IsCompResult a)
  => SimpleStateIf m
  -> CompAp a
  -> DepSet
  -> Maybe a
  -> m (HashSet AnyCompAp, Garbage)
capEvaluationFinishedImpl sif cap deps mres =
  withSifState sif $ \st -> liftIO $
    withRow st cap $ \defIdx dt row _fresh ->
      finishCap st cap defIdx dt row deps mres

finishCap
  :: forall p a
   . IsCompResult a
  => SifState
  -> CompAp a
  -> DT.DefIdx
  -> DefTable p a
  -> DT.RowIdx
  -> DepSet
  -> Maybe a
  -> IO (HashSet AnyCompAp, Garbage)
finishCap st cap defIdx dt row deps mres = do
  let ref = DT.packRef defIdx row
      capIdStr = show (capId cap)
      capAny = AnyCompAp cap

  oldFlags <- DT.readFlags dt row
  let oldResultState = DT.flagsResultState oldFlags
  oldHash <- if oldResultState == NoResult then pure Nothing else Just <$> DT.readResultHash dt row
  oldCompDepsVer <- DT.readCompDeps dt row
  oldSrcDeps <- DT.readSrcDeps dt row
  -- who's currently watching this row -- read before this call mutates
  -- anything (nothing below touches our *own* rdeps column).
  rdeps <- DT.readRdeps dt row

  (newCompDepsVer, newSrcDeps) <- splitDeps st deps

  -- cap_comp/cap_param aren't usable as plain functions on an existential
  -- CompAp (GHC: "escaped type variables") -- pattern-match instead, same
  -- as withRow/withDefFor do.
  let ccb = case cap of CompAp _ comp _ -> comp_caching comp
      mccv = fmap (ccb_memcache ccb) mres
      newHash = fmap (ccm_largeHash . ccv_meta) mccv
      newResultState = case mccv of
        Nothing -> ResultFailure
        Just ccv -> case ccv_payload ccv of
          Some _ -> ResultValue
          None -> ResultMetaOnly
      oldCompDepsMap =
        IntMap.fromList
          [ (DT.cdeTarget e, decodeVer (DT.cdeObservedVer e))
          | e <- VU.toList oldCompDepsVer
          ]
      sameDeps = oldCompDepsMap == newCompDepsVer && oldSrcDeps == newSrcDeps
      impure = sameDeps && oldResultState /= NoResult && oldHash /= newHash

  when impure $
    logError
      ( "Impure cap "
          ++ capIdStr
          ++ " returned a different result than previously although inputs "
          ++ "didn't change. Not marking anything as stale. New hash: "
          ++ show newHash
          ++ ", old hash: "
          ++ show oldHash
      )

  -- Commit the new value/hash and edge columns unconditionally, before
  -- branching on the impure check below: the row's stored state must
  -- reflect what this evaluation actually produced regardless of whether
  -- it gets flagged impure, so a later read sees the real current value
  -- rather than a stale one just because this run looked suspicious.
  writeFinishedRow dt row newResultState newHash mres
  edgeGarbage <- updateEdges dt st ref oldCompDepsVer newCompDepsVer oldSrcDeps newSrcDeps
  outputGarbage <- commitPendingOutputsForKey st ref capAny
  let garbage = edgeGarbage <> outputGarbage
  removeFromStale st ref
  DT.setPending dt row False

  if impure
    then pure (HashSet.empty, garbage)
    else do
      let resultChanged = oldResultState /= newResultState || oldHash /= newHash
      newlyStale <-
        if resultChanged
          then enqueueRefs st (HashSet.fromList (map DT.mkDefRefUnsafe (VU.toList rdeps)))
          else pure HashSet.empty
      selfStale <- checkSelfStale st newCompDepsVer
      -- Self-enqueue happens (it's the mechanism keeping this cap on the
      -- queue when one of its own deps moved during evaluation), but it
      -- never shows up in the *reported* stale set below, only in the
      -- actual queue: the reported set describes what became stale as a
      -- result of this cap's result changing, not this cap re-enqueueing
      -- itself.
      when selfStale $ void $ enqueueRefs st (HashSet.singleton ref)
      staleCaps <-
        if resultChanged
          then resolveRefsToAny st newlyStale
          else pure HashSet.empty
      pure (staleCaps, garbage)

--
-- Source-change notification (the only place enqueueStaleCaps is actually
-- called with -- see Impl.hs's notifyCompEngine, always CompEngDepSrc).
--

enqueueStaleCapsImpl
  :: (MonadIO m, Foldable t)
  => SimpleStateIf m
  -> t CompEngDep
  -> m EnqueueInfo
enqueueStaleCapsImpl sif deps = withSifState sif $ \st -> liftIO $ do
  affected <- forM (F.toList deps) $ \dep -> do
    stale <- notifyDepChange st dep
    pure (dep, stale)
  let affectedStaleCaps = mkRecompInfoMap affected
  qsize <- Paq.size <$> readIORef (sifs_stale st)
  pure EnqueueInfo{ei_affectedCaps = affectedStaleCaps, ei_currentQueueSize = qsize}

-- | 'CompEngDepComp' is unreachable in practice (see the haddock above),
-- handled defensively (no-op) rather than erroring: it's not a bug if hit,
-- just unused by anything today.
notifyDepChange :: SifState -> CompEngDep -> IO (HashSet AnyCompAp)
notifyDepChange _st (CompEngDepComp _) = pure HashSet.empty
notifyDepChange st (CompEngDepSrc srcDep) = do
  let key = depKey srcDep
      newVer = depVer srcDep
  mKeyId <- SI.kiLookup (sifs_srcKeyIntern st) key
  case mKeyId of
    Nothing -> pure HashSet.empty
    Just keyId -> do
      entries <- readIORef (sifs_srcEntries st)
      case IntMap.lookup (SI.unSrcKeyId keyId) entries of
        Nothing -> pure HashSet.empty
        Just arena -> do
          -- Invalidate only the dependents whose *own* recorded observation
          -- differs from the new version -- not "everyone who depends on
          -- this key" (see the reverse-index haddock on 'SifState' for why
          -- that's required, not just a nicety). Deliberately does not
          -- write the new version back into the arena: a dependent's
          -- recorded version only moves when *it* re-registers via a real
          -- finish (updateEdges), not merely because a notification went
          -- by.
          pairs <- SI.skaToList arena
          let staleRefs = HashSet.fromList [r | (r, v) <- pairs, v /= newVer]
          -- Actually enqueue (the side effect, internally pending-aware),
          -- but report the full stale-relative-to-this-notification set
          -- regardless of whether each one was already queued or is
          -- mid-evaluation: 'EnqueueInfo' answers "who's affected by this
          -- change", not "who did this call newly enqueue". Contrast
          -- 'finishCap'\'s @staleCaps@, which answers the latter and does
          -- use 'enqueueRefs'\'s filtered return.
          void $ enqueueRefs st staleRefs
          resolveRefsToAny st staleRefs
