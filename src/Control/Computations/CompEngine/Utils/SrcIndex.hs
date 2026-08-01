{-# LANGUAGE TypeFamilies #-}

{- | Columnar storage for the reverse source index
 ("SimpleStateIf.hs"'s @sifs_srcIndex@): for every source key currently
 depended on by at least one row, the set of dependent rows and each
 dependent's own observed version of that key. This is the mirror of
 "DefTable.hs"'s "Src-dep interning" section on the /forward/ side (a row's
 own @dt_srcDeps@ column) -- the same underlying problem (a boxed
 existential, "AnyCompSrcKey"/"AnyCompSrcVer", used as a hot mutable-table
 key or value) attacked from the reverse direction, and mostly the same
 tools, but not an identical structure -- see "Why this isn't just another
 EdgeArena" below for why.

 This haddock documents the *design*: what's here and the invariants it
 maintains (see the "Invariants (summary)" section at the end for the quick
 checklist).

 = Two tables, one per existential

 'KeyIntern' interns 'AnyCompSrcKey' (which source key) to a dense
 'SrcKeyId'; 'SrcKeyArena' stores, per interned key id, that key's current
 set of @(DefRef, AnyCompSrcVer)@ dependents. Above one dependent, that set
 is a flat pair of columns -- unboxed 'DefRef', boxed (parallel,
 index-aligned) 'AnyCompSrcVer'; at zero or one dependents (the common case
 for one of this module's two target workloads -- see "One dependent is the
 common case" below) it is nothing more than an inline pair, no vector
 allocated at all.

 ASCII sketch: source key @"k"@ interned to id 3, currently depended on by
 two rows (packed refs @r7@ and @r12@), each having last observed a
 different version of @"k"@:

 >  KeyIntern (AnyCompSrcKey -> SrcKeyId):
 >    "k" -> 3   (ki_forward)                    ki_count = 4, ki_free = []
 >
 >  sifs_srcEntries :: IntMap SrcKeyId(as Int) SrcKeyArena
 >    3 -> SrcKeyArena { ska_refs = [r7, r12], ska_vers = [v_old, v_new] }
 >
 >  A change notification carrying "k"@v_new arriving at this SrcKeyArena
 >  invalidates only the dependent whose *own* recorded version doesn't
 >  match (r7, still at v_old) -- not both, and not by key identity alone.

 = Why 'KeyIntern' needs no refcounting

 Interning a /version-carrying/ value without a reclaim path leaks, because
 a (key, version) pair can be referenced by many rows at once and only some
 of them stop referencing it at a time -- nothing short of a genuine
 reference count knows when the last one let go. 'KeyIntern' sidesteps that
 by interning 'AnyCompSrcKey' alone (no version component), and -- unlike
 "DefTable.hs"'s @SrcDepIntern@, whose ids are retained independently by
 every row that currently lists them in its own @dt_srcDeps@ span -- a key
 id here has exactly one owner at a time: the single 'SrcKeyArena' this
 module creates for it on first use and deletes on last-dependent-removed
 (below). Ownership is 1:1, not many:1, so a plain assign-on-first-use /
 recycle-on-last-removal lifecycle is the *correct* match for this shape,
 not a shortcut that happens to avoid refcounting -- there is only ever one
 thing to ask "is anyone still holding this?", and it already exists
 ('SimpleStateIf.removeSrcDependentKey'\'s own "did this key's dependent set
 just become empty?" check). Interning 'AnyCompSrcVer' *would* reopen the
 many:1 sharing problem (the same version value can be the current
 observation of many different rows at once) -- which is exactly why this
 module does not do that; see "Why the version is a boxed side column, not
 interned" below.

 = Why the version is a boxed side column, not interned

 The alternative design -- intern 'AnyCompSrcVer' too, refcounted like
 'DefTable.hs'\'s @SrcDepIntern@ -- was considered and rejected, for two
 reasons about where the actual value duplication lives. On the /forward/
 side, a src-dep set's apparent heavy duplication is an artifact of every
 row's set being freshly reconstructed on every call ('wrapCompSrcDep'
 rebuilding a 'CompSrcId'\/'Text' each time), not genuinely many distinct
 rows sharing identical values (see "DefTable.hs"'s own discussion of
 @SrcDepIntern@). That justification does not transfer to the /reverse/
 side: a source key's dependent set spans rows that last observed it at
 *different* times (that is the entire reason 'SrcKeyArena' tracks a
 version /per dependent/ rather than one shared "current version" for the
 key: see @test_modifcationWhileWorkingOnQueue@), so the real
 distinct-version count among one key's dependents is workload-dependent,
 not a known-small constant the way the forward side's snapshot-instant
 duplication is. Second, and decisive: interning here would reintroduce
 exactly the many:1 sharing 'KeyIntern' above is designed to avoid -- a
 version value can be the current observation of many rows simultaneously,
 so reclaiming it needs the same refcount discipline 'DefTable.hs'\'s
 @SrcDepIntern@ already carries, for a payoff that (unlike the forward
 side, and unlike keys, which are typically few and stable) is not
 established to be there. A boxed side column costs one boxed word per
 currently-live dependent -- exactly what a @HashMap DefRef AnyCompSrcVer@
 would cost per entry, just outside a HAMT leaf -- with none of a second
 refcounted table's bookkeeping or failure modes.

 = A boxed side column needs to force its own writes

 A boxed column built directly on @Data.Vector.Mutable@ is not a neutral
 stand-in for a @Data.HashMap.Strict@: the latter forces its value on
 every insert, while @Data.Vector.Mutable.write@ carries no such contract
 and will happily store an unevaluated thunk. Left lazy, 'skaAppend'\'s
 @ver@ argument would be a thunk closing over its caller's whole
 'Control.Computations.CompEngine.CompSrc.AnyCompSrcDep' (and,
 transitively, the 'DepSet' it came from) -- and nothing would force it
 during cold eval, since nothing reads a version back out of the arena
 until the live phase's 'SimpleStateIf.notifyDepChange' compares it. Left
 unforced, every dependent's evaluation closure would stay reachable for
 the entire cold-eval run even though nothing will ever look at it again --
 a pure space leak, invisible to every type signature and every test in
 this module, since semantics are never wrong, only retention is.
 'skaAppend' forces @ver@ to WHNF before storing it (down through
 'AnyCompSrcVer'\'s 'StrictData' fields, which is sufficient -- see its own
 haddock); the lesson generalizes to any boxed column in this codebase
 built directly on @Data.Vector.Mutable@ rather than a @Strict@ container.

 = Why this isn't just another EdgeArena

 "DefTable.hs"'s @EdgeArena@ (compDeps\/rdeps\/srcDeps) is shaped around a
 single owner rewriting its *entire* span at once (a row always replaces
 all of its edges in one 'eaWrite' call) -- append-new-span,
 orphan-old-span, amortize via periodic compaction. The reverse source
 index has a different access pattern: many different rows mutate the
 *same* key's dependent list one entry at a time
 ('SimpleStateIf.addSrcDependent'\/'removeSrcDependentKey', each handling one
 key\/ref pair at a time, called from a single fold over a row's newly
 diffed source deps), so there is no "whole span" to replace and 'EdgeArena'\'s
 machinery does not fit. 'SrcKeyArena'\'s many-dependents representation
 ('ManyArena', below) is the structure that /does/ fit single-entry
 mutation: a plain growable unboxed\/boxed column pair per key with append
 at the tail and O(current key size) linear-scan-then-swap-with-last
 removal -- no tombstones, no compaction pass, because a swap-remove leaves
 no dead space to reclaim in the first place. The O(current key size) scan
 is the one deliberate complexity tradeoff here (a hand-rolled per-key
 secondary index mapping 'DefRef' to slot would make removal O(1) at the
 cost of a second boxed structure per key, undoing much of the point) --
 accepted because the persistence benchmark shipped with this library (see
 @bench\/.../Bench\/Main.hs@) exercises roughly 683 dependents per key
 averaged across 300 keys, which keeps a linear scan cheap in practice, and
 because the scan operates on an unboxed 'Int' column, not a boxed
 structure -- see the module's own churn tests for the bound this actually
 achieves.

 = One dependent is the common case, not the exception

 The 683-dependents-per-key shape above is the workload 'ManyArena' was
 designed against, and it is not the only shape this module has to serve.
 A second benchmark (the "hospital" variant) creates roughly 1.6M distinct
 source keys -- one per (patient, observation, field) triple, since vitals
 split value\/unit\/reference-range into three separate keys, labs split
 result\/range\/specimen into three more, and so on -- each read by exactly
 one computation. That workload never touches the many-dependents path at
 all, yet before this section's optimisation it paid for one unconditionally
 whether it needed it or not: every 'SrcKeyArena', regardless of how many
 dependents its key ever actually had, was three 'IORef's wrapping two
 growable vectors. At ~1.6M keys averaging ~1 dependent, that structure
 dominates the reverse index's live memory -- by a wide margin the largest
 identified item in a benchmark suite built specifically to surface exactly
 this kind of thing.

 'SrcKeyArena' is therefore a small-size optimisation over three states, not
 the vector-backed structure directly:

 > data SrcKeyRep
 >   = SrcKeyZero
 >   | SrcKeyOne !DefRef !AnyCompSrcVer
 >   | SrcKeyMany !ManyArena
 >
 > newtype SrcKeyArena = SrcKeyArena (IORef SrcKeyRep)

 'SrcKeyZero' is the state 'newSrcKeyArena' creates, and the state
 'skaRemove' returns a key to once its lone dependent is removed. It exists
 as a real representable state -- rather than, say, 'sifs_srcEntries' simply
 not holding an entry for the key yet -- because "SimpleStateIf.hs" keeps
 one stable 'SrcKeyArena' handle per interned key across that key's whole
 create-populate-drain lifecycle (see 'addSrcDependent'\/
 'removeSrcDependentKey'\/'updateSrcDependentVersion'): the handle is
 created and stored in @sifs_srcEntries@ once, on the key's first
 dependent, and every subsequent add\/remove\/update on that key reads the
 same handle back out and mutates it in place, with no further @IntMap@
 write -- deliberately, since @sifs_srcEntries@ can hold on the order of a
 million entries on the hospital workload, and an @IntMap.insert@ on every
 dependent add\/remove would be a ~20-node path copy each time instead of
 one @IORef@ write. A handle that already exists (interned, stored) but
 whose dependent set is momentarily empty is therefore a state this design
 has to represent, not an edge case it can avoid by construction.

 'SrcKeyOne' holds a single dependent inline -- no vector, no extra
 'IORef', just the @(DefRef, AnyCompSrcVer)@ pair itself, which is exactly
 what the hospital workload's typical key never needs more than one of.
 'SrcKeyMany' is the old always-allocated structure, renamed to 'ManyArena'
 but otherwise unchanged, reached only once a *second* distinct dependent
 arrives for the same key -- the shape the 683-dependents-per-key benchmark
 above exercises from its second dependent onward, at the cost of one extra
 'IORef' dereference and pattern match per operation versus the old
 unconditional-vector design, and nothing else.

 Promotion ('SrcKeyZero' -> 'SrcKeyOne' -> 'SrcKeyMany') is one-way in this
 version: once a key reaches 'SrcKeyMany' it stays there even if it later
 drains back down to one or zero live dependents, until the *whole* arena
 is discarded (see 'skaRemove' and 'removeSrcDependentKey'). Demoting
 'SrcKeyMany' back to 'SrcKeyOne' on shrink was considered and deliberately
 not done: a workload whose keys oscillate between one and two dependents
 would pay a repeated promote\/demote allocation for no lasting benefit,
 and there is no evidence of such a workload among this module's own
 benchmarks or tests -- so the complexity (and allocation churn) of
 tracking that transition is left undone rather than spent on a shape
 nothing here exercises. 'SrcKeyOne' -> 'SrcKeyZero' on removal costs
 nothing to do, by contrast (no vector to keep or discard either way), so
 'skaRemove' always performs that one.

 = Invariants (summary)

 __Key interning ('KeyIntern'):__

 * A key id is live iff it currently has a forward-map entry; release
   deletes the entry and pushes the id onto a free list for reuse -- no
   refcounting, because ownership here is 1:1 (see "Why KeyIntern needs no
   refcounting" above), unlike 'DefTable.hs'\'s @SrcDepIntern@.
 * A caller only ever releases a key id after its own bookkeeping
   ('SimpleStateIf.removeSrcDependentKey') has confirmed the key's dependent
   arena just became empty -- 'kiRelease' on a key with a live dependent is
   always a lifecycle bug, not a legitimate state.

 __Per-key dependent arena ('SrcKeyArena'):__

 * A freshly created arena ('newSrcKeyArena') starts as 'SrcKeyZero'. The
   first 'skaAppend' promotes it to 'SrcKeyOne'; a second 'skaAppend' for a
   different ref promotes it to 'SrcKeyMany' -- see "One dependent is the
   common case" above. Every operation dispatches on the current state and
   is observably equivalent to the single-representation version of this
   module in every case except which branch runs and what it allocates.
 * 'skaAppend' does not deduplicate -- a second append for a ref already
   present in the arena adds a second entry. Safe because 'skaAppend' is
   only ever reached for a source key a row was *not* already depending on:
   "SimpleStateIf.hs"'s @updateEdges@ diffs old vs. new source deps by key,
   not by @(key, version)@, so "genuinely new key" and "same key, new
   version" are distinguished at the call site, and only the former routes
   here -- the latter goes through 'skaUpdateVer' instead, which overwrites
   the existing entry's version in place and never appends.
 * 'skaRemove' on 'SrcKeyMany' locates the entry by linear scan
   ('ManyArena''s own, unchanged) and removes it swap-with-last -- order is
   not part of this structure's contract. It never demotes 'SrcKeyMany'
   back to 'SrcKeyOne' or 'SrcKeyZero', even when the removal drains it to
   one or zero live entries -- see "One dependent is the common case"
   above for why not. 'skaRemove' on 'SrcKeyOne' does demote, straight to
   'SrcKeyZero', because unlike the 'SrcKeyMany' case there is no vector to
   either keep or discard either way -- the demotion is free, and it is
   what lets 'skaNull' report the arena empty so its caller can discard it.
 * 'skaUpdateVer' locates the entry (a direct match against 'SrcKeyOne''s
   single ref, or the same linear scan 'skaRemove' uses for 'SrcKeyMany'),
   then overwrites its version in place -- no length change, no swap, no
   'KeyIntern' churn, no representation transition. Caller's responsibility:
   @ref@ must already be present (every real call site's own by-key diff
   guarantees this -- see above); a miss returns 'False' rather than
   inserting.
 * __Every write through 'skaAppend' or 'skaUpdateVer' must force @ver@ to
   WHNF.__ Both do this themselves (the @!ver@ bang) precisely because
   'Data.Vector.Mutable.write' carries no strictness contract of its own --
   see "A boxed side column needs to force its own writes" above; this is
   the one invariant on this list that is not obvious from the types alone,
   which is why it is called out here explicitly rather than left implicit.
-}
module Control.Computations.CompEngine.Utils.SrcIndex (
  -- * Key interning
  SrcKeyId,
  unSrcKeyId,
  mkSrcKeyId,
  KeyIntern,
  newKeyIntern,
  kiIntern,
  kiLookup,
  kiRelease,
  kiLiveCount,
  kiAssignedCount,

  -- * Per-key dependent arena
  SrcKeyArena,
  newSrcKeyArena,
  skaAppend,
  skaRemove,
  skaUpdateVer,
  skaToList,
  skaNull,
  skaLiveCount,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompSrc (AnyCompSrcKey, AnyCompSrcVer)
import Control.Computations.CompEngine.Utils.DefTable (DefRef)

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Monad (when)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed.Mutable as VUM

--
-- Key interning -- see the module haddock's "Why KeyIntern needs no
-- refcounting" section.
--

-- | An id 'KeyIntern' has assigned to some 'AnyCompSrcKey'. The public
-- boundary type for 'kiIntern'\/'kiLookup'\/'kiRelease'; "SimpleStateIf.hs"
-- unwraps it via 'unSrcKeyId' at exactly one seam -- @sifs_srcEntries@,
-- which is 'Data.IntMap.Strict'-keyed and so (like 'DefTable.hs'\'s
-- @sifs_defs@ and 'DefIdx') needs a literal 'Int', not because
-- 'SrcKeyId' itself is unboxed storage.
newtype SrcKeyId = SrcKeyId Int
  deriving (Eq, Ord, Show)

unSrcKeyId :: SrcKeyId -> Int
unSrcKeyId (SrcKeyId i) = i
{-# INLINE unSrcKeyId #-}

mkSrcKeyId :: Int -> SrcKeyId
mkSrcKeyId = SrcKeyId
{-# INLINE mkSrcKeyId #-}

data KeyIntern = KeyIntern
  { ki_forward :: !(IORef (HashMap AnyCompSrcKey Int))
  , ki_count :: !(IORef Int)
  -- ^ one past the highest id ever assigned -- test/debug-only
  -- ('kiAssignedCount'); not consulted by any lookup.
  , ki_free :: !(IORef [Int])
  }

newKeyIntern :: IO KeyIntern
newKeyIntern = KeyIntern <$> newIORef HashMap.empty <*> newIORef 0 <*> newIORef []

-- | Pure lookup -- does not assign an id on a miss. Callers that must not
-- create a mapping as a side effect of a failed lookup (everything except
-- 'kiIntern' itself) use this.
kiLookup :: KeyIntern -> AnyCompSrcKey -> IO (Maybe SrcKeyId)
kiLookup ki key = fmap mkSrcKeyId . HashMap.lookup key <$> readIORef (ki_forward ki)

-- | Look up @key@'s interned id, assigning one (popping the free list
-- first, only extending 'ki_count' once it's empty) on a miss.
kiIntern :: KeyIntern -> AnyCompSrcKey -> IO SrcKeyId
kiIntern ki key = do
  fwd <- readIORef (ki_forward ki)
  case HashMap.lookup key fwd of
    Just i -> pure (mkSrcKeyId i)
    Nothing -> do
      free <- readIORef (ki_free ki)
      i <- case free of
        (i : rest) -> writeIORef (ki_free ki) rest >> pure i
        [] -> do
          i <- readIORef (ki_count ki)
          writeIORef (ki_count ki) (i + 1)
          pure i
      -- Forced to WHNF at the write site -- see DefTable.hs's 'colWrite'
      -- haddock for why a bare 'writeIORef' of a computed 'HashMap' update
      -- is not, on its own, a strictness guarantee.
      writeIORef (ki_forward ki) $! HashMap.insert key i fwd
      pure (mkSrcKeyId i)

-- | Release @key@'s interned id: delete the forward entry and push the id
-- onto the free list for 'kiIntern' to reuse. Caller's responsibility (like
-- 'DefTable.hs'\'s @sdiRelease@'s refcount-underflow check): @key@ is
-- currently interned -- every call site only reaches this after confirming
-- (via its own dependent-count bookkeeping) that nothing references the key
-- any more, so a miss here is always a lifecycle bug, not a legitimate
-- runtime occurrence.
kiRelease :: KeyIntern -> AnyCompSrcKey -> IO ()
kiRelease ki key = do
  fwd <- readIORef (ki_forward ki)
  case HashMap.lookup key fwd of
    Nothing ->
      error "Utils.SrcIndex.kiRelease: releasing a key that was never interned (or already released) -- a bug"
    Just i -> do
      writeIORef (ki_forward ki) $! HashMap.delete key fwd
      modifyIORef' (ki_free ki) (i :)

-- | Test/debug-only: number of currently-interned keys.
kiLiveCount :: KeyIntern -> IO Int
kiLiveCount ki = HashMap.size <$> readIORef (ki_forward ki)

-- | Test/debug-only: total ids ever assigned (not decremented by release) --
-- the churn tests use this to confirm ids are actually being recycled
-- rather than growing once per release/reassign cycle.
kiAssignedCount :: KeyIntern -> IO Int
kiAssignedCount ki = readIORef (ki_count ki)

--
-- Per-key dependent arena -- see the module haddock's "Why this isn't just
-- another EdgeArena" section.
--

-- | The vector-backed "two or more dependents" representation -- what every
-- 'SrcKeyArena' used to be, unconditionally, before the small-size
-- optimisation described in the module haddock's "One dependent is the
-- common case" section. @ma_refs@ stores each dependent's 'DefRef'
-- directly: 'DefRef' has a real 'Data.Vector.Unboxed.Unbox' instance (via
-- vector's 'Data.Vector.Unboxed.UnboxViaPrim' -- see "DefTable.hs"'s
-- 'Control.Computations.CompEngine.Utils.DefTable.unDefRef' haddock for the
-- recipe), so this column stores 'DefRef' values directly rather than a raw
-- 'Int' that would need coercing at every read\/write.
data ManyArena = ManyArena
  { ma_refs :: !(IORef (VUM.IOVector DefRef))
  , ma_vers :: !(IORef (VM.IOVector AnyCompSrcVer))
  , ma_len :: !(IORef Int)
  }

newManyArena :: IO ManyArena
newManyArena =
  ManyArena
    <$> (newIORef =<< VUM.new 0)
    <*> (newIORef =<< VM.new 0)
    <*> newIORef 0

-- | Grow both columns in lockstep to at least @needed@ slots. Freshly grown
-- capacity in the boxed column is the @vector@ package's own
-- uninitialised-element error thunk; safe because every slot below
-- 'ma_len' is written by 'maAppend' before it is ever read.
maGrowBoth :: ManyArena -> Int -> IO ()
maGrowBoth ma needed = do
  rv <- readIORef (ma_refs ma)
  let cap = GM.length rv
  when (needed > cap) $ do
    let extra = max (needed - cap) (max 4 cap)
    rv' <- GM.unsafeGrow rv extra
    writeIORef (ma_refs ma) rv'
    vv <- readIORef (ma_vers ma)
    vv' <- GM.unsafeGrow vv extra
    writeIORef (ma_vers ma) vv'

-- | Append one @(ref, ver)@ entry. Caller's responsibility: if @ref@ is
-- already present, this appends a *second* entry rather than updating the
-- first -- see 'skaAppend's haddock, which this is the 'SrcKeyMany' half
-- of. @ver@ must already be forced to WHNF by the caller (both 'maAppend'
-- call sites, inside 'skaAppend', pass a @ver@ that 'skaAppend' itself
-- already forced via its own @!ver@ bang, per the module haddock's "A boxed
-- side column needs to force its own writes" section).
maAppend :: ManyArena -> DefRef -> AnyCompSrcVer -> IO ()
maAppend ma ref ver = do
  len <- readIORef (ma_len ma)
  maGrowBoth ma (len + 1)
  rv <- readIORef (ma_refs ma)
  vv <- readIORef (ma_vers ma)
  VUM.write rv len ref
  VM.write vv len ver
  writeIORef (ma_len ma) (len + 1)

-- | Remove the entry for @ref@ via linear scan, then swap-with-last (no
-- tombstone -- a swap-remove leaves no gap to reclaim later, unlike
-- 'EdgeArena'\'s append-orphan-compact scheme, which this module
-- deliberately does not need -- see the module haddock). Returns 'True' iff
-- an entry was found and removed. Does not, and cannot on its own, shrink
-- @ma@ back to a 'SrcKeyOne'\/'SrcKeyZero' representation even if this
-- removal drains it to one or zero entries -- that is 'skaRemove's call to
-- make (it doesn't -- see the module haddock's "One dependent is the common
-- case" section for why).
maRemove :: ManyArena -> DefRef -> IO Bool
maRemove ma ref = do
  len <- readIORef (ma_len ma)
  rv <- readIORef (ma_refs ma)
  let go i
        | i >= len = pure Nothing
        | otherwise = do
            r <- VUM.read rv i
            if r == ref then pure (Just i) else go (i + 1)
  found <- go 0
  case found of
    Nothing -> pure False
    Just i -> do
      let lastIdx = len - 1
      when (i /= lastIdx) $ do
        lastRef <- VUM.read rv lastIdx
        VUM.write rv i lastRef
        vv <- readIORef (ma_vers ma)
        lastVer <- VM.read vv lastIdx
        VM.write vv i lastVer
      writeIORef (ma_len ma) lastIdx
      pure True

-- | Overwrite the stored version for @ref@'s existing entry in place.
-- Locates the entry via the same linear scan 'maRemove' performs, but --
-- unlike 'maRemove' followed by 'maAppend' -- reuses the slot rather than
-- shrinking then regrowing the arena: no swap-with-last, no length change.
-- This is strictly less work than a remove/re-add pair for the same
-- @(key, row)@: one linear scan instead of two, one boxed write instead of
-- two (plus, when the removed entry wasn't the arena's last, 'maRemove'\'s
-- own swap writes a second time), and zero unboxed 'ma_refs' writes since
-- @ref@ itself never changes. Returns 'False' (rather than inserting) if
-- @ref@ is not present -- see 'skaUpdateVer's haddock, which this is the
-- 'SrcKeyMany' half of.
maUpdateVer :: ManyArena -> DefRef -> AnyCompSrcVer -> IO Bool
maUpdateVer ma ref ver = do
  len <- readIORef (ma_len ma)
  rv <- readIORef (ma_refs ma)
  let go i
        | i >= len = pure Nothing
        | otherwise = do
            r <- VUM.read rv i
            if r == ref then pure (Just i) else go (i + 1)
  found <- go 0
  case found of
    Nothing -> pure False
    Just i -> do
      vv <- readIORef (ma_vers ma)
      VM.write vv i ver
      pure True

-- | Every currently-live @(ref, ver)@ pair, in unspecified order: order is
-- not part of this structure's contract, and 'maRemove'\'s swap-with-last
-- deliberately does not preserve insertion order.
maToList :: ManyArena -> IO [(DefRef, AnyCompSrcVer)]
maToList ma = do
  len <- readIORef (ma_len ma)
  rv <- readIORef (ma_refs ma)
  vv <- readIORef (ma_vers ma)
  mapM (\i -> (,) <$> VUM.read rv i <*> VM.read vv i) [0 .. len - 1]

maNull :: ManyArena -> IO Bool
maNull ma = (== 0) <$> readIORef (ma_len ma)

maLiveCount :: ManyArena -> IO Int
maLiveCount ma = readIORef (ma_len ma)

--
-- The public arena API -- a small-size optimisation switching between the
-- three 'SrcKeyRep' states over the 'ManyArena' machinery above. See the
-- module haddock's "One dependent is the common case" section for the two
-- workload shapes this exists to serve and why promotion is one-way.
--

-- | The state a 'SrcKeyArena' is in: no dependents yet (or drained back to
-- none), exactly one (held inline, no vector), or two-or-more (the
-- vector-backed 'ManyArena').
data SrcKeyRep
  = SrcKeyZero
  | SrcKeyOne !DefRef !AnyCompSrcVer
  | SrcKeyMany !ManyArena

-- | One stable mutable handle per interned source key, held by
-- "SimpleStateIf.hs"'s @sifs_srcEntries@ for that key's whole
-- create-populate-drain lifecycle -- see the module haddock. The
-- representation behind the handle changes as the key's dependent count
-- crosses zero and one; the handle itself never does.
newtype SrcKeyArena = SrcKeyArena (IORef SrcKeyRep)

newSrcKeyArena :: IO SrcKeyArena
newSrcKeyArena = SrcKeyArena <$> newIORef SrcKeyZero

-- | Append one @(ref, ver)@ entry, forcing @ver@ to WHNF before storing it.
--
-- Caller's responsibility: if @ref@ is already present, this appends a
-- *second* entry rather than updating the first. Every real call site only
-- reaches 'skaAppend' for a key the row was not already depending on --
-- "SimpleStateIf.hs"'s @updateEdges@ diffs old vs. new source deps by key,
-- so a dependent re-registering the *same* key at a new version is routed
-- to 'skaUpdateVer' instead (overwrite in place, never append) -- so a live
-- duplicate never actually occurs on the real call path; a direct
-- 'skaAppend' misuse would silently double-count rather than fail loudly,
-- which is why the module's own tests exercise the by-key routing this
-- depends on directly rather than trusting the invariant silently.
--
-- Forcing @ver@ is not optional. @AnyCompSrcVer@'s constructor, like every
-- constructor in this codebase, has strict fields by default
-- ('StrictData'), so WHNF is enough to fully evaluate it -- but nothing
-- forces @ver@ to WHNF in the first place unless the write site does it:
-- @Data.Vector.Mutable.write@ carries no strictness contract of its own
-- and will happily store an unevaluated thunk (unlike a
-- @Data.HashMap.Strict@, which forces its value on every insert), and a
-- bare 'writeIORef' of an 'SrcKeyOne' built from an unforced @ver@ is no
-- better. Left lazy, that thunk would close over its caller's whole
-- @AnyCompSrcDep@ (and, transitively, the 'DepSet'\/'HashSet' it came
-- from) and never get forced during cold eval at all, since nothing reads
-- a version back out until the live phase's
-- 'SimpleStateIf.notifyDepChange' compares it -- meaning every dependent's
-- evaluation closure would stay reachable for the entire cold-eval run for
-- no reason. That is exactly the shape of space leak no type signature can
-- catch: semantics stay correct (the same instance\/rerun counts either
-- way), only retention is wrong.
skaAppend :: SrcKeyArena -> DefRef -> AnyCompSrcVer -> IO ()
skaAppend (SrcKeyArena rep) ref !ver = do
  r <- readIORef rep
  case r of
    SrcKeyZero -> writeIORef rep (SrcKeyOne ref ver)
    SrcKeyOne ref0 ver0 -> do
      -- Second dependent for this key -- promote. ver0 was already forced
      -- to WHNF when it was stored (this same function, on the previous
      -- call), so re-passing it to maAppend needs no further forcing.
      ma <- newManyArena
      maAppend ma ref0 ver0
      maAppend ma ref ver
      writeIORef rep (SrcKeyMany ma)
    SrcKeyMany ma -> maAppend ma ref ver

-- | Remove the entry for @ref@. Returns 'True' iff an entry was found and
-- removed; a caller that expects @ref@ to be present (there isn't one here
-- -- see 'SimpleStateIf.removeSrcDependentKey', which tolerates a miss
-- deliberately) can check the result.
--
-- On 'SrcKeyOne', a match demotes straight to 'SrcKeyZero' -- free to do,
-- since there is no vector to keep or discard either way, and it's what
-- lets 'skaNull' report the arena empty afterwards. On 'SrcKeyMany', this
-- delegates to 'maRemove' (linear-scan-then-swap-with-last) and
-- deliberately does *not* demote back to 'SrcKeyOne' or 'SrcKeyZero' even
-- when the removal drains it to one or zero live entries -- see the module
-- haddock's "One dependent is the common case" section for why not.
skaRemove :: SrcKeyArena -> DefRef -> IO Bool
skaRemove (SrcKeyArena rep) target = do
  r <- readIORef rep
  case r of
    SrcKeyZero -> pure False
    SrcKeyOne ref0 _
      | ref0 == target -> writeIORef rep SrcKeyZero >> pure True
      | otherwise -> pure False
    SrcKeyMany ma -> maRemove ma target

-- | Overwrite the stored version for @ref@'s existing entry in place,
-- forcing @ver@ to WHNF before storing it (same strictness requirement as
-- 'skaAppend' -- see its haddock). No length change, no swap, no
-- 'KeyIntern' round trip, no representation transition, regardless of
-- which state the arena is in.
--
-- Returns 'False' (rather than inserting) if @ref@ is not present. Every
-- real call site ("SimpleStateIf.hs"'s @updateEdges@, via
-- @updateSrcDependentVersion@) only reaches this after its own by-key diff
-- has confirmed @ref@ already depends on this key, so a 'False' result
-- there is a lifecycle bug, not a legitimate runtime occurrence.
skaUpdateVer :: SrcKeyArena -> DefRef -> AnyCompSrcVer -> IO Bool
skaUpdateVer (SrcKeyArena rep) target !ver = do
  r <- readIORef rep
  case r of
    SrcKeyZero -> pure False
    SrcKeyOne ref0 _
      | ref0 == target -> writeIORef rep (SrcKeyOne ref0 ver) >> pure True
      | otherwise -> pure False
    SrcKeyMany ma -> maUpdateVer ma target ver

-- | Every currently-live @(ref, ver)@ pair, in unspecified order: order is
-- not part of this structure's contract, and 'SrcKeyMany's swap-with-last
-- removal deliberately does not preserve insertion order.
skaToList :: SrcKeyArena -> IO [(DefRef, AnyCompSrcVer)]
skaToList (SrcKeyArena rep) = do
  r <- readIORef rep
  case r of
    SrcKeyZero -> pure []
    SrcKeyOne ref0 ver0 -> pure [(ref0, ver0)]
    SrcKeyMany ma -> maToList ma

skaNull :: SrcKeyArena -> IO Bool
skaNull (SrcKeyArena rep) = do
  r <- readIORef rep
  case r of
    SrcKeyZero -> pure True
    SrcKeyOne _ _ -> pure False
    SrcKeyMany ma -> maNull ma

-- | Test/debug-only: current live entry count.
skaLiveCount :: SrcKeyArena -> IO Int
skaLiveCount (SrcKeyArena rep) = do
  r <- readIORef rep
  case r of
    SrcKeyZero -> pure 0
    SrcKeyOne _ _ -> pure 1
    SrcKeyMany ma -> maLiveCount ma
