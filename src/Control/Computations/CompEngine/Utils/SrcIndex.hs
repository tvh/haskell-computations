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
 'SrcKeyId'; 'SrcKeyArena' stores, per interned key id, the flat
 @(DefRef, AnyCompSrcVer)@ list of that key's current dependents -- unboxed
 'DefRef' column, boxed (parallel, index-aligned) 'AnyCompSrcVer' column.

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
 ('SimpleStateIf.removeSrcDependent'\'s own "did this key's dependent set
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
 ('SimpleStateIf.addSrcDependent'\/'removeSrcDependent', each handling one
 'AnyCompSrcDep' at a time, called from a per-dep @forM_@ over a diffed
 edge set), so there is no "whole span" to replace and 'EdgeArena'\'s
 machinery does not fit. 'SrcKeyArena' is the structure that /does/ fit
 single-entry mutation: a plain growable unboxed\/boxed column pair per key
 with append at the tail and O(current key size) linear-scan-then-
 swap-with-last removal -- no tombstones, no compaction pass, because a
 swap-remove leaves no dead space to reclaim in the first place. The
 O(current key size) scan is the one deliberate complexity tradeoff here
 (a hand-rolled per-key secondary index mapping 'DefRef' to slot would make
 removal O(1) at the cost of a second boxed structure per key, undoing much
 of the point) -- accepted because the persistence benchmark shipped with
 this library (see @bench\/.../Bench\/Main.hs@) exercises roughly 683
 dependents per key averaged across 300 keys, which keeps a linear scan
 cheap in practice, and because the scan operates on an unboxed 'Int'
 column, not a boxed structure -- see the module's own churn tests for the
 bound this actually achieves.

 = Invariants (summary)

 __Key interning ('KeyIntern'):__

 * A key id is live iff it currently has a forward-map entry; release
   deletes the entry and pushes the id onto a free list for reuse -- no
   refcounting, because ownership here is 1:1 (see "Why KeyIntern needs no
   refcounting" above), unlike 'DefTable.hs'\'s @SrcDepIntern@.
 * A caller only ever releases a key id after its own bookkeeping
   ('SimpleStateIf.removeSrcDependent') has confirmed the key's dependent
   arena just became empty -- 'kiRelease' on a key with a live dependent is
   always a lifecycle bug, not a legitimate state.

 __Per-key dependent arena ('SrcKeyArena'):__

 * 'skaAppend' does not deduplicate -- a second append for a ref already
   present in the arena adds a second entry. Safe only because every real
   call path removes the old @(key, version)@ entry before adding the new
   one on a version change ("SimpleStateIf.hs"'s @updateEdges@, "removes
   before adds, deliberately").
 * 'skaRemove' is swap-with-last, not a stable shift -- order is not part
   of this structure's contract.
 * __Every write through 'skaAppend' must force @ver@ to WHNF.__
   'skaAppend' does this itself (the @!ver@ bang) precisely because
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

-- | @ska_refs@ stores each dependent's 'DefRef' directly: 'DefRef' has a
-- real 'Data.Vector.Unboxed.Unbox' instance (via vector's
-- 'Data.Vector.Unboxed.UnboxViaPrim' -- see "DefTable.hs"'s
-- 'Control.Computations.CompEngine.Utils.DefTable.unDefRef' haddock for the
-- recipe), so this column stores 'DefRef' values directly rather than a raw
-- 'Int' that would need coercing at every read\/write.
-- 'skaAppend'\/'skaRemove'\/'skaToList' -- this module's actual API
-- boundary -- take\/return 'DefRef' exactly as this column stores it, with
-- no representation seam between them.
data SrcKeyArena = SrcKeyArena
  { ska_refs :: !(IORef (VUM.IOVector DefRef))
  , ska_vers :: !(IORef (VM.IOVector AnyCompSrcVer))
  , ska_len :: !(IORef Int)
  }

newSrcKeyArena :: IO SrcKeyArena
newSrcKeyArena =
  SrcKeyArena
    <$> (newIORef =<< VUM.new 0)
    <*> (newIORef =<< VM.new 0)
    <*> newIORef 0

-- | Grow both columns in lockstep to at least @needed@ slots. Freshly grown
-- capacity in the boxed column is the @vector@ package's own
-- uninitialised-element error thunk; safe because every slot below
-- 'ska_len' is written by 'skaAppend' before it is ever read.
growBoth :: SrcKeyArena -> Int -> IO ()
growBoth ska needed = do
  rv <- readIORef (ska_refs ska)
  let cap = GM.length rv
  when (needed > cap) $ do
    let extra = max (needed - cap) (max 4 cap)
    rv' <- GM.unsafeGrow rv extra
    writeIORef (ska_refs ska) rv'
    vv <- readIORef (ska_vers ska)
    vv' <- GM.unsafeGrow vv extra
    writeIORef (ska_vers ska) vv'

-- | Append one @(ref, ver)@ entry, forcing @ver@ to WHNF before storing it.
--
-- Caller's responsibility: if @ref@ is already present, this appends a
-- *second* entry rather than updating the first. Every call site removes
-- the old entry before adding the new one when a dependent re-registers at
-- a different version (see "SimpleStateIf.hs"'s @updateEdges@, "removes
-- before adds, deliberately"), so a live duplicate never actually occurs on
-- the real call path; a direct 'skaAppend' misuse would silently
-- double-count rather than fail loudly, which is why the module's own
-- tests exercise the ordering this depends on directly rather than
-- trusting the invariant silently.
--
-- Forcing @ver@ is not optional. @AnyCompSrcVer@'s constructor, like every
-- constructor in this codebase, has strict fields by default
-- ('StrictData'), so WHNF is enough to fully evaluate it -- but nothing
-- forces @ver@ to WHNF in the first place unless the write site does it:
-- @Data.Vector.Mutable.write@ carries no strictness contract of its own
-- and will happily store an unevaluated thunk (unlike a
-- @Data.HashMap.Strict@, which forces its value on every insert). Left
-- lazy, that thunk would close over its caller's whole @AnyCompSrcDep@
-- (and, transitively, the 'DepSet'\/'HashSet' it came from) and never get
-- forced during cold eval at all, since nothing reads a version back out
-- until the live phase's 'SimpleStateIf.notifyDepChange' compares it --
-- meaning every dependent's evaluation closure would stay reachable for
-- the entire cold-eval run for no reason. That is exactly the shape of
-- space leak no type signature can catch: semantics stay correct (the
-- same instance\/rerun counts either way), only retention is wrong.
skaAppend :: SrcKeyArena -> DefRef -> AnyCompSrcVer -> IO ()
skaAppend ska ref !ver = do
  len <- readIORef (ska_len ska)
  growBoth ska (len + 1)
  rv <- readIORef (ska_refs ska)
  vv <- readIORef (ska_vers ska)
  VUM.write rv len ref
  VM.write vv len ver
  writeIORef (ska_len ska) (len + 1)

-- | Remove the entry for @ref@ via linear scan, then swap-with-last (no
-- tombstone -- a swap-remove leaves no gap to reclaim later, unlike
-- 'EdgeArena'\'s append-orphan-compact scheme, which this module
-- deliberately does not need -- see the module haddock). Returns 'True' iff
-- an entry was found and removed; a caller that expects @ref@ to be
-- present (there isn't one here -- see 'SimpleStateIf.removeSrcDependent',
-- which tolerates a miss deliberately) can check the result.
skaRemove :: SrcKeyArena -> DefRef -> IO Bool
skaRemove ska ref = do
  len <- readIORef (ska_len ska)
  rv <- readIORef (ska_refs ska)
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
        vv <- readIORef (ska_vers ska)
        lastVer <- VM.read vv lastIdx
        VM.write vv i lastVer
      writeIORef (ska_len ska) lastIdx
      pure True

-- | Every currently-live @(ref, ver)@ pair, in unspecified order: order is
-- not part of this structure's contract, and 'skaRemove'\'s swap-with-last
-- deliberately does not preserve insertion order.
skaToList :: SrcKeyArena -> IO [(DefRef, AnyCompSrcVer)]
skaToList ska = do
  len <- readIORef (ska_len ska)
  rv <- readIORef (ska_refs ska)
  vv <- readIORef (ska_vers ska)
  mapM (\i -> (,) <$> VUM.read rv i <*> VM.read vv i) [0 .. len - 1]

skaNull :: SrcKeyArena -> IO Bool
skaNull ska = (== 0) <$> readIORef (ska_len ska)

-- | Test/debug-only: current live entry count.
skaLiveCount :: SrcKeyArena -> IO Int
skaLiveCount ska = readIORef (ska_len ska)
