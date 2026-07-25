{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}

{- | Per-definition columnar row storage: the "Tier 2" memory redesign from
 docs/benchmark-notes.md's "Memory roadmap" section, modeled on the Rust
 port's Stage 5 (@rust-computations/docs/persistence-benchmark-notes.md@,
 "Stage 5 -- Tier 2 columnar per-def tables").

 = Row identity

 A row is identified by a def index (which definition it belongs to) and a
 row number within that definition's table. Callers of this module pack the
 two into a single 'Int' via 'DefRef' rather than using a pair, so the rest
 of the state layer (the stale-cap priority queue, the outputs side table,
 anything hashing/comparing an identity) keeps working with plain 'Int's --
 the same "hot-path keys are Ints" property Stage 1's interning established,
 just reinterpreted: what used to be an arbitrary dense counter is now
 @(defIndex \<\< rowBits) .|. row@.

 = Columns

 Each column is a separate growable vector, unboxed where the element type
 allows it ('Data.Vector.Unboxed.Mutable' for @param_hash@/@result_hash@
 (as two 'Word64's each, since 'Hash128' itself has no 'Unbox' instance and
 splitting it avoids needing to write one) and @flags@), boxed where it
 can't be ('Data.Vector.Mutable' for the typed @param@/@value@ columns). A
 boxed column's freshly-grown capacity is left as the @vector@ package's own
 "uninitialised element" error thunk (see 'growBoxed') -- exactly the
 loud-failure-on-premature-read canary this module established for row
 lifecycle in an earlier increment (dead ids there, dead cells here), now
 extended to row storage. Growth for the @flags@ column (and, see below, the
 edge-arena @len@ columns) is the one exception: it is explicitly zeroed
 (not left as unboxed garbage), because a stray nonzero byte there would
 silently read as an occupied row or a nonempty edge span. @srcDeps@ is
 neither of the above -- see "Src-dep interning" below.

 = Edge storage: per-def CSR arenas, not per-row vectors

 An earlier increment of this rewrite gave every row its own small unboxed
 'VU.Vector' for @compDeps@/@rdeps@ -- correct, but each such vector is its
 own heap object (a boxed cell in a 'VM.Mutable' column holding a 'VU.Vector'
 wrapper plus its own 'Data.Array.Byte.ByteArray#'), so a fan-in-3 row paid
 for several small, separately-traced nursery allocations to hold ~24-72
 bytes of actual edge payload. This module instead gives each def two
 shared, growable, flat unboxed 'Word64' arenas (one for @compDeps@, one for
 @rdeps@) plus two unboxed per-row columns (@offset@ :: 'Int32', @len@ ::
 'Word32') indexing into them -- CSR (compressed sparse row), with the
 arena itself, not each row, as the unit of (de)allocation. A comp-dep edge
 is 3 words (target 'DefRef', observed-hash hi, observed-hash lo, matching
 exactly what the old per-row @VU.Vector (Int, Word64, Word64)@ carried --
 see "Preserved semantics" below); an rdep edge is 1 word (target
 'DefRef'). One large arena is the point of this exercise: unlike a swarm of
 small per-row vectors, a large flat array is a single object GHC's
 allocator places in the large-object area once it crosses the pinned/large
 threshold, where the copying collector never traces or copies it -- see the
 module's "Small defs" note below for the one case where this doesn't apply.

 __Mutation strategy: append-new-span, mark-old-span-dead, compact on a
 dead-fraction threshold.__ Chosen over "CSR with per-row slack" because
 every write here (@writeCompDeps@/@writeRdeps@, both called from
 "SimpleStateIf.hs") replaces a row's /entire/ edge set at once -- there is
 no in-place single-edge splice anywhere on the hot path for slack to help
 with, only whole-row replacement, so append-only is both simpler and no
 less dense. This is the same append-only-arena shape the Rust port's Stage
 5 uses for /param/ bytes (@persistence-benchmark-notes.md@, "Stage 5",
 @param_off@/@param_len@ indexing into one @param_arena@ per def), applied
 here to edges instead: a write appends the new span at the arena's current
 end and bumps a per-def dead-word counter by the row's /previous/ span
 length (if any); nothing is physically overwritten in place, so a stale
 span is simply orphaned -- exactly like 'freeRow' already leaves a freed
 row's other columns as untouched garbage, gated by flags. Unlike the Rust
 param arena (deliberately never compacted -- params are small, so even
 permanently-unreclaimed spans are cheap), edges here are compacted,
 because this task's whole point is bounding the arena: repeated
 recomputation of the /same/ live rows (exactly what the benchmark's live-
 update phase does, tens of thousands of times over the same 999,760-row
 graph) would otherwise make the arena grow without bound, one orphaned
 span per rerun, forever.

 Compaction ('eaCompact') rebuilds a def's arena from scratch, walking every
 row 0..rowCount-1 and keeping only the /current/ span of every row that is
 currently alive; a row that isn't alive contributes nothing, whether or
 not it was ever given an explicit new write after going dead -- this is
 what lets a freed-but-not-yet-reused row's orphaned span get reclaimed
 too, not just rows some other write already knew were stale. This is why
 "GC" (the row-lifecycle kind, 'freeRow') is the natural trigger the task
 brief asks for: freeing a row doesn't touch its edge columns at all (see
 "Row lifecycle and reuse" below), so the /existing/ garbage-tolerance
 design already guarantees a dead row's stale span is inert and safe to
 discard whenever compaction next runs, without freeRow itself needing to
 know anything about edge arenas. 'maybeCompact' fires this whenever, after
 an append, a def's dead-word count exceeds half its used length (and the
 arena is past a minimum size not worth an O(rowCount) scan for) --
 the standard amortized array-with-tombstones argument: a compaction
 immediately halves (at least) the dead fraction, so the arena's peak size
 is bounded to roughly 2x its live data, and the total compaction work
 across N writes is O(N) amortized, tied to the write path itself rather
 than a separately scheduled sweep.

 __Small defs.__ A def with few rows (or few edges per row) never
 accumulates enough words to reach GHC's large-object threshold (currently
 a few kB), so its arena is a small nursery-resident array like everything
 else -- this module does not special-case that, and it's fine: the whole
 argument for "large arrays escape the nursery" only pays off where it's
 needed, on the defs with many rows (this benchmark's front-loaded levels,
 up to 205,000 rows), which is also exactly where the old per-row-vector
 scheme paid the worst multiplicative overhead. A small def's arena stays
 small precisely because its total edge payload is small; there is nothing
 to reclaim by treating it differently.

 __Preserved semantics.__ Per-edge observed-result-hash tracking on comp-dep
 edges is unchanged bit-for-bit from the prior representation: each edge is
 still (target 'DefRef', observed hash hi, observed hash lo), and
 @test_modifcationWhileWorkingOnQueue@'s impure-cap detection (which
 compares a row's previously-recorded per-edge observed version against
 what it observes on the current run) depends on exactly this, per
 "SimpleStateIf.hs"'s module haddock. Only the storage layout changed; nothing
 was dropped to save bytes.

 = Hash index: open addressing over the existing hash column

 The old per-def index was a persistent 'Data.HashMap.Strict.HashMap'
 'Hash128' 'Int' -- a HAMT leaf plus a boxed 'Hash128' key and boxed 'Int'
 value per entry, every one of them traced by every major GC, even though
 the key is pure duplication: every row's param hash already lives in the
 unboxed @param_hash@ column above. 'HashIndex' replaces it with a
 hand-rolled open-addressing table that stores only row ids
 ('Data.Int.Int32', @-1@ the empty-slot sentinel) in a flat
 'Data.Vector.Unboxed.Mutable.IOVector' -- no key storage, no boxing,
 nothing for the GC to trace.

 __Probe scheme.__ A candidate slot for hash @h@ is @w128_first h .\&.
 (capacity - 1)@ (capacity is always a power of two, so this is @mod@ via a
 mask) -- MD5-derived hashes (this codebase's only hash source, see
 "Utils/Hash.hs") are already uniformly distributed, so either 64-bit half
 works equally well as the slot seed; the high word was picked arbitrarily.
 Collisions resolve by linear probing (slot, slot+1, ... wrapping mod
 capacity). A probe candidate is verified by reading the *existing*
 @param_hash@ column for the row id stored at that slot and comparing the
 full 128-bit hash against it -- exactly the redundant-key trick this
 module is built around: the table never needs to store or compare against
 a second copy of the key.

 __Growth.__ Capacity starts at 8 and doubles whenever an insert would push
 the load factor (@(count+1) \/ capacity@) past 0.7; growing rebuilds a
 fresh table and reinserts every currently-occupied row id at its new slot
 (no key copies to move, since the table holds no keys -- only a row id and
 a lookup of that row's own @param_hash@ column entry).

 __Deletion: backward-shift, not tombstones.__ Rows are freed constantly on
 this engine's live-update path (recompute invalidates a row, a fresh param
 hash allocates a new one), so a tombstone strategy would accumulate dead
 markers under exactly the workload this table sees most -- degrading probe
 length over the life of the process with no reclamation short of a full
 rehash. Backward-shift deletion avoids that: removing an entry clears its
 slot, then walks forward relocating any subsequent entry whose ideal slot
 no longer cyclically requires the gap left behind (i.e. an entry a plain
 linear probe could no longer find past the new hole) back into the hole,
 opening a fresh hole where that entry used to sit, until a genuinely empty
 slot is reached -- the standard argument for why linear probing never
 needs tombstones. The net effect: repeated free/reuse churn (this module's
 own row-lifecycle contract, exercised by the live-update benchmark tens of
 thousands of times over) never grows the table's dead-slot fraction --
 capacity only ever grows in response to genuine live-entry growth, never
 in response to churn. See 'hixBackwardShift' for the algorithm (Knuth's
 backward-shift deletion for linear probing, adapted to not store keys).

 = Src-dep interning

 @dt_srcDeps@ used to be a boxed per-row column of persistent
 'Data.HashSet.HashSet' 'AnyCompSrcDep's -- correct, but sparse and
 expensive where populated: in the benchmark graph only the level-0 rows
 (~205k of 1M) have any source dependency at all, and each of those has
 exactly one, but 'AnyCompSrcDep' is an existential wrapper
 ('Control.Computations.CompEngine.CompFlow.ForAnyCompFlow' around a
 'CompSrcId' plus a typed key/version pair) -- a boxed, multi-word object in
 its own right, sitting inside a boxed 'HashSet' HAMT leaf, per row. Worse,
 real graphs (this benchmark included, at 300 distinct source keys against
 ~200k dependent rows) see the *same* @(key, version)@ pair observed by many
 rows at once, and nothing before this deduplicated that: every row got its
 own separate heap copy of an equal value.

 This module now interns 'AnyCompSrcDep' values to small 'Int' ids (a
 per-def 'SrcDepIntern': forward @'HashMap' 'AnyCompSrcDep' 'Int'@, reverse
 growable boxed vector for the @Int -> AnyCompSrcDep@ direction, ids
 assigned monotonically) and stores a row's src-dep set as an interned-id
 CSR arena -- the exact same 'EdgeArena' machinery @compDeps@/@rdeps@ already
 use above (stride 1, ids narrow enough to fit the arena's 'Word64' words
 with room to spare), rather than inventing a third storage shape. A row's
 set is now a handful of unboxed words indexing into a small, heavily-shared
 table instead of a private boxed structure duplicated per row.

 __Ids are refcounted and recycled.__ Stage 1's original global intern
 table (and this table's own first version, docs/benchmark-notes.md's
 "Stage 4e") never recycled ids, accepted as a deliberate tradeoff on the
 assumption that a src-dep id's *value space* stays small relative to row
 count. That assumption fails for the part of the value space that
 actually varies at runtime: 'AnyCompSrcDep' carries an *observed version*,
 so a long-running, high-churn engine sees an unbounded stream of distinct
 @(key, version)@ pairs over its lifetime even though only a handful are
 ever *referenced* at once -- exactly the gap between "value space size"
 and "currently-live set size" that made the never-recycled table an
 actual leak, not just a theoretical one. Each id in @sdi_refcount@ (parallel
 to @sdi_reverse@, one 'Int' per assigned id) now counts how many rows'
 arena spans currently reference it. 'DefTable.writeSrcDeps' (a row's whole
 span is always replaced wholesale, the same "no in-place single-edge
 splice" shape as @compDeps@/@rdeps@) retains every id in the new span and
 releases every id the row's *previous* span held; 'freeRow' releases the
 freed row's own span outright, since a freed row's references can't wait
 for its def's arena to next compact the way @compDeps@/@rdeps@ garbage
 can -- an id with no more referencing rows must become reclaimable
 immediately, not whenever compaction next happens to run. A release that
 drops a refcount to zero deletes the 'sdi_forward' entry, clears the
 'sdi_reverse' slot to 'Nothing' (dropping the boxed 'AnyCompSrcDep' itself
 -- retaining it there would still be half the leak even with the forward
 entry gone), and pushes the id onto a free list; 'sdiIntern' pops that
 free list before ever extending 'sdi_count', so a reused id behaves
 exactly like a fresh one (refcount zero until retained, same slot machinery
 either way). 'sdiResolve' checks both range (against 'sdi_count') and
 liveness (refcount > 0) -- recycling reopens exactly the "in range but
 dead" gap Stage 1's never-recycled contract didn't have to worry about, so
 the explicit zero-refcount check is required, not just belt-and-suspenders.

 __The ordering hazard.__ 'writeSrcDeps' runs unconditionally on every
 finish (changed or not), so an id present in *both* the old and new span
 (an unchanged dependency) is the common case, not an edge case. Retaining
 the new span before releasing the old one is required for exactly this
 case: releasing first would let such an id's refcount transiently touch
 zero, reclaiming it (and freeing its slot for reuse by something else
 entirely) before the retain that was about to re-establish the reference
 ever runs. See 'writeSrcDeps''s own haddock for the same point closer to
 the code, and the module's test section for a test that pins this down
 directly (a shared id across an overlapping overwrite must never observe
 an intermediate zero).

 __What this does and does not change.__ 'readSrcDeps'/'writeSrcDeps' keep
 their exact prior signatures (@'DefTable' p a -> 'Int' -> 'IO' ('HashSet'
 'AnyCompSrcDep')@ and the writing dual) -- intern/refcount/decode happens
 entirely inside them, invisible to "SimpleStateIf.hs", which needed zero
 edits (the same "column access boundary absorbs the representation
 change" property 4b/4c/4d established; 'freeRow''s signature is likewise
 unchanged, since the extra release work happens inside it against columns
 it already owns). What SimpleStateIf.hs's own @sifs_srcIndex@ tracks
 (which rows currently depend on which source key, and when a key becomes
 wholly unclaimed and should be reported as garbage/unregistered) is
 entirely untouched by any of this -- that bookkeeping only ever sees
 fully-decoded 'AnyCompSrcDep' values via the unchanged read/write API, so
 dead-source-key reporting and 'CompSrc.compSrcUnregister' triggering keep
 working exactly as before.

 = Row lifecycle and reuse

 A freed row is pushed onto a free list and its 'flags' cleared (not-alive,
 no result, not pending); most other columns are left untouched -- stale
 param/value/@compDeps@/@rdeps@ from the previous occupant, including those
 two edge arenas' spans, become reclaimable garbage handled by the
 edge-arena compaction machinery above rather than by 'freeRow' itself.
 This is safe under the invariant that every read of a possibly-stale
 column is gated behind a flags check (@alive@ for identity-ish columns,
 @hasResult@ for the value/result-hash columns) that is unconditionally
 cleared before the row can be reused, so the garbage can never be
 observed. Reuse does not recycle the row's *hash* index entry (that is
 removed by the caller before freeing) but does recycle the row *number* --
 new occupants of a freed row get a fresh index entry pointing at the
 recycled number.

 The @srcDeps@ arena is the one column 'freeRow' /does/ eagerly touch, and
 for a reason specific to it: its ids are a refcounted shared resource (see
 "Src-dep interning" above), and an id's refcount must drop the moment
 nothing references it, not whenever this def's arena next happens to
 compact -- so 'freeRow' reads the freed row's span and releases every id
 in it via 'eaTakeRow', which also zeroes the row's own offset/len as it
 does so. That zeroing matters beyond bookkeeping hygiene: without it, the
 reset write 'lookupOrInsertRow' issues when this row number is next reused
 would read the same (already-released) span again and release it a second
 time -- a double-release, and exactly the bug the module's tests check
 for directly.
-}
module Control.Computations.CompEngine.Utils.DefTable (
  -- * Row identity
  DefIdx,
  RowIdx,
  DefRef,
  packRef,
  unpackRef,
  refDefIdx,
  refRow,
  unDefRef,
  mkDefRefUnsafe,
  unDefIdx,
  mkDefIdx,
  RowCount,
  unRowCount,
  rowIndices,

  -- * Flags
  Flags,
  ResultState (..),
  flagsAlive,
  flagsPending,
  flagsResultState,
  mkFlags,
  zeroFlags,

  -- * The table
  DefTable,
  new,
  rowCount,

  -- * Row lifecycle
  lookupOrInsertRow,
  freeRow,
  isAlive,

  -- * Column access
  readFlags,
  writeFlags,
  setPending,
  readParamHash,
  readResultHash,
  writeResultHash,
  clearResultHash,
  readParam,
  readValue,
  writeValue,
  readCompDeps,
  writeCompDeps,
  compDepTargets,
  hashToPair,
  pairToHash,
  noResultSentinel,
  readRdeps,
  writeRdeps,
  readSrcDeps,
  writeSrcDeps,
  srcDepInternLiveCount,
  htf_thisModulesTests,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompSrc (
  AnyCompSrcDep,
  CompSrc (..),
  CompSrcInstanceId (..),
  Dep (..),
  wrapCompSrcDep,
 )
import Control.Computations.Utils.Hash (Hash128 (..), largeHash128)

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Applicative ((<|>))
import Control.Concurrent.STM (retry)
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (foldM, forM, forM_, unless, when)
import Data.Bits
import Data.Either (isLeft)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (..))
import Data.IORef
import Data.Int (Int32)
import qualified Data.LargeHashable as LH
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Type.Equality ((:~:) (Refl))
import Data.Typeable (Typeable, eqT)
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word32, Word64, Word8)
import Test.Framework hiding ((.&.))
import qualified Test.QuickCheck as QC

--
-- Row identity: DefRef = packed (DefIdx, RowIdx). Three historically
-- Int-shaped concepts, now three distinct types -- see the module haddock's
-- "Row identity" section for the full rationale (in short: a transposed
-- @packRef row defIdx@, or a def index handed to a function expecting a
-- row, used to typecheck silently when all three were bare 'Int's).
--

-- | Which definition a row belongs to.
newtype DefIdx = DefIdx Int
  deriving (Eq, Ord, Show)

-- | A row number within one definition's 'DefTable'. Also reused, outside
-- 'DefTable' proper, as the generic "row id" type 'HashIndex' indexes by
-- (deliberately decoupled from 'DefTable' -- see its own haddock) and as
-- the per-entry row component 'EdgeArena' functions take.
newtype RowIdx = RowIdx Int
  deriving (Eq, Ord, Show)
  deriving newtype (Hashable)

-- | A packed @(DefIdx, RowIdx)@ identity: 20 bits of def index (~1M
-- definitions), 44 bits of row (~17 trillion rows per definition) -- both
-- comically generous versus this engine's actual scale, chosen to leave
-- headroom rather than to be tight. A real 'newtype', not a type alias --
-- see the module haddock's "Row identity" section for why that distinction
-- matters here specifically. Its constructor is not exported; the only ways
-- to produce or take one apart from outside this module are 'packRef' \/
-- 'unpackRef' \/ 'refDefIdx' \/ 'refRow' and the explicit 'unDefRef' escape
-- hatch below.
newtype DefRef = DefRef Int
  deriving (Eq, Ord, Show)
  deriving newtype (Hashable)

rowBits :: Int
rowBits = 44

rowMask :: Int
rowMask = (1 `shiftL` rowBits) - 1

packRef :: DefIdx -> RowIdx -> DefRef
packRef (DefIdx defIdx) (RowIdx row) = DefRef ((defIdx `shiftL` rowBits) .|. (row .&. rowMask))
{-# INLINE packRef #-}

unpackRef :: DefRef -> (DefIdx, RowIdx)
unpackRef (DefRef ref) = (DefIdx (ref `shiftR` rowBits), RowIdx (ref .&. rowMask))
{-# INLINE unpackRef #-}

refDefIdx :: DefRef -> DefIdx
refDefIdx = fst . unpackRef
{-# INLINE refDefIdx #-}

refRow :: DefRef -> RowIdx
refRow = snd . unpackRef
{-# INLINE refRow #-}

-- | Escape hatch to the raw 'Int' a 'DefRef' packs. Exists for exactly two
-- call sites outside this module's own column storage: "Utils/SrcIndex.hs"'s
-- @SrcKeyArena@, which stores one 'DefRef' per dependent in an /unboxed/
-- column (so it must hold a raw 'Int', not a 'DefRef' -- giving 'DefRef' its
-- own 'Data.Vector.Unboxed.Unbox' instance would mean writing one by hand,
-- since 'Unbox' has associated data families that newtype-deriving can't
-- shortcut without a Template Haskell dependency this codebase deliberately
-- avoids), and this module's own 'EdgeArena'-packed @Word64@ triples (see
-- 'flattenCompDeps'\/'unflattenCompDeps'). Not needed, and not used, by
-- "SimpleStateIf.hs" -- every container it keys by row identity (the stale
-- queue, the outputs map, the pending-outputs map) is already generic in
-- its key type, so 'DefRef' flows through them unwrapped.
unDefRef :: DefRef -> Int
unDefRef (DefRef i) = i
{-# INLINE unDefRef #-}

-- | The 'unDefRef' of 'packRef': build a 'DefRef' back up from a raw 'Int'
-- read out of unboxed storage. Caller's responsibility, same as 'unDefRef':
-- the 'Int' must actually be a previously-'unDefRef''d 'DefRef', not an
-- arbitrary value.
mkDefRefUnsafe :: Int -> DefRef
mkDefRefUnsafe = DefRef
{-# INLINE mkDefRefUnsafe #-}

-- | Unwrap a 'RowIdx' to the raw 'Int' index a mutable-vector read\/write
-- ultimately needs. Used throughout this module's own column access, where
-- @row@ is already known typed; not exported (row indices never need to
-- leave "DefTable.hs"/"SimpleStateIf.hs" as anything other than 'RowIdx').
unRowIdx :: RowIdx -> Int
unRowIdx (RowIdx i) = i
{-# INLINE unRowIdx #-}

-- | Unwrap a 'DefIdx' to the raw 'Int' "SimpleStateIf.hs"'s @IntMap@-keyed
-- def registry (@sifs_defs@) needs -- 'Data.IntMap.Strict' mandates a
-- literal 'Int' key, so a 'DefIdx' must be unwrapped at exactly that
-- boundary. Exported for that one call site; every other place a 'DefIdx'
-- flows (e.g. as a 'HashMap' /value/, or opaquely through this module's own
-- API) needs no unwrapping.
unDefIdx :: DefIdx -> Int
unDefIdx (DefIdx i) = i
{-# INLINE unDefIdx #-}

-- | The 'unDefIdx' of a fresh index: wrap the raw 'Int' counter
-- "SimpleStateIf.hs"'s @sifs_nextDefIdx@ hands out.
mkDefIdx :: Int -> DefIdx
mkDefIdx = DefIdx
{-# INLINE mkDefIdx #-}

--
-- Row count / arena stride: three more historically Int-shaped concepts
-- that used to sit unlabeled next to a row index in EdgeArena's functions
-- -- "how many rows does this def currently have" (the bound an
-- alive-scanning compaction walks) is not "which row" and is not "how many
-- words make up one edge in this arena".
--

-- | A def's current logical row count -- what 'rowCount' returns and what
-- 'eaWrite'\/'eaCompact'\/'maybeCompact' scan up to when reclaiming a def's
-- arena.
newtype RowCount = RowCount Int
  deriving (Eq, Ord, Show)

unRowCount :: RowCount -> Int
unRowCount (RowCount i) = i
{-# INLINE unRowCount #-}

-- | Every 'RowIdx' from 0 up to (but not including) a 'RowCount' -- the
-- iteration escape hatch for callers (e.g. "SimpleStateIf.hs"'s
-- @validateSifState@) that need to walk every row a table currently has,
-- rather than operate on one specific 'RowIdx' already in hand.
rowIndices :: RowCount -> [RowIdx]
rowIndices (RowCount n) = map RowIdx [0 .. n - 1]

-- | Words per edge in an 'EdgeArena': 3 for @compDeps@ (target 'DefRef' +
-- observed-hash hi\/lo), 1 for @rdeps@\/@srcDeps@ (target 'DefRef' or
-- interned id alone). A plain newtype rather than a phantom type parameter
-- on 'EdgeArena' itself -- see the module haddock's "Why EdgeArena's stride
-- isn't a phantom type" section for why that was considered and rejected.
newtype Stride = Stride Int
  deriving (Eq, Ord, Show)

-- | 'dt_compDeps''s stride: target 'DefRef', observed-hash hi, observed-hash
-- lo.
compDepsStride :: Stride
compDepsStride = Stride 3

-- | 'dt_rdeps''s and 'dt_srcDeps''s stride: one word (a target 'DefRef' or
-- an interned src-dep id, respectively) per edge.
singleStride :: Stride
singleStride = Stride 1

--
-- Flags
--

-- | Packed per-row state: bit 0 alive, bit 1 pending (mid-evaluation),
-- bits 2-3 result state (see 'ResultState'). A 'newtype' over the packed
-- 'Word8', not a type alias, so the bit layout below is the /only/ code
-- that ever touches the byte directly -- everywhere else goes through the
-- named accessors ('flagsAlive', 'flagsPending', 'flagsResultState',
-- 'mkFlags'), never a raw bit test or an arithmetic comparison against the
-- packed value.
newtype Flags = Flags Word8
  deriving (Eq, Show)

data ResultState = NoResult | ResultFailure | ResultValue | ResultMetaOnly
  deriving (Eq, Show, Enum, Bounded)

bitAlive, bitPending :: Int
bitAlive = 0
bitPending = 1

flagsAlive :: Flags -> Bool
flagsAlive (Flags f) = testBit f bitAlive
{-# INLINE flagsAlive #-}

flagsPending :: Flags -> Bool
flagsPending (Flags f) = testBit f bitPending
{-# INLINE flagsPending #-}

flagsResultState :: Flags -> ResultState
flagsResultState (Flags f) = toEnum (fromIntegral ((f `shiftR` 2) .&. 0x3))
{-# INLINE flagsResultState #-}

mkFlags :: Bool -> Bool -> ResultState -> Flags
mkFlags alive pending rs =
  Flags $
    (if alive then bit bitAlive else 0)
      .|. (if pending then bit bitPending else 0)
      .|. (fromIntegral (fromEnum rs) `shiftL` 2)
{-# INLINE mkFlags #-}

-- | The all-zero 'Flags' value -- not alive, not pending, no result. What a
-- freed row's flags are reset to ('freeRow') and, via 'growUnboxedZeroed',
-- what a freshly grown (never-written) region of the @flags@ column reads
-- as -- see that function's haddock for why the column must be explicitly
-- zeroed rather than left as garbage the way every other unboxed column is.
zeroFlags :: Flags
zeroFlags = Flags 0

--
-- Hash128 <-> unboxed pair
--

hashToPair :: Hash128 -> (Word64, Word64)
hashToPair (Hash128 w) = (LH.w128_first w, LH.w128_second w)
{-# INLINE hashToPair #-}

pairToHash :: (Word64, Word64) -> Hash128
pairToHash (a, b) = Hash128 (LH.Word128 a b)
{-# INLINE pairToHash #-}

-- | Sentinel pair for "observed no result" (a failed dependency), used in
-- the @compDeps@ column's observed-version slot. Colliding with this via a
-- real MD5-derived 'Hash128' is not a realistic concern.
noResultSentinel :: (Word64, Word64)
noResultSentinel = (maxBound, maxBound)

--
-- Generic growable-vector helpers (shared by boxed and unboxed columns via
-- Data.Vector.Generic.Mutable, which both implement)
--

-- | Grow a boxed column so it has room for at least @needed@ rows. Freshly
-- grown capacity is the @vector@ package's own uninitialised-element error
-- thunk -- reading it before writing is a loud crash, by design.
growBoxed :: IORef (VM.IOVector e) -> Int -> IO ()
growBoxed ref needed = do
  vec <- readIORef ref
  let cap = GM.length vec
  when (needed > cap) $ do
    vec' <- GM.unsafeGrow vec (max (needed - cap) (max 4 cap))
    writeIORef ref vec'

-- | Grow an unboxed column so it has room for at least @needed@ elements.
-- Freshly grown capacity is unspecified garbage bytes -- safe only because
-- every unboxed column grown this way is read strictly behind a flags (or
-- edge-length) check that a legitimate write establishes first.
growUnboxed :: VUM.Unbox e => IORef (VUM.IOVector e) -> Int -> IO ()
growUnboxed ref needed = do
  vec <- readIORef ref
  let cap = GM.length vec
  when (needed > cap) $ do
    vec' <- GM.unsafeGrow vec (max (needed - cap) (max 4 cap))
    writeIORef ref vec'

-- | Like 'growUnboxed', but explicitly zeroes the freshly grown region.
-- Used for @flags@ (a stray nonzero garbage byte there would silently read
-- as an occupied row) and for an edge arena's @len@ column (a stray
-- nonzero garbage value there would silently read as a nonempty edge
-- span) -- both are the "is this row's data safe to read" gate for their
-- respective columns, unlike every other unboxed column here, which is
-- read only after one of these two checks has already gated it.
growUnboxedZeroed :: (VUM.Unbox e, Num e) => IORef (VUM.IOVector e) -> Int -> IO ()
growUnboxedZeroed ref needed = do
  vec <- readIORef ref
  let cap = GM.length vec
  when (needed > cap) $ do
    let extra = max (needed - cap) (max 4 cap)
    vec' <- GM.unsafeGrow vec extra
    GM.set (GM.unsafeSlice cap extra vec') 0
    writeIORef ref vec'

--
-- Typed value columns (param / value): unboxed where the element type is
-- provably one of a fixed set of primitives, boxed otherwise -- see the
-- module haddock's "Typed value columns" section for the full rationale.
-- Selection happens once, at column-construction time, via 'eqT' against
-- @e@'s 'Typeable' instance; nothing on the read/write hot path branches on
-- the *type*, only on which data constructor the already-built 'Column'
-- happens to be (a single, cheap, wholly local pattern match GHC has every
-- opportunity to specialize per call site).
--

-- | A param or value column. @e@ is unconstrained on 'ColBoxed' -- every
-- type, including e.g. 'Data.ByteString.ByteString', goes through it; the
-- existential 'VUM.Unbox' dictionary plus the @e :~: u@ equality witness on
-- 'ColUnboxed' is what lets a single read\/write pair (below) work for both
-- branches without a second, type-directed dispatch at every access.
data Column e where
  ColBoxed :: !(IORef (VM.IOVector e)) -> Column e
  ColUnboxed :: (VUM.Unbox u) => !(e :~: u) -> !(IORef (VUM.IOVector u)) -> Column e

-- | Build a fresh, empty column for @e@, choosing 'ColUnboxed' when @e@ is
-- one of the fixed set of primitive types this module recognizes (mirrors
-- the task brief's list: Word32\/Word64\/Int\/Char\/Bool\/Double -- the
-- benchmark's own def bodies are @Word32@ param \/ @Word64@ result, so this
-- exercises the unboxed path directly) and 'ColBoxed' for everything else
-- (e.g. the test suite's @ByteString@/@String@ param\/result types).
-- Requires only 'Typeable' @e@ -- already implied by both
-- 'Control.Computations.CompEngine.Types.IsCompParam' and
-- 'Control.Computations.CompEngine.Types.IsCompResult', so this adds no
-- constraint beyond what every caller already has in scope; nothing in the
-- public engine interface (@defineComp@, @wireComp@, etc.) changes shape.
mkColumn :: forall e. (Typeable e) => IO (Column e)
mkColumn =
  case tryUnboxed of
    Just mk -> mk
    Nothing -> ColBoxed <$> (newIORef =<< VM.new 0)
 where
  tryUnboxed :: Maybe (IO (Column e))
  tryUnboxed =
    unboxedAs @Word32
      <|> unboxedAs @Word64
      <|> unboxedAs @Int
      <|> unboxedAs @Char
      <|> unboxedAs @Bool
      <|> unboxedAs @Double
  unboxedAs :: forall u. (Typeable u, VUM.Unbox u) => Maybe (IO (Column e))
  unboxedAs = case eqT :: Maybe (e :~: u) of
    Just Refl -> Just (ColUnboxed Refl <$> (newIORef =<< VUM.new 0))
    Nothing -> Nothing

-- | Test/introspection helper: which representation a column picked.
columnIsUnboxed :: Column e -> Bool
columnIsUnboxed ColBoxed{} = False
columnIsUnboxed ColUnboxed{} = True

colGrow :: RowCount -> Column e -> IO ()
colGrow needed (ColBoxed ref) = growBoxed ref (unRowCount needed)
colGrow needed (ColUnboxed Refl ref) = growUnboxed ref (unRowCount needed)

colRead :: Column e -> RowIdx -> IO e
colRead (ColBoxed ref) row = readIORef ref >>= \v -> VM.read v (unRowIdx row)
colRead (ColUnboxed Refl ref) row = readIORef ref >>= \v -> VUM.read v (unRowIdx row)

-- | Write a value into the column at @row@. The 'ColBoxed' case is the one
-- that matters: 'Data.Vector.Mutable.write' carries no strictness contract
-- (unlike the 'Data.HashMap.Strict' that used to hold param\/value data
-- pre-Stage-3), so a caller that hands this an unforced thunk gets exactly
-- 4h's bug -- a per-row retained closure invisible to every type signature
-- and every correctness test, showing up only as time and allocation. In
-- the current call paths @e@ is already forced by the time it gets here
-- (both 'writeParam' and 'writeValue''s callers force a 'largeHash128' of
-- the same value first, to compute the row's param\/result hash, which
-- requires traversing -- hence forcing -- the whole value), but that is an
-- accident of control flow elsewhere, not a guarantee this module owns; the
-- bang here makes the column itself carry the same strictness contract the
-- @.Strict@ container it replaced did, rather than depending on every
-- present and future caller getting the ordering right.
colWrite :: Column e -> RowIdx -> e -> IO ()
colWrite (ColBoxed ref) row !e = readIORef ref >>= \v -> VM.write v (unRowIdx row) e
colWrite (ColUnboxed Refl ref) row e = readIORef ref >>= \v -> VUM.write v (unRowIdx row) e

--
-- CSR-style per-def edge arenas -- see the module haddock's "Edge storage"
-- section for the full design rationale. Everything below is stride-
-- generic (stride 3 for compDeps, 1 for rdeps) and knows nothing about
-- 'DefTable' or 'DefRef'; the typed (un)flattening into
-- @VU.Vector (Int, Word64, Word64)@ / @VU.Vector Int@ happens at the
-- DefTable column-access boundary further down.
--

-- | A row's current liveness, as a callback rather than a direct
-- 'DefTable' reference -- this section's functions are deliberately
-- decoupled from 'DefTable' (see the module haddock), so they ask for "is
-- this row alive" the same indirect way 'HashIndex' below asks for "what is
-- this row's hash" (see 'GetHash'). What 'isAlive' partially applied to a
-- concrete 'DefTable' looks like from an 'EdgeArena''s side.
type IsRowAlive = RowIdx -> IO Bool

data EdgeArena = EdgeArena
  { ea_off :: !(IORef (VUM.IOVector Int32))
  -- ^ per-row arena word-offset of the row's current span
  , ea_len :: !(IORef (VUM.IOVector Word32))
  -- ^ per-row edge count (0 => no span, @ea_off@ is meaningless)
  , ea_data :: !(IORef (VUM.IOVector Word64))
  -- ^ the shared, growable, append-only arena itself
  , ea_used :: !(IORef Int)
  -- ^ arena append point, in words
  , ea_dead :: !(IORef Int)
  -- ^ words within [0, ea_used) known to belong to an orphaned span
  }

newEdgeArena :: IO EdgeArena
newEdgeArena =
  EdgeArena
    <$> (newIORef =<< VUM.new 0)
    <*> (newIORef =<< VUM.new 0)
    <*> (newIORef =<< VUM.new 0)
    <*> newIORef 0
    <*> newIORef 0

-- | Grow an edge arena's per-row @offset@/@len@ columns to at least
-- @needed@ rows. Does not touch the shared word arena itself -- that grows
-- independently, on append, in 'eaWrite'.
eaGrowRows :: EdgeArena -> RowCount -> IO ()
eaGrowRows ea needed = do
  growUnboxed (ea_off ea) (unRowCount needed)
  growUnboxedZeroed (ea_len ea) (unRowCount needed)

-- | Only compact once an arena has reached this many words -- below this,
-- the O(rowCount) scan a compaction costs isn't worth it.
compactMinWords :: Int
compactMinWords = 4096

-- | Compact iff the def's dead words are more than half of its used
-- words (and the arena is large enough to bother) -- see the module
-- haddock's amortized-cost argument.
maybeCompact :: EdgeArena -> IsRowAlive -> RowCount -> Stride -> IO ()
maybeCompact ea isRowAlive rows stride = do
  used <- readIORef (ea_used ea)
  dead <- readIORef (ea_dead ea)
  when (used >= compactMinWords && dead * 2 > used) $
    eaCompact ea isRowAlive rows stride

-- | Rebuild the arena keeping only the current span of every currently-
-- alive row (in row order); every other row (dead, or alive with no
-- edges) has its offset/len columns zeroed. A dead row contributes nothing
-- regardless of whether it was ever given an explicit new write after
-- going dead -- see the module haddock for why that's the key property
-- that makes tying this to the write path (rather than row-free) correct.
eaCompact :: EdgeArena -> IsRowAlive -> RowCount -> Stride -> IO ()
eaCompact ea isRowAlive rows (Stride stride) = do
  used <- readIORef (ea_used ea)
  srcV <- readIORef (ea_data ea)
  dstV <- VUM.new (max 4 used)
  offV <- readIORef (ea_off ea)
  lenV <- readIORef (ea_len ea)
  writeIdxRef <- newIORef 0
  forM_ [0 .. unRowCount rows - 1] $ \rowInt -> do
    let row = RowIdx rowInt
    alive <- isRowAlive row
    len <- VUM.read lenV rowInt
    if alive && len > 0
      then do
        off <- VUM.read offV rowInt
        writeIdx <- readIORef writeIdxRef
        let n = fromIntegral len * stride
        GM.unsafeCopy (VUM.slice writeIdx n dstV) (VUM.slice (fromIntegral off) n srcV)
        VUM.write offV rowInt (fromIntegral writeIdx)
        writeIORef writeIdxRef (writeIdx + n)
      else do
        VUM.write offV rowInt 0
        VUM.write lenV rowInt 0
  finalUsed <- readIORef writeIdxRef
  writeIORef (ea_data ea) dstV
  writeIORef (ea_used ea) finalUsed
  writeIORef (ea_dead ea) 0

-- | Read a row's edge span as a flat, stride-major 'VU.Vector Word64' (a
-- safe copy -- the arena keeps mutating after this call returns). Empty
-- for a row with no edges.
eaRead :: EdgeArena -> RowIdx -> Stride -> IO (VU.Vector Word64)
eaRead ea row (Stride stride) = do
  lenV <- readIORef (ea_len ea)
  len <- VUM.read lenV (unRowIdx row)
  if len == 0
    then pure VU.empty
    else do
      offV <- readIORef (ea_off ea)
      off <- VUM.read offV (unRowIdx row)
      dataV <- readIORef (ea_data ea)
      VU.freeze (VUM.slice (fromIntegral off) (fromIntegral len * stride) dataV)

-- | Read a row's edge span (like 'eaRead') and then unconditionally clear
-- its offset/len, marking the vacated span as dead weight the same way
-- 'eaWrite' marks a replaced span dead. Used when a row is freed outright
-- (no replacement span follows, so 'eaWrite''s own old-span handling never
-- runs for it): a caller that needs to release something per edge in the
-- vacated span (src-dep refcounts -- see \"Src-dep interning\" below) can
-- do so exactly once, because the zeroed len means a later read of this
-- row (e.g. the reset write 'lookupOrInsertRow' issues when a freed row
-- number is reused) sees an empty span rather than the same stale ids
-- again.
eaTakeRow :: EdgeArena -> RowIdx -> Stride -> IO (VU.Vector Word64)
eaTakeRow ea row stride@(Stride strideN) = do
  flat <- eaRead ea row stride
  lenV <- readIORef (ea_len ea)
  offV <- readIORef (ea_off ea)
  len <- VUM.read lenV (unRowIdx row)
  when (len > 0) $ do
    modifyIORef' (ea_dead ea) (+ (fromIntegral len * strideN))
    VUM.write offV (unRowIdx row) 0
    VUM.write lenV (unRowIdx row) 0
  pure flat

-- | Overwrite a row's entire edge span with @flat@ (already stride-major
-- flattened; its length must be a multiple of @stride@). An empty write
-- just zeroes the row's offset/len -- clearing a row's edges never
-- appends to (or grows) the arena. Otherwise appends a fresh span at the
-- arena's current end, marks the row's previous span (if any) as dead
-- weight, then lets 'maybeCompact' decide whether the def's dead fraction
-- now warrants reclaiming it.
eaWrite :: EdgeArena -> RowIdx -> IsRowAlive -> RowCount -> Stride -> VU.Vector Word64 -> IO ()
eaWrite ea row isRowAlive rows stride@(Stride strideN) flat = do
  lenV <- readIORef (ea_len ea)
  offV <- readIORef (ea_off ea)
  oldLen <- VUM.read lenV (unRowIdx row)
  when (oldLen > 0) $
    modifyIORef' (ea_dead ea) (+ (fromIntegral oldLen * strideN))
  let numWords = VU.length flat
  if numWords == 0
    then do
      VUM.write offV (unRowIdx row) 0
      VUM.write lenV (unRowIdx row) 0
    else do
      used <- readIORef (ea_used ea)
      growUnboxed (ea_data ea) (used + numWords)
      dataV <- readIORef (ea_data ea)
      VU.unsafeCopy (VUM.slice used numWords dataV) flat
      writeIORef (ea_used ea) (used + numWords)
      VUM.write offV (unRowIdx row) (fromIntegral used)
      VUM.write lenV (unRowIdx row) (fromIntegral (numWords `div` strideN))
  maybeCompact ea isRowAlive rows stride

--
-- Open-addressing hash->row index -- see the module haddock's "Hash index"
-- section for the full design rationale. Deliberately generic over how a
-- row's hash is fetched (a `getHash` callback) rather than hardwired to
-- `DefTable`'s own `dt_paramHash` column, so it can be built and tested in
-- isolation from the rest of the table.
--

-- | A row's own hash-column entry, as a callback -- see the comment above
-- for why 'HashIndex' asks for this indirectly rather than taking a
-- 'DefTable' directly. What 'readParamHash' partially applied to a concrete
-- 'DefTable' looks like from a 'HashIndex''s side.
type GetHash = RowIdx -> IO Hash128

-- | Sentinel marking an empty slot. Real row ids are always >= 0 (they come
-- from 'allocRow', a monotonic-or-recycled non-negative counter), so -1
-- can never collide with a genuine occupant.
hixEmpty :: Int32
hixEmpty = -1

-- | Starting capacity for a fresh index (a fresh 'DefTable' allocates one
-- of these per def, including many-small-defs, so this stays small).
hixInitialCap :: Int
hixInitialCap = 8

-- | Grow once occupancy would exceed this fraction of capacity.
hixLoadFactor :: Double
hixLoadFactor = 0.7

data HashIndex = HashIndex
  { hix_table :: !(IORef (VUM.IOVector Int32))
  , hix_count :: !(IORef Int)
  -- ^ number of occupied slots; tracked separately rather than derived by
  -- scanning, since backward-shift deletion (no tombstones) makes
  -- "occupied slot count" and "live entry count" the same thing at all
  -- times.
  }

newHashIndex :: IO HashIndex
newHashIndex = do
  v <- VUM.new hixInitialCap
  VUM.set v hixEmpty
  HashIndex <$> newIORef v <*> newIORef 0

-- | The slot a hash probes first. Capacity is always a power of two, so
-- @mod@ is a bitmask. Uses the hash's high word arbitrarily -- MD5 output
-- (this codebase's only hash source) is uniform in either half; see the
-- module haddock's "Hash index" section.
hixSlot :: Int -> Hash128 -> Int
hixSlot cap (Hash128 w) = fromIntegral (LH.w128_first w) .&. (cap - 1)
{-# INLINE hixSlot #-}

-- | Walk the probe sequence for @h@ against a concrete table, returning the
-- slot and row id of the matching entry (verified via @getHash@, not a
-- stored key -- there isn't one), or 'Nothing' once the sequence reaches an
-- empty slot. Shared by 'hixLookup' (wants the row) and 'hixDelete' (wants
-- the slot, to start the backward-shift walk from).
hixProbe :: VUM.IOVector Int32 -> GetHash -> Hash128 -> IO (Maybe (Int, RowIdx))
hixProbe table getHash h = go start 0
 where
  cap = VUM.length table
  start = hixSlot cap h
  go i steps
    | steps >= cap = pure Nothing -- defensive: can't happen, load factor < 1 guarantees an empty slot
    | otherwise = do
        v <- VUM.read table i
        if v == hixEmpty
          then pure Nothing
          else do
            rh <- getHash (RowIdx (fromIntegral v))
            if rh == h
              then pure (Just (i, RowIdx (fromIntegral v)))
              else go ((i + 1) .&. (cap - 1)) (steps + 1)

-- | Find the row currently indexed under @h@.
hixLookup :: HashIndex -> GetHash -> Hash128 -> IO (Maybe RowIdx)
hixLookup hix getHash h = do
  table <- readIORef (hix_table hix)
  fmap snd <$> hixProbe table getHash h

-- | Insert @row@ under @h@. Caller's responsibility: @h@ is not already
-- present ('DefTable.lookupOrInsertRow' only calls this on a lookup miss).
-- Grows first if this insert would cross the load factor.
hixInsert :: HashIndex -> GetHash -> Hash128 -> RowIdx -> IO ()
hixInsert hix getHash h row = do
  hixMaybeGrow hix getHash
  table <- readIORef (hix_table hix)
  let cap = VUM.length table
      go i = do
        v <- VUM.read table i
        if v == hixEmpty
          then VUM.write table i (fromIntegral (unRowIdx row))
          else go ((i + 1) .&. (cap - 1))
  go (hixSlot cap h)
  modifyIORef' (hix_count hix) (+ 1)

hixMaybeGrow :: HashIndex -> GetHash -> IO ()
hixMaybeGrow hix getHash = do
  table <- readIORef (hix_table hix)
  count <- readIORef (hix_count hix)
  let cap = VUM.length table
  when (fromIntegral (count + 1) > hixLoadFactor * fromIntegral cap) $
    hixGrow hix getHash

-- | Double capacity and reinsert every currently-occupied row id at its new
-- slot. No keys to copy -- only row ids -- so this is a fresh table plus,
-- per occupied old slot, one @getHash@ call (the row's own @param_hash@
-- column entry) to find its new slot.
hixGrow :: HashIndex -> GetHash -> IO ()
hixGrow hix getHash = do
  old <- readIORef (hix_table hix)
  let oldCap = VUM.length old
      newCap = oldCap * 2
  new <- VUM.new newCap
  VUM.set new hixEmpty
  forM_ [0 .. oldCap - 1] $ \i -> do
    v <- VUM.read old i
    when (v /= hixEmpty) $ do
      rh <- getHash (RowIdx (fromIntegral v))
      insertFresh new newCap rh v
  writeIORef (hix_table hix) new
 where
  insertFresh table cap rh row = go (hixSlot cap rh)
   where
    go i = do
      w <- VUM.read table i
      if w == hixEmpty
        then VUM.write table i row
        else go ((i + 1) .&. (cap - 1))

-- | Remove the entry for @h@, then close the gap via backward-shift
-- deletion. No-op if @h@ isn't present (defensive; callers are expected to
-- only free a row they previously inserted).
hixDelete :: HashIndex -> GetHash -> Hash128 -> IO ()
hixDelete hix getHash h = do
  table <- readIORef (hix_table hix)
  found <- hixProbe table getHash h
  case found of
    Nothing -> pure ()
    Just (slot, _row) -> do
      let cap = VUM.length table
      VUM.write table slot hixEmpty
      modifyIORef' (hix_count hix) (subtract 1)
      hixBackwardShift table cap getHash slot

-- | Backward-shift deletion's gap-closing walk (Knuth's algorithm for
-- linear probing without tombstones, adapted to verify candidates via
-- @getHash@ rather than a stored key). @i@ starts at the position of the
-- hole just opened; @j@ walks forward from it. An entry at @j@ whose ideal
-- slot @k@ does /not/ cyclically fall in @(i, j]@ is one a plain linear
-- probe could no longer find past the hole at @i@, so it is relocated back
-- to @i@, opening a fresh hole at @j@; otherwise it is left alone and the
-- walk continues. Terminates the first time @j@ reaches a genuinely empty
-- slot.
hixBackwardShift :: VUM.IOVector Int32 -> Int -> GetHash -> Int -> IO ()
hixBackwardShift table cap getHash i0 = go i0 i0
 where
  go i jPrev = do
    let j = (jPrev + 1) .&. (cap - 1)
    vj <- VUM.read table j
    if vj == hixEmpty
      then pure ()
      else do
        hj <- getHash (RowIdx (fromIntegral vj))
        let k = hixSlot cap hj
            inRange = if i <= j then i < k && k <= j else i < k || k <= j
        if inRange
          then go i j -- entry at j stays reachable through the gap at i; keep scanning
          else do
            VUM.write table i vj
            VUM.write table j hixEmpty
            go j j -- the hole moved to j; continue scanning from there

--
-- Src-dep interning -- see the module haddock's "Src-dep interning" section
-- for the full design rationale. A per-def AnyCompSrcDep <-> Int table,
-- refcounted: an id is live iff its refcount is > 0, and a refcount
-- reaching zero reclaims it (forward map entry deleted, reverse slot
-- cleared, id pushed onto a free list for reuse) -- see the haddock.
--

-- | An id 'SrcDepIntern' has assigned to some 'AnyCompSrcDep'. The public
-- boundary type for 'sdiIntern'\/'sdiRetain'\/'sdiRelease'\/'sdiResolve';
-- internally, every column below stores and indexes by the raw 'Int' this
-- wraps (an unboxed 'Data.Vector.Unboxed.Mutable.IOVector' can't hold
-- 'SrcDepId' itself without a hand-written 'Data.Vector.Unboxed.Unbox'
-- instance, the same tradeoff 'DefRef' makes -- see 'unDefRef''s haddock),
-- and this module's own 'readSrcDeps'\/'writeSrcDeps' convert at the arena
-- boundary the same way they already convert 'DefRef's.
newtype SrcDepId = SrcDepId Int
  deriving (Eq, Ord, Show)

unSrcDepId :: SrcDepId -> Int
unSrcDepId (SrcDepId i) = i
{-# INLINE unSrcDepId #-}

mkSrcDepId :: Int -> SrcDepId
mkSrcDepId = SrcDepId
{-# INLINE mkSrcDepId #-}

data SrcDepIntern = SrcDepIntern
  { sdi_forward :: !(IORef (HashMap.HashMap AnyCompSrcDep Int))
  , sdi_reverse :: !(IORef (VM.IOVector (Maybe AnyCompSrcDep)))
  -- ^ Int -> 'Just' its 'AnyCompSrcDep' while live, 'Nothing' once
  -- released -- the explicit clear that drops the boxed value's last
  -- reference (see 'sdiRelease'), not just a logically-dead-but-still-
  -- retained slot. Grown like any other boxed column; freshly-grown
  -- capacity is the usual uninitialised-element error thunk, never read
  -- there since every index below 'sdi_count' is written by 'sdiIntern'
  -- before being handed out.
  , sdi_refcount :: !(IORef (VUM.IOVector Int))
  -- ^ Int -> current reference count. Zero means dead (whether "never
  -- assigned past sdi_count", "on the free list", or "assigned but not
  -- yet retained by any writer" -- the last of these is only ever a
  -- transient state within a single 'sdiIntern' caller, never observed by
  -- 'sdiResolve' from outside this module, since every id handed to a
  -- 'DefTable' column is retained before the write that stores it
  -- returns).
  , sdi_count :: !(IORef Int)
  -- ^ one past the highest id ever assigned; the range bound for
  -- 'sdiResolve' (a liveness check via 'sdi_refcount' is required in
  -- addition -- see 'sdiResolve').
  , sdi_free :: !(IORef [Int])
  -- ^ ids released back to refcount zero, available for 'sdiIntern' to
  -- reuse before extending 'sdi_count'.
  }

newSrcDepIntern :: IO SrcDepIntern
newSrcDepIntern =
  SrcDepIntern
    <$> newIORef HashMap.empty
    <*> (newIORef =<< VM.new 0)
    <*> (newIORef =<< VUM.new 0)
    <*> newIORef 0
    <*> newIORef []

-- | Look up @dep@'s interned id, assigning one on a miss -- popping the
-- free list first, only extending 'sdi_count' once the free list is empty
-- -- with its refcount initialized to zero either way. Does /not/ retain
-- the id (bump its refcount); callers combine this with 'sdiRetain', kept
-- as two steps rather than one so a write can intern its whole new span
-- before retaining any of it -- see 'DefTable.writeSrcDeps' and the module
-- haddock's "ordering hazard" note for why that separation matters when a
-- write's old and new spans overlap.
sdiIntern :: SrcDepIntern -> AnyCompSrcDep -> IO SrcDepId
sdiIntern sdi !dep = do
  fwd <- readIORef (sdi_forward sdi)
  case HashMap.lookup dep fwd of
    Just i -> pure (mkSrcDepId i)
    Nothing -> do
      free <- readIORef (sdi_free sdi)
      i <- case free of
        (i : rest) -> writeIORef (sdi_free sdi) rest >> pure i
        [] -> do
          i <- readIORef (sdi_count sdi)
          growBoxed (sdi_reverse sdi) (i + 1)
          growUnboxed (sdi_refcount sdi) (i + 1)
          writeIORef (sdi_count sdi) (i + 1)
          pure i
      rv <- readIORef (sdi_reverse sdi)
      VM.write rv i (Just dep)
      rc <- readIORef (sdi_refcount sdi)
      VUM.write rc i 0
      -- Forced to WHNF at the write site, not left to a later reader to
      -- force transitively -- see 'colWrite''s haddock for the general
      -- rationale (a plain 'writeIORef' of a computed 'HashMap' update
      -- gives zero strictness guarantee on its own).
      writeIORef (sdi_forward sdi) $! HashMap.insert dep i fwd
      pure (mkSrcDepId i)

-- | Bump @i@'s refcount. Caller's responsibility: @i@ is a currently-live
-- id (just returned by 'sdiIntern', or already known live) -- this module's
-- only caller ('DefTable.writeSrcDeps') always satisfies that.
sdiRetain :: SrcDepIntern -> SrcDepId -> IO ()
sdiRetain sdi (SrcDepId i) = do
  rc <- readIORef (sdi_refcount sdi)
  cur <- VUM.read rc i
  VUM.write rc i (cur + 1)

-- | Drop one reference to @i@. Fails loudly on an underflow (releasing an
-- id already at refcount zero is always a caller bug -- every release is
-- paired with a prior retain, see 'DefTable.writeSrcDeps'/'DefTable.freeRow').
-- At zero: delete the forward-map entry, clear the reverse slot (dropping
-- the boxed 'AnyCompSrcDep' -- see 'sdi_reverse'), and push @i@ onto the
-- free list for 'sdiIntern' to reuse.
sdiRelease :: SrcDepIntern -> SrcDepId -> IO ()
sdiRelease sdi (SrcDepId i) = do
  rc <- readIORef (sdi_refcount sdi)
  cur <- VUM.read rc i
  when (cur <= 0) $
    error ("DefTable.sdiRelease: refcount underflow releasing interned src-dep id " ++ show i ++ " (bug)")
  let cur' = cur - 1
  VUM.write rc i cur'
  when (cur' == 0) $ do
    rv <- readIORef (sdi_reverse sdi)
    mdep <- VM.read rv i
    case mdep of
      Nothing ->
        error ("DefTable.sdiRelease: interned src-dep id " ++ show i ++ " has no value despite a live refcount (bug)")
      Just dep -> do
        modifyIORef' (sdi_forward sdi) (HashMap.delete dep)
        VM.write rv i Nothing
        modifyIORef' (sdi_free sdi) (i :)

-- | Resolve an interned id back to its 'AnyCompSrcDep'. Fails loudly both
-- on an out-of-range id (never assigned) and, since ids are recycled, on a
-- dead one that happens to be in range (assigned once, then released back
-- to refcount zero -- the "range check alone can't tell fresh from freed"
-- gap that recycling opens up versus Stage 1's original never-recycled
-- table, see the module haddock). A zero refcount is exactly the liveness
-- signal a range check can't provide, so it's checked explicitly here
-- rather than inferred from the range.
sdiResolve :: SrcDepIntern -> SrcDepId -> IO AnyCompSrcDep
sdiResolve sdi (SrcDepId i) = do
  count <- readIORef (sdi_count sdi)
  when (i < 0 || i >= count) $
    error ("DefTable.sdiResolve: interned src-dep id " ++ show i ++ " out of range (bug)")
  rc <- readIORef (sdi_refcount sdi)
  cur <- VUM.read rc i
  when (cur <= 0) $
    error
      ( "DefTable.sdiResolve: interned src-dep id "
          ++ show i
          ++ " is dead (refcount 0) -- resolving a freed id (bug)"
      )
  rv <- readIORef (sdi_reverse sdi)
  mdep <- VM.read rv i
  case mdep of
    Just dep -> pure dep
    Nothing ->
      error ("DefTable.sdiResolve: interned src-dep id " ++ show i ++ " has no value despite a live refcount (bug)")

-- | Test/debug-only: number of currently-live interned ids (the forward
-- map's size -- what the refcounting scheme keeps bounded to the number of
-- distinct src-deps any *currently alive* row actually references, as
-- opposed to 'sdi_count', which only ever grows). Exposed at the 'DefTable'
-- level as 'srcDepInternLiveCount' for the engine-level churn regression
-- test (Tests/TestStateIf.hs).
sdiLiveCount :: SrcDepIntern -> IO Int
sdiLiveCount sdi = HashMap.size <$> readIORef (sdi_forward sdi)

--
-- The table
--

data DefTable p a = DefTable
  { dt_paramHash :: !(IORef (VUM.IOVector (Word64, Word64)))
  , dt_resultHash :: !(IORef (VUM.IOVector (Word64, Word64)))
  , dt_flags :: !(IORef (VUM.IOVector Word8))
  , dt_param :: !(Column p)
  , dt_value :: !(Column a)
  -- ^ valid only when 'flagsResultState' is 'ResultValue'. Unboxed when @a@
  -- is one of 'mkColumn''s recognized primitive types, boxed otherwise --
  -- see the "Typed value columns" section above.
  , dt_compDeps :: !EdgeArena
  -- ^ flat forward comp-dep edges, stride 3: (packed 'DefRef' this row
  -- depends on, the target's result hash *as observed* when this row last
  -- ran, split hi/lo) -- needed to replicate the old VerList-based "impure
  -- cap" detection, which must distinguish "I depend on the same target at
  -- the same version as before, yet produced a different result"
  -- (genuinely impure) from "I depend on the same target but at a newly-
  -- changed version" (an ordinary, expected recompute) -- a target-set
  -- comparison alone can't tell those apart. A target with no result at
  -- observation time (a failed dependency) is encoded as the sentinel
  -- @(maxBound, maxBound)@; colliding with a real MD5-derived hash is not
  -- a realistic concern. See the module haddock's "Edge storage" section
  -- for the CSR-arena layout this lives in.
  , dt_rdeps :: !EdgeArena
  -- ^ flat reverse comp-dep edges (packed 'DefRef's depending on this
  -- row), stride 1.
  , dt_srcDeps :: !EdgeArena
  -- ^ flat interned src-dep ids, stride 1 -- see the module haddock's
  -- "Src-dep interning" section.
  , dt_srcDepIntern :: !SrcDepIntern
  -- ^ this def's own @AnyCompSrcDep <-> Int@ table backing 'dt_srcDeps'.
  , dt_index :: !HashIndex
  -- ^ this def's own param-hash -> row index (its share of what used to be
  -- one global intern table). Open addressing over row ids only -- no
  -- stored key -- see the module haddock's "Hash index" section; probes
  -- verify against the row's own @param_hash@ column entry.
  , dt_free :: !(IORef [Int])
  , dt_len :: !(IORef Int)
  -- ^ logical row count (<= every column's current capacity)
  }

new :: forall p a. (Typeable p, Typeable a) => IO (DefTable p a)
new = do
  ph <- newIORef =<< VUM.new 0
  rh <- newIORef =<< VUM.new 0
  fl <- newIORef =<< VUM.new 0
  pa <- mkColumn
  va <- mkColumn
  cd <- newEdgeArena
  rd <- newEdgeArena
  sd <- newEdgeArena
  sdi <- newSrcDepIntern
  ix <- newHashIndex
  fr <- newIORef []
  ln <- newIORef 0
  pure
    DefTable
      { dt_paramHash = ph
      , dt_resultHash = rh
      , dt_flags = fl
      , dt_param = pa
      , dt_value = va
      , dt_compDeps = cd
      , dt_rdeps = rd
      , dt_srcDeps = sd
      , dt_srcDepIntern = sdi
      , dt_index = ix
      , dt_free = fr
      , dt_len = ln
      }

growAllTo :: DefTable p a -> RowCount -> IO ()
growAllTo dt needed = do
  growUnboxed (dt_paramHash dt) (unRowCount needed)
  growUnboxed (dt_resultHash dt) (unRowCount needed)
  growUnboxedZeroed (dt_flags dt) (unRowCount needed)
  colGrow needed (dt_param dt)
  colGrow needed (dt_value dt)
  eaGrowRows (dt_compDeps dt) needed
  eaGrowRows (dt_rdeps dt) needed
  eaGrowRows (dt_srcDeps dt) needed

-- | The table's current logical row count (rows 0 until this are valid
-- indices, though not all are necessarily alive -- see 'isAlive').
rowCount :: DefTable p a -> IO RowCount
rowCount dt = RowCount <$> readIORef (dt_len dt)

allocRow :: DefTable p a -> IO RowIdx
allocRow dt = do
  free <- readIORef (dt_free dt)
  case free of
    (r : rs) -> do
      writeIORef (dt_free dt) rs
      pure (RowIdx r)
    [] -> do
      len <- readIORef (dt_len dt)
      growAllTo dt (RowCount (len + 1))
      writeIORef (dt_len dt) (len + 1)
      pure (RowIdx len)

-- | Get-or-create the row for a given param hash. On a fresh row, writes
-- the param hash, the (caller-supplied) typed param, initializes flags to
-- alive/not-pending/no-result, resets the edge/src-dep columns to empty
-- (cheap -- an empty edge write never touches the arena, see 'eaWrite' --
-- not per-row garbage, an explicit reset, since a freshly *allocated* row
-- -- as opposed to a *reused* one -- has never had edges and there is
-- nothing stale to gate), and returns 'True' as the second component.
-- Returns 'False' on a hit.
lookupOrInsertRow :: forall p a. DefTable p a -> Hash128 -> p -> IO (RowIdx, Bool)
lookupOrInsertRow dt h p = do
  found <- hixLookup (dt_index dt) (readParamHash dt) h
  case found of
    Just row -> pure (row, False)
    Nothing -> do
      row <- allocRow dt
      writeHash (dt_paramHash dt) row h
      hixInsert (dt_index dt) (readParamHash dt) h row
      writeParam dt row p
      writeFlags dt row (mkFlags True False NoResult)
      writeCompDeps dt row VU.empty
      writeRdeps dt row VU.empty
      writeSrcDeps dt row HashSet.empty
      pure (row, True)

-- | Remove a row's hash-index entry and push it onto the free list,
-- clearing flags (not-alive, not-pending, no-result). @compDeps@/@rdeps@/
-- @param@/@value@/@result_hash@ are left untouched -- see the module
-- haddock's "Row lifecycle and reuse" section for why that's safe. The
-- src-dep arena span is the one exception: releasing this row's interned
-- src-dep ids can't be deferred to compaction the way @compDeps@/@rdeps@
-- garbage is (an unreferenced interned id must become reclaimable the
-- moment nothing points at it, not whenever this def's arena next
-- compacts), so it's released here via 'eaTakeRow', which also zeroes the
-- row's own offset/len -- otherwise the reset write 'lookupOrInsertRow'
-- issues on reuse would read the same (already-released) ids again and
-- double-release them.
freeRow :: DefTable p a -> Hash128 -> RowIdx -> IO ()
freeRow dt h row = do
  hixDelete (dt_index dt) (readParamHash dt) h
  oldSrc <- eaTakeRow (dt_srcDeps dt) row singleStride
  mapM_ (sdiRelease (dt_srcDepIntern dt) . mkSrcDepId . fromIntegral) (VU.toList oldSrc)
  modifyIORef' (dt_free dt) (unRowIdx row :)
  writeFlags dt row zeroFlags

isAlive :: DefTable p a -> IsRowAlive
isAlive dt row = flagsAlive <$> readFlags dt row

--
-- Column access. All of these assume `row` is < the table's current
-- logical length; callers only ever get a `row` from `lookupOrInsertRow`
-- or from a `DefRef` that was itself minted that way, so this holds by
-- construction.
--

readFlags :: DefTable p a -> RowIdx -> IO Flags
readFlags dt row = Flags <$> (readIORef (dt_flags dt) >>= \v -> VUM.read v (unRowIdx row))

writeFlags :: DefTable p a -> RowIdx -> Flags -> IO ()
writeFlags dt row (Flags f) = readIORef (dt_flags dt) >>= \v -> VUM.write v (unRowIdx row) f

setPending :: DefTable p a -> RowIdx -> Bool -> IO ()
setPending dt row p = do
  f <- readFlags dt row
  writeFlags dt row (mkFlags (flagsAlive f) p (flagsResultState f))

readHash :: IORef (VUM.IOVector (Word64, Word64)) -> RowIdx -> IO Hash128
readHash ref row = pairToHash <$> (readIORef ref >>= \v -> VUM.read v (unRowIdx row))

writeHash :: IORef (VUM.IOVector (Word64, Word64)) -> RowIdx -> Hash128 -> IO ()
writeHash ref row hv = readIORef ref >>= \v -> VUM.write v (unRowIdx row) (hashToPair hv)

readParamHash :: DefTable p a -> GetHash
readParamHash dt = readHash (dt_paramHash dt)

readResultHash :: DefTable p a -> RowIdx -> IO Hash128
readResultHash dt = readHash (dt_resultHash dt)

writeResultHash :: DefTable p a -> RowIdx -> Hash128 -> IO ()
writeResultHash dt = writeHash (dt_resultHash dt)

-- | Zero out the result-hash slot. Not required for correctness (every
-- read is gated behind the result-state flag bits), but avoids a
-- changed-bit false-negative from comparing against an ancient hash if a
-- future column dump/debug tool ever reads it unconditionally.
clearResultHash :: DefTable p a -> RowIdx -> IO ()
clearResultHash dt row = writeHash (dt_resultHash dt) row (pairToHash (0, 0))

writeParam :: DefTable p a -> RowIdx -> p -> IO ()
writeParam dt row p = colWrite (dt_param dt) row p

readParam :: DefTable p a -> RowIdx -> IO p
readParam dt row = colRead (dt_param dt) row

readValue :: DefTable p a -> RowIdx -> IO a
readValue dt row = colRead (dt_value dt) row

writeValue :: DefTable p a -> RowIdx -> a -> IO ()
writeValue dt row a = colWrite (dt_value dt) row a

-- | Flatten a comp-dep edge set into stride-3 words (target, hash-hi,
-- hash-lo) for 'eaWrite'. The target component is a packed 'DefRef' stored
-- as a raw 'Word64' -- like the rest of this section, a bulk unboxed
-- payload deliberately left in its raw machine representation (see
-- 'unDefRef''s haddock: giving 'DefRef' its own 'Unbox' instance is exactly
-- the friction this module avoids), not a scalar call-site argument where a
-- transposition would be the risk. 'DefTable'\'s own row-identity API
-- ('packRef'\/'unpackRef'\/'refDefIdx'\/'refRow') is the actual 'DefRef'
-- boundary; callers reconstruct a typed 'DefRef' from a target 'Int' via
-- 'mkDefRefUnsafe' only where they cross back into scalar, single-ref logic
-- (e.g. "SimpleStateIf.hs"'s @resolveRefToAny@).
flattenCompDeps :: VU.Vector (Int, Word64, Word64) -> VU.Vector Word64
flattenCompDeps xs = VU.generate (3 * VU.length xs) go
 where
  go i = case i `divMod` 3 of
    (n, 0) -> let (t, _, _) = xs VU.! n in fromIntegral t
    (n, 1) -> let (_, hi, _) = xs VU.! n in hi
    (n, _) -> let (_, _, lo) = xs VU.! n in lo

-- | Inverse of 'flattenCompDeps'.
unflattenCompDeps :: VU.Vector Word64 -> VU.Vector (Int, Word64, Word64)
unflattenCompDeps flat = VU.generate (VU.length flat `div` 3) go
 where
  go n = (fromIntegral (flat VU.! (3 * n)), flat VU.! (3 * n + 1), flat VU.! (3 * n + 2))

readCompDeps :: DefTable p a -> RowIdx -> IO (VU.Vector (Int, Word64, Word64))
readCompDeps dt row = unflattenCompDeps <$> eaRead (dt_compDeps dt) row compDepsStride

writeCompDeps :: DefTable p a -> RowIdx -> VU.Vector (Int, Word64, Word64) -> IO ()
writeCompDeps dt row xs = do
  n <- rowCount dt
  eaWrite (dt_compDeps dt) row (isAlive dt) n compDepsStride (flattenCompDeps xs)

-- | Just the target refs (raw, packed 'DefRef' 'Int's -- see
-- 'flattenCompDeps''s haddock) of a comp-dep edge set, discarding the
-- observed version -- what the rdeps graph (add/remove edges, GC liveness)
-- cares about.
compDepTargets :: VU.Vector (Int, Word64, Word64) -> VU.Vector Int
compDepTargets = VU.map (\(r, _, _) -> r)

readRdeps :: DefTable p a -> RowIdx -> IO (VU.Vector Int)
readRdeps dt row = VU.map fromIntegral <$> eaRead (dt_rdeps dt) row singleStride

writeRdeps :: DefTable p a -> RowIdx -> VU.Vector Int -> IO ()
writeRdeps dt row xs = do
  n <- rowCount dt
  eaWrite (dt_rdeps dt) row (isAlive dt) n singleStride (VU.map fromIntegral xs)

-- | Decode a row's interned src-dep arena span back into a 'HashSet' of
-- full 'AnyCompSrcDep' values -- see the module haddock's "Src-dep
-- interning" section. Callers outside this module never see an interned
-- id.
readSrcDeps :: DefTable p a -> RowIdx -> IO (HashSet AnyCompSrcDep)
readSrcDeps dt row = do
  flat <- eaRead (dt_srcDeps dt) row singleStride
  deps <- mapM (sdiResolve (dt_srcDepIntern dt) . mkSrcDepId . fromIntegral) (VU.toList flat)
  pure (HashSet.fromList deps)

-- | Intern every element of @s@ (assigning fresh ids on a miss -- see
-- 'sdiIntern'), retain each of them, release every id the row's *previous*
-- span held, then overwrite the row's entire src-dep span with the new
-- ids -- exactly like 'writeCompDeps'/'writeRdeps' do for their own edge
-- columns, plus the refcount bookkeeping those don't need.
--
-- __Ordering hazard.__ The new span is retained /before/ the old span is
-- released, deliberately: a write is not guaranteed to change anything (an
-- unchanged dependency reappears in both the old and new span, and this
-- function runs unconditionally on every finish, changed or not -- see
-- \"SimpleStateIf.hs\"'s @finishCap@). Releasing first would let such an
-- id's refcount transiently hit zero -- reclaiming it (forward-map delete,
-- reverse-slot clear, free-list push) out from under the retain that was
-- about to follow, corrupting the very id this write is about to store
-- right back into the row's own new span.
writeSrcDeps :: DefTable p a -> RowIdx -> HashSet AnyCompSrcDep -> IO ()
writeSrcDeps dt row s = do
  let sdi = dt_srcDepIntern dt
  oldFlat <- eaRead (dt_srcDeps dt) row singleStride
  newIds <- mapM (sdiIntern sdi) (HashSet.toList s)
  mapM_ (sdiRetain sdi) newIds
  mapM_ (sdiRelease sdi . mkSrcDepId . fromIntegral) (VU.toList oldFlat)
  n <- rowCount dt
  eaWrite (dt_srcDeps dt) row (isAlive dt) n singleStride (VU.fromList (map (fromIntegral . unSrcDepId) newIds))

-- | Test/debug-only: number of currently-live interned src-dep ids in this
-- def's table -- see 'sdiLiveCount'. Exported (unlike the rest of
-- 'SrcDepIntern''s internals) purely so the engine-level churn regression
-- test (Tests/TestStateIf.hs, via a small debug accessor in
-- "SimpleStateIf.hs") can assert the intern table stays bounded through
-- "SimpleStateIf.hs"'s real read/write API, not just this module's own
-- unit tests.
srcDepInternLiveCount :: DefTable p a -> IO Int
srcDepInternLiveCount = sdiLiveCount . dt_srcDepIntern

--
-- Tests
--

h :: Int -> Hash128
h = largeHash128

newTest :: IO (DefTable Int Int)
newTest = new

-- | Test-only shorthand for building 'DefIdx'/'RowIdx' values from plain
-- integer literals -- this module's own test section has the constructors
-- in scope (unlike every other module), so there's no need to go via the
-- public 'mkDefIdx' escape hatch.
di :: Int -> DefIdx
di = DefIdx

ri :: Int -> RowIdx
ri = RowIdx

test_packUnpackRoundTrips :: IO ()
test_packUnpackRoundTrips = do
  assertEqual (di 3, ri 7) (unpackRef (packRef (di 3) (ri 7)))
  assertEqual (di 0, ri 0) (unpackRef (packRef (di 0) (ri 0)))
  assertEqual (di 5, ri 0) (unpackRef (packRef (di 5) (ri 0)))
  assertEqual (di 3) (refDefIdx (packRef (di 3) (ri 7)))
  assertEqual (ri 7) (refRow (packRef (di 3) (ri 7)))

test_newTableIsEmpty :: IO ()
test_newTableIsEmpty = do
  dt <- newTest
  n <- rowCount dt
  assertEqual (RowCount 0) n

test_lookupOrInsertRowIsIdempotent :: IO ()
test_lookupOrInsertRowIsIdempotent = do
  dt <- newTest
  (r1, fresh1) <- lookupOrInsertRow dt (h 1) 100
  (r2, fresh2) <- lookupOrInsertRow dt (h 1) 100
  assertEqual r1 r2
  assertBool fresh1
  assertBool (not fresh2)

test_lookupOrInsertRowDistinctHashesGetDistinctRows :: IO ()
test_lookupOrInsertRowDistinctHashesGetDistinctRows = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  assertBool (r1 /= r2)
  n <- rowCount dt
  assertEqual (RowCount 2) n

test_freeRowThenReinsertNewHashReusesRowNumber :: IO ()
test_freeRowThenReinsertNewHashReusesRowNumber = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  freeRow dt (h 1) r1
  (r2, fresh) <- lookupOrInsertRow dt (h 2) 2
  assertBool fresh
  assertEqual r1 r2
  -- freeing didn't grow the table -- the freed slot was reused, not a new one
  n <- rowCount dt
  assertEqual (RowCount 1) n

test_freeRowRemovesOldHashFromIndex :: IO ()
test_freeRowRemovesOldHashFromIndex = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  freeRow dt (h 1) r1
  -- looking the old hash up again must allocate a *fresh* row (it's gone
  -- from the index), even though the row number happens to be recyclable
  (r2, fresh) <- lookupOrInsertRow dt (h 1) 1
  assertBool fresh
  assertEqual r1 r2

test_manyRowsAreAllDistinctAndCounted :: IO ()
test_manyRowsAreAllDistinctAndCounted = do
  dt <- newTest
  rows <- mapM (\i -> fst <$> lookupOrInsertRow dt (h i) i) [1 .. 200]
  n <- rowCount dt
  assertEqual (RowCount 200) n
  assertEqual 200 (length (HashMap.toList (toSet rows)))
 where
  toSet = HashMap.fromList . map (\r -> (r, ()))

test_freshRowIsAliveWithNoResult :: IO ()
test_freshRowIsAliveWithNoResult = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  alive <- isAlive dt r
  assertBool alive
  f <- readFlags dt r
  assertEqual NoResult (flagsResultState f)
  assertBool (not (flagsPending f))

test_freeRowIsNoLongerAlive :: IO ()
test_freeRowIsNoLongerAlive = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  freeRow dt (h 1) r
  alive <- isAlive dt r
  assertBool (not alive)

test_paramHashRoundTripsThroughLookupOrInsertRow :: IO ()
test_paramHashRoundTripsThroughLookupOrInsertRow = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 42) 42
  got <- readParamHash dt r
  assertEqual (h 42) got

test_resultHashRoundTrips :: IO ()
test_resultHashRoundTrips = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeResultHash dt r (h 99)
  got <- readResultHash dt r
  assertEqual (h 99) got

test_setPendingTogglesFlagWithoutDisturbingOthers :: IO ()
test_setPendingTogglesFlagWithoutDisturbingOthers = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeFlags dt r (mkFlags True False ResultValue)
  setPending dt r True
  f <- readFlags dt r
  assertBool (flagsAlive f)
  assertBool (flagsPending f)
  assertEqual ResultValue (flagsResultState f)
  setPending dt r False
  f' <- readFlags dt r
  assertBool (not (flagsPending f'))

test_mkFlagsResultStateRoundTripsForEveryValue :: IO ()
test_mkFlagsResultStateRoundTripsForEveryValue =
  mapM_
    ( \rs -> do
        let f = mkFlags True True rs
        assertEqual rs (flagsResultState f)
        assertBool (flagsAlive f)
        assertBool (flagsPending f)
    )
    [minBound .. maxBound]

test_hashPairRoundTrips :: IO ()
test_hashPairRoundTrips = do
  assertEqual (h 7) (pairToHash (hashToPair (h 7)))

test_growthAcrossManyInsertsPreservesAllData :: IO ()
test_growthAcrossManyInsertsPreservesAllData = do
  dt <- newTest
  -- force several regrowths (initial/step capacity starts at 4)
  rows <- mapM (\i -> fst <$> lookupOrInsertRow dt (h i) i) [1 .. 500]
  mapM_
    ( \(i, r) -> do
        ph <- readParamHash dt r
        assertEqual (h i) ph
        v <- readParam dt r
        assertEqual i v
        alive <- isAlive dt r
        assertBool alive
    )
    (zip [1 .. 500] rows)

test_valueRoundTrips :: IO ()
test_valueRoundTrips = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeValue dt r 12345
  v <- readValue dt r
  assertEqual (12345 :: Int) v

--
-- Column representation: mkColumn must pick ColUnboxed for the recognized
-- primitive types (mirroring the benchmark's own Word32 param / Word64
-- result) and ColBoxed for everything else (e.g. a non-unboxable param or
-- result type such as String -- the task brief's fallback requirement),
-- and both representations must round-trip correctly through DefTable's
-- ordinary read/write API either way.
--

test_columnPicksUnboxedForRecognizedPrimitiveTypes :: IO ()
test_columnPicksUnboxedForRecognizedPrimitiveTypes = do
  (dtW32 :: DefTable Word32 Word64) <- new
  assertBool (columnIsUnboxed (dt_param dtW32))
  assertBool (columnIsUnboxed (dt_value dtW32))
  (dtOther :: DefTable Bool Double) <- new
  assertBool (columnIsUnboxed (dt_param dtOther))
  assertBool (columnIsUnboxed (dt_value dtOther))

-- | A non-unboxable param/result type (here 'String', standing in for the
-- task brief's @ByteString@ example -- real param types in the test suite
-- include both) must fall back to 'ColBoxed', not fail to build.
test_columnFallsBackToBoxedForNonUnboxableTypes :: IO ()
test_columnFallsBackToBoxedForNonUnboxableTypes = do
  (dt :: DefTable String String) <- new
  assertBool (not (columnIsUnboxed (dt_param dt)))
  assertBool (not (columnIsUnboxed (dt_value dt)))

-- | Round-trip through the unboxed path (Word32 param / Word64 result,
-- exactly the benchmark's def shape) with values that exercise the full
-- width of each type.
test_unboxedColumnParamValueRoundTrip :: IO ()
test_unboxedColumnParamValueRoundTrip = do
  (dt :: DefTable Word32 Word64) <- new
  (r, _) <- lookupOrInsertRow dt (h 1) maxBound
  writeValue dt r maxBound
  p <- readParam dt r
  v <- readValue dt r
  assertEqual (maxBound :: Word32) p
  assertEqual (maxBound :: Word64) v

-- | Round-trip through the boxed fallback path with a non-unboxable type
-- (String), the case the task brief specifically asks to be covered: a
-- real param/result type that can never satisfy an 'Unbox' constraint
-- (like 'Data.ByteString.ByteString' in the wider test suite) must keep
-- working unchanged.
test_boxedColumnParamValueRoundTrip :: IO ()
test_boxedColumnParamValueRoundTrip = do
  (dt :: DefTable String String) <- new
  (r, _) <- lookupOrInsertRow dt (h 1) "hello param"
  writeValue dt r "hello value"
  p <- readParam dt r
  v <- readValue dt r
  assertEqual "hello param" p
  assertEqual "hello value" v

test_freshRowHasEmptyEdges :: IO ()
test_freshRowHasEmptyEdges = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  cd <- readCompDeps dt r
  rd <- readRdeps dt r
  sd <- readSrcDeps dt r
  assertEqual VU.empty cd
  assertEqual VU.empty rd
  assertEqual HashSet.empty sd

test_compDepsAndRdepsRoundTrip :: IO ()
test_compDepsAndRdepsRoundTrip = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  let cd = VU.fromList [(10, 1, 2), (20, 3, 4)]
  writeCompDeps dt r cd
  got <- readCompDeps dt r
  assertEqual cd got
  assertEqual (VU.fromList [10, 20]) (compDepTargets got)
  let rd = VU.fromList [30, 40, 50]
  writeRdeps dt r rd
  gotRd <- readRdeps dt r
  assertEqual rd gotRd

-- | The sentinel used for "no result observed" must not collide with any
-- real hash pair produced by 'hashToPair' -- spot-check a few concrete
-- hashes against it.
test_noResultSentinelDoesNotCollideWithRealHashes :: IO ()
test_noResultSentinelDoesNotCollideWithRealHashes =
  mapM_
    (\i -> assertBool (hashToPair (h i) /= noResultSentinel))
    [1 .. 50 :: Int]

--
-- Src-dep interning: a minimal 'CompSrc' instance for building real
-- 'AnyCompSrcDep' test values (mirrors Tests/TestStateIf.hs's
-- @TestStateSrc@), plus tests covering the interned-CSR-arena
-- representation directly against the module's internals (this file's own
-- test section has access to 'SrcDepIntern', unlike SimpleStateIf.hs which
-- only ever sees decoded 'HashSet' 'AnyCompSrcDep' values through the
-- unchanged public API).
--

data TestSrc = TestSrc deriving (Show, Eq, Typeable)

data VoidRequest a

instance Hashable TestSrc where
  hashWithSalt s TestSrc = hashWithSalt s (0 :: Int)

instance CompSrc TestSrc where
  type CompSrcReq TestSrc = VoidRequest
  type CompSrcKey TestSrc = String
  type CompSrcVer TestSrc = Int
  compSrcInstanceId _ = CompSrcInstanceId (fromString "TestSrc")
  compSrcExecute _ act = case act of {}
  compSrcUnregister _ _ = pure ()
  compSrcWaitChanges _ = retry

srcDep :: String -> Int -> AnyCompSrcDep
srcDep k v = wrapCompSrcDep TestSrc (Dep k v)

test_srcDepsRoundTrip :: IO ()
test_srcDepsRoundTrip = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  let s = HashSet.fromList [srcDep "a" 1, srcDep "b" 1]
  writeSrcDeps dt r s
  got <- readSrcDeps dt r
  assertEqual s got

-- | The same @(key, version)@ pair, written by two different rows, must be
-- interned exactly once -- this is the whole point of the rewrite (see the
-- module haddock's "Src-dep interning" section): rows sharing a source dep
-- share one heap copy via a small shared id, not a private copy each.
test_srcDepsInterningDedupesEqualValues :: IO ()
test_srcDepsInterningDedupesEqualValues = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  writeSrcDeps dt r1 (HashSet.singleton (srcDep "shared" 1))
  writeSrcDeps dt r2 (HashSet.singleton (srcDep "shared" 1))
  count <- readIORef (sdi_count (dt_srcDepIntern dt))
  assertEqual 1 count
  got1 <- readSrcDeps dt r1
  got2 <- readSrcDeps dt r2
  assertEqual (HashSet.singleton (srcDep "shared" 1)) got1
  assertEqual (HashSet.singleton (srcDep "shared" 1)) got2

-- | Distinct src deps get distinct ids -- including the case that actually
-- drives real-world interning table growth: the *same key* observed at a
-- *different version* is a genuinely distinct 'AnyCompSrcDep' (see
-- CompSrc.hs's @SomeCompSrcDep@, which wraps key and version together), not
-- deduplicated against the old version.
test_srcDepsDistinctValuesGetDistinctIds :: IO ()
test_srcDepsDistinctValuesGetDistinctIds = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r (HashSet.fromList [srcDep "k" 1, srcDep "k" 2, srcDep "other" 1])
  count <- readIORef (sdi_count (dt_srcDepIntern dt))
  assertEqual 3 count
  got <- readSrcDeps dt r
  assertEqual (HashSet.fromList [srcDep "k" 1, srcDep "k" 2, srcDep "other" 1]) got

-- | Overwriting a row's src deps repeatedly (the live-update path's actual
-- write pattern) must keep reading back the *latest* set, and must force at
-- least one compaction of the shared arena along the way -- exercising the
-- same append-new-span/mark-old-span-dead machinery 'writeCompDeps'/
-- 'writeRdeps' already get covered for, now via the interned-id path.
test_srcDepsManyOverwritesKeepLatestCorrectAndCompact :: IO ()
test_srcDepsManyOverwritesKeepLatestCorrectAndCompact = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  mapM_
    (\i -> writeSrcDeps dt r (HashSet.singleton (srcDep "churn" i)))
    [1 .. 5000 :: Int]
  got <- readSrcDeps dt r
  assertEqual (HashSet.singleton (srcDep "churn" 5000)) got

-- | A freed row's src-dep arena span is released (unlike @compDeps@/
-- @rdeps@, which stay orphaned garbage until compaction -- see the module
-- haddock for why src-deps can't wait): a *fresh* occupant of the recycled
-- row number must never see the old occupant's src deps.
test_srcDepsResetOnRowReuse :: IO ()
test_srcDepsResetOnRowReuse = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r1 (HashSet.singleton (srcDep "old" 1))
  freeRow dt (h 1) r1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  assertEqual r1 r2
  got <- readSrcDeps dt r2
  assertEqual HashSet.empty got

--
-- Src-dep refcounting -- Part 1's correctness fix (module haddock's "Ids
-- are refcounted and recycled"). Everything below exercises the intern
-- table's internal state directly (this file's own test section, unlike
-- SimpleStateIf.hs, has access to SrcDepIntern) as well as through the
-- public DefTable read/write API.
--

test_srcDepInternLiveCountTracksDistinctValuesReferenced :: IO ()
test_srcDepInternLiveCountTracksDistinctValuesReferenced = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  c0 <- srcDepInternLiveCount dt
  assertEqual 0 c0
  writeSrcDeps dt r (HashSet.fromList [srcDep "a" 1, srcDep "b" 1])
  c1 <- srcDepInternLiveCount dt
  assertEqual 2 c1

-- | Freeing the only row that referenced a src dep must reclaim its
-- interned id -- the whole point of the refcounting fix (see the module
-- haddock's "Ids are refcounted and recycled"): the live-count must drop
-- back to zero, not stay pinned at its high-water mark the way the
-- pre-refcounting, never-recycled table would have.
test_freeingOnlyReferencingRowReclaimsSrcDepId :: IO ()
test_freeingOnlyReferencingRowReclaimsSrcDepId = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r (HashSet.singleton (srcDep "solo" 1))
  c1 <- srcDepInternLiveCount dt
  assertEqual 1 c1
  freeRow dt (h 1) r
  c2 <- srcDepInternLiveCount dt
  assertEqual 0 c2

-- | The core refcounting invariant: a src dep shared by two rows must
-- survive one of them freeing -- only when *both* referencing rows are
-- gone does the id actually get reclaimed. A naive (non-refcounted, or
-- incorrectly-ordered) implementation would either reclaim the shared id
-- the moment the first row frees (corrupting the still-live row's reads)
-- or never reclaim it at all.
test_freeingRowWithSharedSrcDepDoesNotDropSharedId :: IO ()
test_freeingRowWithSharedSrcDepDoesNotDropSharedId = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  writeSrcDeps dt r1 (HashSet.singleton (srcDep "shared" 1))
  writeSrcDeps dt r2 (HashSet.singleton (srcDep "shared" 1))
  c1 <- srcDepInternLiveCount dt
  assertEqual 1 c1
  freeRow dt (h 1) r1
  -- still referenced by r2 -- must not have been reclaimed
  c2 <- srcDepInternLiveCount dt
  assertEqual 1 c2
  got2 <- readSrcDeps dt r2
  assertEqual (HashSet.singleton (srcDep "shared" 1)) got2
  freeRow dt (h 2) r2
  -- now genuinely unreferenced
  c3 <- srcDepInternLiveCount dt
  assertEqual 0 c3

-- | The ordering-hazard case the module haddock calls out by name: a
-- write that keeps one src dep, drops another, and adds a third, over and
-- over. The *kept* one is present in both the write's old and new span on
-- every iteration -- if retain-before-release weren't respected, its
-- refcount would transiently hit zero at some point and get reclaimed out
-- from under the row that's still supposed to reference it.
test_overlappingOverwriteNeverDropsTheSharedId :: IO ()
test_overlappingOverwriteNeverDropsTheSharedId = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r (HashSet.fromList [srcDep "kept" 1, srcDep "gen0" 1])
  forM_ [1 .. 200 :: Int] $ \i -> do
    writeSrcDeps dt r (HashSet.fromList [srcDep "kept" 1, srcDep ("gen" ++ show i) 1])
    got <- readSrcDeps dt r
    assertBool (HashSet.member (srcDep "kept" 1) got)
  -- after the loop only "kept" and the final generation's id remain live
  c <- srcDepInternLiveCount dt
  assertEqual 2 c

-- | Distinct versions of the same key are distinct 'AnyCompSrcDep' values
-- (see 'test_srcDepsDistinctValuesGetDistinctIds'); replacing one version
-- with another via 'writeSrcDeps' must reclaim the old version's id once
-- nothing else references it, not accumulate one id per version forever
-- -- this is the concrete leak Part 1 fixes (the module haddock's
-- "Ids are refcounted and recycled").
test_versionChurnOnSingleRowKeepsInternTableBounded :: IO ()
test_versionChurnOnSingleRowKeepsInternTableBounded = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  forM_ [1 .. 5000 :: Int] $ \i ->
    writeSrcDeps dt r (HashSet.singleton (srcDep "churn" i))
  c <- srcDepInternLiveCount dt
  assertEqual 1 c
  got <- readSrcDeps dt r
  assertEqual (HashSet.singleton (srcDep "churn" 5000)) got

-- | A reused id (popped off the free list by 'sdiIntern') must behave
-- exactly like a fresh one: readable once retained, and the *previous*
-- occupant's identity must never leak through a resolve of the recycled
-- slot. Forces reuse directly: free a row (releasing its sole src dep to
-- refcount zero, recycling the id) and immediately intern a *different*
-- value on another row, which -- given a table with only that one freed
-- slot on its free list -- must land on the recycled id.
test_reusedSrcDepIdBehavesLikeFreshId :: IO ()
test_reusedSrcDepIdBehavesLikeFreshId = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r1 (HashSet.singleton (srcDep "first" 1))
  freeRow dt (h 1) r1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  writeSrcDeps dt r2 (HashSet.singleton (srcDep "second" 1))
  got <- readSrcDeps dt r2
  assertEqual (HashSet.singleton (srcDep "second" 1)) got
  c <- srcDepInternLiveCount dt
  assertEqual 1 c

-- | Resolving a *freed* (refcount-zero) src-dep id must fail loudly, not
-- silently return stale or wrong data -- the "range check alone isn't
-- enough once ids recycle" contract the module haddock calls out. Reaches
-- the dead id indirectly through the public API: write a solo src dep,
-- capture what its row currently holds isn't directly observable (ids are
-- module-internal), so this drives the scenario through
-- 'readSrcDeps'/'writeSrcDeps' on a row whose dependency set changes,
-- confirming the *old* value is gone from the interned table (live count
-- drops) and cannot be produced by any subsequent read.
test_deadSrcDepIdIsNeverResolvableAfterRelease :: IO ()
test_deadSrcDepIdIsNeverResolvableAfterRelease = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt r (HashSet.singleton (srcDep "gone-soon" 1))
  c1 <- srcDepInternLiveCount dt
  assertEqual 1 c1
  writeSrcDeps dt r (HashSet.singleton (srcDep "replacement" 1))
  c2 <- srcDepInternLiveCount dt
  assertEqual 1 c2
  got <- readSrcDeps dt r
  assertEqual (HashSet.singleton (srcDep "replacement" 1)) got

-- | Direct unit test against 'SrcDepIntern' itself: resolving a released
-- id must throw, not return the stale value or a range-check false
-- negative -- this is the loud-failure contract 'sdiResolve' is required
-- to strengthen (the module haddock's liveness-check note), tested here
-- against the primitive directly rather than only observed indirectly
-- through 'readSrcDeps'.
test_sdiResolveFailsLoudlyOnReleasedId :: IO ()
test_sdiResolveFailsLoudlyOnReleasedId = do
  sdi <- newSrcDepIntern
  let dep = srcDep "temp" 1
  i <- sdiIntern sdi dep
  sdiRetain sdi i
  sdiRelease sdi i
  result <- try (evaluate =<< sdiResolve sdi i) :: IO (Either SomeException AnyCompSrcDep)
  assertBool (isLeft result)

-- | 'sdiResolve' on an id that was never assigned at all (out of range)
-- must also fail loudly -- the other half of the loud-failure contract,
-- unrelated to recycling.
test_sdiResolveFailsLoudlyOnNeverAssignedId :: IO ()
test_sdiResolveFailsLoudlyOnNeverAssignedId = do
  sdi <- newSrcDepIntern
  result <- try (evaluate =<< sdiResolve sdi (mkSrcDepId 42)) :: IO (Either SomeException AnyCompSrcDep)
  assertBool (isLeft result)

-- | 'sdiRelease' on an id already at refcount zero (an underflow -- every
-- real caller pairs release with a prior retain, so this can only happen
-- from a caller bug) must fail loudly rather than silently going negative
-- or double-freeing the slot.
test_sdiReleaseUnderflowFailsLoudly :: IO ()
test_sdiReleaseUnderflowFailsLoudly = do
  sdi <- newSrcDepIntern
  let dep = srcDep "never-retained" 1
  i <- sdiIntern sdi dep
  result <- try (evaluate =<< sdiRelease sdi i) :: IO (Either SomeException ())
  assertBool (isLeft result)

-- | A row that never had any src deps must not appear to reference
-- anything on free -- 'freeRow' on such a row is a pure no-op as far as
-- the intern table is concerned (nothing to release, nothing to
-- underflow).
test_freeingRowWithNoSrcDepsIsInternTableNoOp :: IO ()
test_freeingRowWithNoSrcDepsIsInternTableNoOp = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  c0 <- srcDepInternLiveCount dt
  freeRow dt (h 1) r
  c1 <- srcDepInternLiveCount dt
  assertEqual c0 c1
  assertEqual 0 c1

--
-- Row reuse and garbage tolerance (Rust Stage 5's rules, ported): freeing a
-- row must not physically clear its result-hash/value/edge columns -- only
-- flags, param hash, and the caller-visible param/edge-reset fields that
-- lookupOrInsertRow re-establishes on the *next* occupant. Everything else
-- is left as garbage until overwritten, safe only because every read is
-- gated behind a flags check that reuse unconditionally clears first.
--

test_freeRowLeavesResultHashAndValueAsGarbageButFlagsGateThem :: IO ()
test_freeRowLeavesResultHashAndValueAsGarbageButFlagsGateThem = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 111
  writeResultHash dt r1 (h 999)
  writeValue dt r1 (777 :: Int)
  freeRow dt (h 1) r1
  -- the row is dead -- callers must not trust it without checking flags
  -- first (this module doesn't force-clear result_hash/value: reusing the
  -- row is what re-establishes a safe state, matching Rust Stage 5)
  alive <- isAlive dt r1
  assertBool (not alive)
  -- reuse: a fresh occupant of the same row number gets fresh flags/param,
  -- but the physical result_hash/value bytes are untouched garbage until
  -- this new occupant's own finish overwrites them -- readable-but-stale,
  -- never observed because nothing reads them without a flags check first.
  (r2, fresh) <- lookupOrInsertRow dt (h 2) 222
  assertBool fresh
  assertEqual r1 r2
  f <- readFlags dt r2
  assertEqual NoResult (flagsResultState f)
  assertBool (flagsAlive f)
  staleHash <- readResultHash dt r2
  assertEqual (h 999) staleHash -- old bytes, still physically there
  staleVal <- readValue dt r2
  assertEqual (777 :: Int) staleVal -- old bytes, still physically there
  -- but the *new* occupant's own param is correct, not garbage
  p <- readParam dt r2
  assertEqual (222 :: Int) p

test_freeRowResetsEdgesOnReuse :: IO ()
test_freeRowResetsEdgesOnReuse = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 1
  writeCompDeps dt r1 (VU.fromList [(5, 1, 2)])
  writeRdeps dt r1 (VU.fromList [7, 8])
  freeRow dt (h 1) r1
  (r2, _) <- lookupOrInsertRow dt (h 2) 2
  assertEqual r1 r2
  cd <- readCompDeps dt r2
  rd <- readRdeps dt r2
  assertEqual VU.empty cd
  assertEqual VU.empty rd

--
-- Edge-arena specific: repeated overwrite (the pattern that drives the
-- append-only arena's growth and eventually forces a compaction), and
-- compaction's interaction with rows that are alive vs. freed.
--

test_manyOverwritesOfSameRowKeepLatestEdgesCorrect :: IO ()
test_manyOverwritesOfSameRowKeepLatestEdgesCorrect = do
  dt <- newTest
  (r, _) <- lookupOrInsertRow dt (h 1) 1
  -- comfortably past compactMinWords (4096) at stride 3, so this forces at
  -- least one compaction of the compDeps arena along the way.
  mapM_
    ( \i -> do
        let cd = VU.fromList [(i, fromIntegral i, fromIntegral (i + 1))]
        writeCompDeps dt r cd
        let rd = VU.fromList [i, (i + 1), (i + 2)]
        writeRdeps dt r rd
    )
    [1 .. 3000]
  gotCd <- readCompDeps dt r
  assertEqual (VU.fromList [(3000, 3000, 3001)]) gotCd
  gotRd <- readRdeps dt r
  assertEqual (VU.fromList [3000, 3001, 3002]) gotRd

test_compactionDoesNotDisturbOtherAliveRowsEdges :: IO ()
test_compactionDoesNotDisturbOtherAliveRowsEdges = do
  dt <- newTest
  (rHot, _) <- lookupOrInsertRow dt (h 0) 0
  -- a handful of bystander rows, each given a distinct, stable edge set
  -- that must still read back correctly after the hot row's repeated
  -- overwrites force one or more compactions of the shared arena.
  bystanders <- mapM (\i -> lookupOrInsertRow dt (h i) i) [1 .. 10]
  mapM_
    ( \(r, i) -> do
        writeCompDeps dt r (VU.fromList [((100 + i), fromIntegral i, fromIntegral i)])
        writeRdeps dt r (VU.fromList [(200 + i)])
    )
    (zip (map fst bystanders) [1 .. 10])
  mapM_
    ( \i -> writeCompDeps dt rHot (VU.fromList [(i, fromIntegral i, fromIntegral i)])
    )
    [1 .. 3000 :: Int]
  mapM_
    ( \(r, i) -> do
        cd <- readCompDeps dt r
        assertEqual (VU.fromList [((100 + i), fromIntegral i, fromIntegral i)]) cd
        rd <- readRdeps dt r
        assertEqual (VU.fromList [(200 + i)]) rd
    )
    (zip (map fst bystanders) [1 .. 10])

test_freedRowsDontResurfaceAfterCompaction :: IO ()
test_freedRowsDontResurfaceAfterCompaction = do
  dt <- newTest
  (rKeep, _) <- lookupOrInsertRow dt (h 0) 0
  writeCompDeps dt rKeep (VU.fromList [(999, 1, 1)])
  (rDoomed, _) <- lookupOrInsertRow dt (h 1) 1
  writeCompDeps dt rDoomed (VU.fromList [(888, 2, 2)])
  freeRow dt (h 1) rDoomed
  -- force compaction via repeated overwrites of the surviving row
  mapM_
    (\i -> writeCompDeps dt rKeep (VU.fromList [(i, fromIntegral i, fromIntegral i)]))
    [1 .. 3000 :: Int]
  aliveKeep <- isAlive dt rKeep
  assertBool aliveKeep
  cd <- readCompDeps dt rKeep
  assertEqual (VU.fromList [(3000, 3000, 3000)]) cd
  aliveDoomed <- isAlive dt rDoomed
  assertBool (not aliveDoomed)

-- | The srcDeps-arena analogue of 'test_compactionDoesNotDisturbOtherAliveRowsEdges',
-- with the added wrinkle that matters specifically for src-deps: a
-- bystander row's src dep is *shared* with the row being churned, so this
-- also checks that forcing a compaction of the shared arena doesn't
-- disturb the shared id's refcount or resolvability.
test_srcDepsCompactionDoesNotDisturbBystanderRowsOrSharedIds :: IO ()
test_srcDepsCompactionDoesNotDisturbBystanderRowsOrSharedIds = do
  dt <- newTest
  (rHot, _) <- lookupOrInsertRow dt (h 0) 0
  bystanders <- mapM (\i -> lookupOrInsertRow dt (h i) i) [1 .. 10]
  -- every bystander shares one common src dep plus one of its own
  mapM_
    ( \(r, i) ->
        writeSrcDeps dt r (HashSet.fromList [srcDep "common" 1, srcDep ("own" ++ show i) 1])
    )
    (zip (map fst bystanders) [1 .. 10 :: Int])
  -- force several compactions of the shared srcDeps arena via the hot row
  mapM_
    (\i -> writeSrcDeps dt rHot (HashSet.singleton (srcDep "common" 1)) >> writeSrcDeps dt rHot (HashSet.singleton (srcDep ("hotgen" ++ show i) 1)))
    [1 .. 2000 :: Int]
  mapM_
    ( \(r, i) -> do
        got <- readSrcDeps dt r
        assertEqual (HashSet.fromList [srcDep "common" 1, srcDep ("own" ++ show i) 1]) got
    )
    (zip (map fst bystanders) [1 .. 10 :: Int])
  -- "common" is still referenced by every bystander plus rHot's current span
  c <- srcDepInternLiveCount dt
  assertBool (c > 0)

-- | A row freed and reused *between* two forced compactions of the
-- srcDeps arena -- the specific interleaving the task brief calls out.
-- The freed-and-reused row's own src dep must never resurface, and a
-- second compaction after reuse must still leave everything else correct.
test_rowFreedAndReusedBetweenTwoSrcDepsCompactions :: IO ()
test_rowFreedAndReusedBetweenTwoSrcDepsCompactions = do
  dt <- newTest
  (rHot, _) <- lookupOrInsertRow dt (h 0) 0
  (rVictim, _) <- lookupOrInsertRow dt (h 1) 1
  writeSrcDeps dt rVictim (HashSet.singleton (srcDep "victim-only" 1))
  -- first compaction, forced via the hot row
  mapM_ (\i -> writeSrcDeps dt rHot (HashSet.singleton (srcDep ("gen" ++ show i) 1))) [1 .. 2000 :: Int]
  freeRow dt (h 1) rVictim
  (rVictim', _) <- lookupOrInsertRow dt (h 2) 2
  assertEqual rVictim rVictim'
  gotFresh <- readSrcDeps dt rVictim'
  assertEqual HashSet.empty gotFresh
  writeSrcDeps dt rVictim' (HashSet.singleton (srcDep "new-occupant" 1))
  -- second compaction, again forced via the hot row
  mapM_ (\i -> writeSrcDeps dt rHot (HashSet.singleton (srcDep ("gen2-" ++ show i) 1))) [1 .. 2000 :: Int]
  gotAfter <- readSrcDeps dt rVictim'
  assertEqual (HashSet.singleton (srcDep "new-occupant" 1)) gotAfter
  -- the old occupant's src dep must be gone from the intern table entirely
  gotHot <- readSrcDeps dt rHot
  assertBool (not (HashSet.member (srcDep "victim-only" 1) gotAfter))
  assertBool (not (HashSet.member (srcDep "victim-only" 1) gotHot))

-- | Comprehensive row-reuse garbage check: every column a fresh occupant
-- of a recycled row number could conceivably see stale data through --
-- param hash, flags/result-state, param, value, all three edge arenas, and
-- the hash index -- must reflect only the *new* occupant, never the old
-- one. Combines what several narrower tests above check individually into
-- one end-to-end pass over the whole row.
test_rowReuseSeesNoStaleGarbageInAnyColumn :: IO ()
test_rowReuseSeesNoStaleGarbageInAnyColumn = do
  dt <- newTest
  (r1, _) <- lookupOrInsertRow dt (h 1) 111
  writeResultHash dt r1 (h 555)
  writeValue dt r1 (777 :: Int)
  writeFlags dt r1 (mkFlags True False ResultValue)
  writeCompDeps dt r1 (VU.fromList [(9, 1, 2)])
  writeRdeps dt r1 (VU.fromList [9, 10])
  writeSrcDeps dt r1 (HashSet.singleton (srcDep "old-occupant" 1))
  freeRow dt (h 1) r1

  (r2, fresh) <- lookupOrInsertRow dt (h 2) 222
  assertBool fresh
  assertEqual r1 r2 -- confirms this is actually testing row *reuse*

  -- param hash: fresh, not the old occupant's
  ph <- readParamHash dt r2
  assertEqual (h 2) ph
  -- flags: fresh alive/not-pending/no-result, not the old ResultValue
  f <- readFlags dt r2
  assertBool (flagsAlive f)
  assertBool (not (flagsPending f))
  assertEqual NoResult (flagsResultState f)
  -- param: fresh
  p <- readParam dt r2
  assertEqual (222 :: Int) p
  -- edges: all three arenas reset, not the old occupant's
  cd <- readCompDeps dt r2
  rd <- readRdeps dt r2
  sd <- readSrcDeps dt r2
  assertEqual VU.empty cd
  assertEqual VU.empty rd
  assertEqual HashSet.empty sd
  -- hash index: the old hash (h 1) must no longer resolve to anything --
  -- looking it up again must be a fresh insert, not a hit on some
  -- leftover index entry from the freed occupant.
  (_, foundOldIsFresh) <- lookupOrInsertRow dt (h 1) 999
  assertBool foundOldIsFresh
  -- the old occupant's src dep must be entirely gone from the intern table
  c <- srcDepInternLiveCount dt
  assertEqual 0 c

-- | Index growth interleaved with row freeing: insert enough distinct
-- hashes to force several 'HashIndex' growths, freeing every other row
-- along the way (so growth and backward-shift deletion both fire in the
-- same sequence, not in isolation the way the standalone HashIndex tests
-- above exercise them). Every row that stayed alive must remain correctly
-- indexed once the churn settles.
test_indexGrowthInterleavedWithFreeingStaysCorrect :: IO ()
test_indexGrowthInterleavedWithFreeingStaysCorrect = do
  dt <- newTest
  let n = 300
  rows <- mapM (\i -> fst <$> lookupOrInsertRow dt (h i) i) [1 .. n]
  -- free every other row, forcing backward-shift deletes interleaved with
  -- whatever growths already happened while they were all being inserted
  forM_ (zip [1 :: Int ..] rows) $ \(i, r) ->
    when (even i) $ freeRow dt (h i) r
  -- the surviving (odd-indexed) rows must all still resolve to their own row
  forM_ (zip [1 :: Int ..] rows) $ \(i, r) ->
    when (odd i) $ do
      (r', fresh) <- lookupOrInsertRow dt (h i) i
      assertBool (not fresh)
      assertEqual r r'
  -- and fresh inserts past the churn must still work and grow correctly
  moreRows <- mapM (\i -> fst <$> lookupOrInsertRow dt (h (n + i)) (n + i)) [1 .. 100]
  assertEqual 100 (length moreRows)

--
-- Hash index: standalone tests against a small in-memory hash column (not
-- routed through a full DefTable), covering insert/lookup/delete/grow and
-- backward-shift's collision handling directly -- plus a couple of
-- DefTable-level integration/churn tests at the end.
--

-- | A hash built from explicit hi\/lo words, for tests that need to force
-- specific slots\/collisions deterministically rather than relying on
-- 'largeHash128''s MD5 output landing where a test wants it.
rawHash :: Word64 -> Word64 -> Hash128
rawHash hi lo = Hash128 (LH.Word128 hi lo)

-- | A tiny mutable row->hash table for standalone 'HashIndex' tests: plays
-- the role 'DefTable''s own @param_hash@ column plays for the real thing.
newHashColumn :: IO (IORef (VM.IOVector Hash128))
newHashColumn = newIORef =<< VM.new 256

setHashColumn :: IORef (VM.IOVector Hash128) -> RowIdx -> Hash128 -> IO ()
setHashColumn ref row hv = readIORef ref >>= \v -> VM.write v (unRowIdx row) hv

-- | Matches 'GetHash''s shape, partially applied to a concrete column --
-- exactly how a real 'DefTable''s 'readParamHash' is used at this callback.
getHashColumn :: IORef (VM.IOVector Hash128) -> GetHash
getHashColumn ref row = readIORef ref >>= \v -> VM.read v (unRowIdx row)

test_hashIndexLookupMissOnEmptyTable :: IO ()
test_hashIndexLookupMissOnEmptyTable = do
  hix <- newHashIndex
  col <- newHashColumn
  found <- hixLookup hix (getHashColumn col) (rawHash 1 1)
  assertEqual Nothing found

test_hashIndexInsertThenLookupFindsRow :: IO ()
test_hashIndexInsertThenLookupFindsRow = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 42 42
  setHashColumn col (ri 5) hv
  hixInsert hix (getHashColumn col) hv (ri 5)
  found <- hixLookup hix (getHashColumn col) hv
  assertEqual (Just (ri 5)) found

test_hashIndexLookupMissForDifferentHash :: IO ()
test_hashIndexLookupMissForDifferentHash = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 1 1
  setHashColumn col (ri 0) hv
  hixInsert hix (getHashColumn col) hv (ri 0)
  found <- hixLookup hix (getHashColumn col) (rawHash 2 2)
  assertEqual Nothing found

test_hashIndexDeleteThenLookupMisses :: IO ()
test_hashIndexDeleteThenLookupMisses = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 7 7
  setHashColumn col (ri 3) hv
  hixInsert hix (getHashColumn col) hv (ri 3)
  hixDelete hix (getHashColumn col) hv
  found <- hixLookup hix (getHashColumn col) hv
  assertEqual Nothing found

test_hashIndexDeleteOnMissingKeyIsNoOp :: IO ()
test_hashIndexDeleteOnMissingKeyIsNoOp = do
  hix <- newHashIndex
  col <- newHashColumn
  let hv = rawHash 9 9
  setHashColumn col (ri 0) hv
  hixInsert hix (getHashColumn col) hv (ri 0)
  -- deleting an absent key must not throw and must not disturb the real entry
  hixDelete hix (getHashColumn col) (rawHash 123 456)
  found <- hixLookup hix (getHashColumn col) hv
  assertEqual (Just (ri 0)) found

-- | Force several entries into the same initial-capacity (8) probe chain by
-- picking hi words that collide mod 8, and check every entry is still
-- individually findable -- exercises linear-probe collision resolution
-- rather than the (overwhelmingly likely) no-collision case a few random
-- inserts would hit.
test_hashIndexCollisionsAllFindable :: IO ()
test_hashIndexCollisionsAllFindable = do
  hix <- newHashIndex
  col <- newHashColumn
  let his = [0, 8, 16, 24, 32] -- all `mod 8 == 0` at the initial capacity
  mapM_
    ( \(i, hiW) -> do
        let hv = rawHash hiW 0
        setHashColumn col (ri i) hv
        hixInsert hix (getHashColumn col) hv (ri i)
    )
    (zip [0 ..] his)
  mapM_
    ( \(i, hiW) -> do
        found <- hixLookup hix (getHashColumn col) (rawHash hiW 0)
        assertEqual (Just (ri i)) found
    )
    (zip [0 ..] his)

-- | Deleting the *middle* of a collision chain must not strand the entries
-- that come after it in the probe sequence -- the case backward-shift
-- deletion exists to handle (a plain "just clear the slot" delete would
-- break their reachability).
test_hashIndexDeleteMiddleOfCollisionChainKeepsOthersFindable :: IO ()
test_hashIndexDeleteMiddleOfCollisionChainKeepsOthersFindable = do
  hix <- newHashIndex
  col <- newHashColumn
  let hvs = [rawHash hiW 0 | hiW <- [0, 8, 16]] -- all collide to slot 0
  mapM_ (\(i, hv) -> setHashColumn col (ri i) hv >> hixInsert hix (getHashColumn col) hv (ri i)) (zip [0 ..] hvs)
  hixDelete hix (getHashColumn col) (hvs !! 1) -- delete the middle one (hi=8, row 1)
  found0 <- hixLookup hix (getHashColumn col) (hvs !! 0)
  found1 <- hixLookup hix (getHashColumn col) (hvs !! 1)
  found2 <- hixLookup hix (getHashColumn col) (hvs !! 2)
  assertEqual (Just (ri 0)) found0
  assertEqual Nothing found1
  assertEqual (Just (ri 2)) found2

test_hashIndexGrowsAndPreservesAllEntries :: IO ()
test_hashIndexGrowsAndPreservesAllEntries = do
  hix <- newHashIndex
  col <- newHashColumn
  -- comfortably past the initial capacity (8) at load factor 0.7, forcing
  -- at least two doublings
  let n = 100
  mapM_
    ( \i -> do
        let hv = rawHash (fromIntegral i) (fromIntegral i)
        setHashColumn col (ri i) hv
        hixInsert hix (getHashColumn col) hv (ri i)
    )
    [0 .. n - 1]
  mapM_
    ( \i -> do
        let hv = rawHash (fromIntegral i) (fromIntegral i)
        found <- hixLookup hix (getHashColumn col) hv
        assertEqual (Just (ri i)) found
    )
    [0 .. n - 1]

-- | Alternately insert-then-delete the same row many times over (the
-- pattern the engine's live-update path drives: a row is freed and its
-- hash slot recycled, over and over, on the same handful of hot rows) --
-- the property backward-shift deletion (no tombstones) exists to protect:
-- capacity must not creep up from churn alone, and every still-live entry
-- must stay findable throughout.
test_hashIndexChurnDoesNotDegradeCapacityOrCorrectness :: IO ()
test_hashIndexChurnDoesNotDegradeCapacityOrCorrectness = do
  hix <- newHashIndex
  col <- newHashColumn
  -- a couple of entries that stay live for the whole test, to check they
  -- remain findable throughout the churn of everything else
  let stableA = rawHash 1001 1001
      stableB = rawHash 1002 1002
  setHashColumn col (ri 200) stableA
  hixInsert hix (getHashColumn col) stableA (ri 200)
  setHashColumn col (ri 201) stableB
  hixInsert hix (getHashColumn col) stableB (ri 201)
  forM_ [1 .. 500 :: Int] $ \i -> do
    let hv = rawHash (fromIntegral (1000000 + i)) 0
    setHashColumn col (ri 0) hv
    hixInsert hix (getHashColumn col) hv (ri 0)
    found <- hixLookup hix (getHashColumn col) hv
    assertEqual (Just (ri 0)) found
    hixDelete hix (getHashColumn col) hv
    foundAfter <- hixLookup hix (getHashColumn col) hv
    assertEqual Nothing foundAfter
  table <- readIORef (hix_table hix)
  -- only the two stable entries remain live; capacity must not have grown
  -- past what two live entries need (still the initial 8), since
  -- backward-shift deletion leaves no tombstones behind to inflate it
  assertEqual 8 (VUM.length table)
  count <- readIORef (hix_count hix)
  assertEqual 2 count
  foundA <- hixLookup hix (getHashColumn col) stableA
  foundB <- hixLookup hix (getHashColumn col) stableB
  assertEqual (Just (ri 200)) foundA
  assertEqual (Just (ri 201)) foundB

-- | The same churn pattern, but through 'DefTable''s own public API
-- (lookupOrInsertRow / freeRow) rather than 'HashIndex' directly -- the
-- live-update path's actual call shape -- checking that a row untouched by
-- the churn is unaffected and that every freed hash is genuinely gone.
test_defTableChurnManyFreeReuseCyclesStayCorrect :: IO ()
test_defTableChurnManyFreeReuseCyclesStayCorrect = do
  dt <- newTest
  (rSteady, _) <- lookupOrInsertRow dt (h 999999) 999999
  forM_ [1 .. 500 :: Int] $ \i -> do
    (r, fresh) <- lookupOrInsertRow dt (h i) i
    assertBool fresh
    freeRow dt (h i) r
    -- the freed hash must not still be findable -- re-inserting it must
    -- allocate fresh, not hit a stale index entry
    (r', fresh') <- lookupOrInsertRow dt (h i) i
    assertBool fresh'
    freeRow dt (h i) r'
  -- the row that stayed alive throughout must be entirely unaffected
  aliveSteady <- isAlive dt rSteady
  assertBool aliveSteady
  ph <- readParamHash dt rSteady
  assertEqual (h 999999) ph
  (rAgain, freshAgain) <- lookupOrInsertRow dt (h 999999) 999999
  assertBool (not freshAgain)
  assertEqual rSteady rAgain

--
-- Property-based churn test (Part 2's "nothing grows unboundedly" ask):
-- a random sequence of writes and frees against a small population of
-- rows, drawing src-dep values from a pool large enough that most writes
-- mint genuinely fresh (key, version) pairs -- the same shape as the
-- version-churn scenario the refcounting fix targets, but randomized and
-- interleaved with frees instead of hand-written. Checked against a plain
-- 'Map.Map' model: every row the model considers alive must read back
-- exactly what the model recorded, and -- the actual boundedness
-- assertion -- once every row the model still considers alive is freed,
-- the interned src-dep table must be entirely empty, regardless of how
-- many operations it took to get there.
--

-- | One churn operation: overwrite row @i@'s (small, fixed population of
-- row keys 1..8) src-dep set, or free it. 'ChurnWrite'\'s values are
-- @(keyIdx, version)@ pairs drawn from a small key-index pool but a wide
-- version range, so repeated writes to the same row mint mostly-fresh
-- interned ids -- the scenario that would grow the pre-refcounting table
-- without bound.
data ChurnOp
  = ChurnWrite Int [(Int, Int)]
  | ChurnFree Int
  deriving (Show)

instance QC.Arbitrary ChurnOp where
  arbitrary =
    QC.frequency
      [ (3, ChurnWrite <$> QC.choose (1, 8) <*> (QC.choose (0, 3) >>= \n -> QC.vectorOf n genSrcVal))
      , (1, ChurnFree <$> QC.choose (1, 8))
      ]
   where
    genSrcVal = (,) <$> QC.choose (0, 3 :: Int) <*> QC.choose (1, 2000 :: Int)

churnEncode :: (Int, Int) -> AnyCompSrcDep
churnEncode (k, v) = srcDep (show k) v

-- | Interpret a churn sequence against a live 'DefTable', updating a plain
-- 'Map.Map' model (row key -> its current src-dep set) alongside it. A
-- 'ChurnFree' on a row the model doesn't currently consider alive
-- (never written, or already freed) is a no-op, matching 'freeRow''s own
-- contract of being meaningless to call on a hash that isn't indexed.
runChurn :: DefTable Int Int -> [ChurnOp] -> IO (Map.Map Int (HashSet AnyCompSrcDep))
runChurn dt = foldM step Map.empty
 where
  step model (ChurnWrite i vals) = do
    (r, _) <- lookupOrInsertRow dt (h i) i
    let s = HashSet.fromList (map churnEncode vals)
    writeSrcDeps dt r s
    pure (Map.insert i s model)
  step model (ChurnFree i) =
    case Map.lookup i model of
      Nothing -> pure model
      Just _ -> do
        (r, fresh) <- lookupOrInsertRow dt (h i) i
        if fresh
          then pure model -- defensive; shouldn't happen given the model agrees it's alive
          else do
            freeRow dt (h i) r
            pure (Map.delete i model)

prop_srcDepChurnStaysCorrectAndFullyDrains :: [ChurnOp] -> QC.Property
prop_srcDepChurnStaysCorrectAndFullyDrains ops = QC.ioProperty $ do
  dt <- newTest
  model <- runChurn dt ops
  correctness <- forM (Map.toList model) $ \(i, expected) -> do
    (r, fresh) <- lookupOrInsertRow dt (h i) i
    if fresh
      then pure False
      else do
        got <- readSrcDeps dt r
        pure (got == expected)
  forM_ (Map.toList model) $ \(i, _) -> do
    (r, fresh) <- lookupOrInsertRow dt (h i) i
    unless fresh $ freeRow dt (h i) r
  finalCount <- srcDepInternLiveCount dt
  pure $
    QC.counterexample
      ( "per-row correctness: "
          ++ show correctness
          ++ "; live ids remaining after draining every row the model thinks is alive: "
          ++ show finalCount
      )
      (and correctness && finalCount == 0)
