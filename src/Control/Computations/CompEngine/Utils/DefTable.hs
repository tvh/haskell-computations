{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | Per-definition columnar row storage. This haddock documents the
 *design*: what's here and the invariants it maintains.

 = Overview: a per-def struct-of-arrays

 An alternative representation would keep the engine's mutable state as a
 handful of boxed, persistent containers keyed by a dense interned 'Int' id
 -- a 'Data.HashMap.Strict.HashMap'-ish structure whose leaves are
 themselves boxed records, all traced by every major GC. This module
 instead uses one 'DefTable' /per definition/ (@'CompId'@, roughly "one
 @DefTable@ per distinct top-level computation in the program"), each
 holding its rows as a
 struct of parallel columns -- the classic "array of structs" -> "struct of
 arrays" flip: instead of @Vector Row@ where @Row@ bundles one definition's
 param, value, hashes, flags and edges together in one boxed record, it's
 @Row -> paramHash[row]@, @Row -> flags[row]@, @Row -> compDeps[row]@, etc,
 each its own flat column. A row is a plain array index (@'RowIdx'@) into
 every one of these columns at once; the columns are, in order of what they
 hold:

 * @param_hash@, @result_hash@ :: unboxed 'Hash128' (as two 'Word64's each).
 * @flags@ :: unboxed 'Flags' (alive \/ pending \/ result-state -- see
   "Flags" below).
 * @param@, @value@ :: the definition's actual typed param\/result, unboxed
   when the type allows it, boxed otherwise (see "Typed value columns"
   below).
 * @compDeps@, @rdeps@, @srcDeps@ :: not per-row columns at all, but indices
   (@offset@, @len@) into three arenas /shared across every row in the def/
   -- see "Edge storage" below.
 * a hash index (@param_hash -> row@) and a src-dep intern table, both
   per-def, both described in their own sections below.

 This is why the module is called @DefTable@ and not, say, @RowStore@: the
 def is the unit columns are allocated per, arenas are shared within, and
 indexes are scoped to.

 ASCII sketch of one @DefTable@ with 3 rows (row 1 dead, row 0 and 2 alive):

 >                row:        0         1         2
 >  param_hash   [Word64,Word64] [ ... ] [Word64,Word64]
 >  result_hash  [Word64,Word64] [ ... ] [Word64,Word64]
 >  flags        [ alive ]       [ dead ]  [ alive ]
 >  param        [  p0   ]       [  -    ]  [  p2   ]
 >  value        [  v0   ]       [  -    ]  [  v2   ]
 >  compDeps     off=6,len=2     off=?,len=0  off=0,len=2   --\
 >  rdeps        off=1,len=1     off=?,len=0  off=3,len=1   --+-> into the
 >  srcDeps      off=0,len=1     off=?,len=0  off=1,len=0   --/    three
 >                                                                 EdgeArenas
 >  dt_index (open-addressed, param_hash -> row): probe(hash) -> {0, 2}
 >  dt_srcDepIntern (AnyCompSrcDep <-> SrcDepId, refcounted): backs srcDeps

 Row 1's @param@\/@value@\/hash cells are drawn as "-" \/ "?" -- not zeroed,
 just garbage no longer reachable through any column read, because every
 read of a possibly-stale column is gated by a 'flagsAlive' check first (see
 "Row lifecycle and reuse" below).

 = Row identity

 A row is identified by a def index ('DefIdx' -- which definition it belongs
 to) and a row number within that definition's table ('RowIdx'). Callers
 outside this module pack the two into a single 'DefRef' via 'packRef'
 rather than passing a pair around, so the rest of the state layer (the
 stale-cap priority queue, the outputs side table, anything hashing\/
 comparing an identity) keeps working with one flat key type: a 'DefRef' is
 @(defIndex \<\< rowBits) .|. row@, wrapped in a newtype rather than handed
 out as a bare 'Int'.

 __Why 'DefRef' (and 'DefIdx'\/'RowIdx') are newtypes, not aliases.__ A def
 index, a row number, an interned id, and a packed ref are four different
 things that share one representation ('Int'). If they were all literally
 @Int@ to the type checker, a transposed argument (@packRef row defIdx@, or
 a def index handed to a function expecting a row) would compile silently
 and fail only at runtime, if at all. Each is instead its own 'newtype':
 'DefRef''s constructor isn't even exported, so the /only/ way to build or
 take one apart from outside this module is 'packRef'\/'unpackRef'\/
 'refDefIdx'\/'refRow' (plus the explicit 'unDefRef'\/'mkDefRefUnsafe'
 escape hatch for the two places -- "SimpleStateIf.hs"'s @IntMap@-keyed
 containers and "SrcIndex.hs"'s unboxed dependent column -- that must see
 the raw 'Int'). These newtypes cost nothing at runtime: a 'newtype' has no
 runtime representation of its own, so @'RowIdx' 5@ and @5 :: Int@ compile
 to the identical machine word -- the type distinction is erased after
 typechecking, not carried at runtime.

 = Columns

 Each column is a separate growable vector, unboxed where the element type
 allows it ('Data.Vector.Unboxed.Mutable' for @param_hash@/@result_hash@
 (as two 'Word64's each, since 'Hash128' itself has no 'Unbox' instance and
 splitting it avoids needing to write one) and @flags@), boxed where it
 can't be ('Data.Vector.Mutable' for the typed @param@/@value@ columns). A
 boxed column's freshly-grown capacity is left as the @vector@ package's own
 "uninitialised element" error thunk (see 'growBoxed') -- the same
 loud-failure-on-premature-read principle this module applies to row
 lifecycle (dead ids there, dead cells here), applied to row storage too.
 Growth for the @flags@ column (and, see below, the
 edge-arena @len@ columns) is the one exception: it is explicitly zeroed
 (not left as unboxed garbage), because a stray nonzero byte there would
 silently read as an occupied row or a nonempty edge span. @srcDeps@ is
 neither of the above -- see "Src-dep interning" below.

 = Edge storage: per-def CSR arenas, not per-row vectors

 Giving every row its own small unboxed 'VU.Vector' for @compDeps@/@rdeps@
 would be correct but expensive: each such vector is its own heap object (a
 boxed cell in a 'VM.Mutable' column holding a 'VU.Vector' wrapper plus its
 own 'Data.Array.Byte.ByteArray#'), so a fan-in-3 row would pay for several
 small, separately-traced nursery allocations to hold ~24-72 bytes of
 actual edge payload. This module instead gives each def two shared,
 growable, flat unboxed 'Word64' arenas (one for @compDeps@, one for
 @rdeps@) plus two unboxed per-row columns (@offset@ :: 'Int32', @len@ ::
 'Word32') indexing into them -- CSR (compressed sparse row), with the
 arena itself, not each row, as the unit of (de)allocation. A comp-dep edge
 is 3 words (target 'DefRef', observed-hash hi, observed-hash lo -- see
 "Preserved semantics" below for why the observed hash travels with the
 edge); an rdep edge is 1 word (target 'DefRef'). One large arena is the
 point of this exercise: unlike a swarm of
 small per-row vectors, a large flat array is a single object GHC's
 allocator places in the large-object area once it crosses the pinned/large
 threshold, where the copying collector never traces or copies it -- see the
 module's "Small defs" note below for the one case where this doesn't apply.

 ASCII sketch of a stride-3 @compDeps@ arena after row 0 has been
 overwritten once (its original span at words 0-2 is now dead, the current
 span lives at words 6-8) and row 2 has never been touched:

 >  word:        0    1    2    3    4    5    6    7    8
 >  content:   [tgt][hi ][lo ][tgt][hi ][lo ][tgt][hi ][lo ]
 >              \____row 1's span____/     \____row 0's, current____/
 >              (dead: row 0's old span was words 0-2, now orphaned)
 >
 >  per-row offset/len columns (in words, stride already applied):
 >    row 0: offset=6, len=1      row 1: offset=0, len=1      row 2: offset=0, len=0
 >
 >  ea_used = 9 (arena's append point)     ea_dead = 3 (row 0's orphaned span)

 A write never edits a span in place; it always appends at @ea_used@ and
 marks the row's *previous* span (if it had one) as dead by adding its
 length to @ea_dead@ -- see "Mutation strategy" just below. @rdeps@\/
 @srcDeps@ are the same picture at stride 1 (one word per edge, no hi\/lo).

 __'EdgeArena''s stride is a phantom type.__ 'EdgeArena' is not part of this
 module's public API, so every call to 'eaWrite'\/'eaRead'\/'eaCompact'\/
 'eaTakeRow' lives in exactly one place each, pairing one specific
 'EdgeArena' field with one hardcoded 'Stride' constant a few lines below
 its own definition -- a transposition there would show up as an
 immediate, glaring test failure, not a silent-corruption risk, so an
 argument from encapsulation alone would say a phantom tag isn't needed.
 What tips it the other way is that the phantom tag is not a net addition:
 @data EdgeArena (s :: 'StrideKind') = ...@ (a real 'DataKinds' phantom
 parameter -- 'data', not 'newtype', since 'EdgeArena' has five fields and
 'newtype' only wraps one; the phantom parameter is erased identically
 either way, see below) plus a 'KnownStride' class recovering the numeric
 'Stride' from the tag does not make
 'eaWrite'\/'eaRead'\/'eaCompact'\/'eaTakeRow'\/'maybeCompact' /gain/ a type
 parameter alongside their existing 'Stride' argument -- it makes the
 explicit 'Stride' argument disappear from all five entirely, recovered
 instead from @s@ via 'strideOf'. The net line count at each call site goes
 down, not up: 'readCompDeps' passes no 'compDepsStride' argument,
 'readRdeps' passes no 'singleStride' argument, and so on. A change that is
 a net simplification at every call site, not a net addition, is worth
 having even where the runtime risk it closes was already low. 'dt_compDeps'
 \/ 'dt_rdeps' \/ 'dt_srcDeps' are typed @'EdgeArena' ''CompDepsK'@ \/
 @'EdgeArena' ''RdepsK'@ \/ @'EdgeArena' ''SrcDepsK'@ respectively -- three
 tags, not two, even though @'RdepsK'@'s and @'SrcDepsK'@'s numeric strides
 are both 1: giving them separate tags closes an adjacent hazard a two-tag
 scheme would leave open (passing the reverse-comp-dep arena where the
 src-dep-id arena was meant, or vice versa -- same word-per-edge shape,
 unrelated meaning) for the cost of one more promoted constructor and one
 more one-line 'KnownStride' instance. 'strideOf' is marked 'INLINE' on
 every instance, so it inlines to the literal 'Stride' it returns at each
 (statically known) call site rather than a runtime dictionary lookup: GHC
 has no other choice once the instance is inlined and @s@ is known at
 compile time.

 __Mutation strategy: append-new-span, mark-old-span-dead, compact on a
 dead-fraction threshold.__ Chosen over "CSR with per-row slack" because
 every write here (@writeCompDeps@/@writeRdeps@, both called from
 "SimpleStateIf.hs") replaces a row's /entire/ edge set at once -- there is
 no in-place single-edge splice anywhere on the hot path for slack to help
 with, only whole-row replacement, so append-only is both simpler and no
 less dense. A write appends the new span at the arena's current end and
 bumps a per-def dead-word counter by the row's /previous/ span length (if
 any); nothing is physically overwritten in place, so a stale span is
 simply orphaned -- exactly like 'freeRow' already leaves a freed row's
 other columns as untouched garbage, gated by flags. Compaction exists
 because this table's whole point is bounding memory: repeated
 recomputation of the /same/ live rows -- exactly what this library's
 persistence benchmark's live-update phase does, tens of thousands of times
 over a graph of roughly a million rows -- would otherwise make the arena
 grow without bound, one orphaned span per rerun, forever.

 Compaction ('eaCompact') rebuilds a def's arena from scratch, walking every
 row 0..rowCount-1 and keeping only the /current/ span of every row that is
 currently alive; a row that isn't alive contributes nothing, whether or
 not it was ever given an explicit new write after going dead -- this is
 what lets a freed-but-not-yet-reused row's orphaned span get reclaimed
 too, not just rows some other write already knew were stale. Compaction is
 tied to the write path ('maybeCompact'), not to row-lifecycle GC
 ('freeRow'): freeing a row doesn't touch its edge columns at all (see
 "Row lifecycle and reuse" below), so the /existing/ garbage-tolerance
 design already guarantees a dead row's stale span is inert and safe to
 discard whenever compaction next runs, without 'freeRow' itself needing to
 know anything about edge arenas. 'maybeCompact' fires whenever, after
 an append, a def's dead-word count exceeds half its used length (and the
 arena is past a minimum size not worth an O(rowCount) scan for) --
 the standard amortized array-with-tombstones argument: a compaction
 immediately halves (at least) the dead fraction, so the arena's peak size
 is bounded to roughly 2x its live data, and the total compaction work
 across N writes is O(N) amortized, tied to the write path itself rather
 than a separately scheduled sweep.

 __Compaction is instrumented, unconditionally.__ 'eaCompact' walks every
 row of a def's arena (@rowCount@ words of scan, not @dead@ or @used@ words
 -- see 'eaCompact' itself), so its amortized-O(1)-per-write argument above
 is a statement about /how often/ it runs, not about what one run costs;
 whether that per-run cost is actually small next to a round's rerun count
 is an empirical question, not one this haddock can settle by itself. Each
 'EdgeArena' therefore carries a compaction counter, a rows-walked counter,
 and a nanosecond timer (@ea_compactions@\/@ea_rowsWalked@\/@ea_compactNs@),
 bumped inside 'eaCompact' itself on every call, unconditionally -- cheap to
 do unconditionally precisely because compaction is the rare side of
 'maybeCompact''s threshold, unlike per-write instrumentation, which would
 need to be opt-in. 'defArenaCompactionStats' reads them back per def per
 arena kind; only the resulting *report* (built by "SimpleStateIf.hs"'s
 @debugCompactionStats@, printed by "Run.hs" under @COMP_ENGINE_LOCK_STATS@)
 is opt-in.

 __Small defs.__ A def with few rows (or few edges per row) never
 accumulates enough words to reach GHC's large-object threshold (currently
 a few kB), so its arena is a small nursery-resident array like everything
 else -- this module does not special-case that, and it's fine: the whole
 argument for "large arrays escape the nursery" only pays off where it
 matters, on defs with many rows, which is also exactly where a per-row
 small-vector scheme (see "Edge storage" above) would pay the worst
 multiplicative overhead. A small def's arena stays small precisely because
 its total edge payload is small; there is nothing to reclaim by treating
 it differently.

 __Every comp-dep edge carries its own observed hash.__ Each edge is
 (target 'DefRef', observed hash hi, observed hash lo) -- not just the
 target. @test_modifcationWhileWorkingOnQueue@'s impure-cap detection
 (which compares a row's previously-recorded per-edge observed version
 against what it observes on the current run) depends on exactly this, per
 "SimpleStateIf.hs"'s module haddock: nothing here is dropped to save
 bytes, because the observed version travelling with each edge is
 load-bearing for that check.

 = Hash index: open addressing over the existing hash column

 A persistent 'Data.HashMap.Strict.HashMap' 'Hash128' 'Int' would be the
 obvious representation for a per-def index -- but it costs a HAMT leaf
 plus a boxed 'Hash128' key and boxed 'Int' value per entry, every one of
 them traced by every major GC, even though the key is pure duplication:
 every row's param hash already lives in the unboxed @param_hash@ column
 above. 'HashIndex' instead is a hand-rolled open-addressing table that
 stores only row ids ('Data.Int.Int32', @-1@ the empty-slot sentinel) in a
 flat 'Data.Vector.Unboxed.Mutable.IOVector' -- no key storage, no boxing,
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

 A boxed per-row column of persistent 'Data.HashSet.HashSet'
 'AnyCompSrcDep's would be correct, but sparse and expensive where
 populated: in this library's persistence benchmark, only the level-0 rows
 (~205k of 1M) have any source dependency at all, and each of those has
 exactly one, but 'AnyCompSrcDep' is an existential wrapper
 ('Control.Computations.CompEngine.CompFlow.ForAnyCompFlow' around a
 'CompSrcId' plus a typed key/version pair) -- a boxed, multi-word object in
 its own right, sitting inside a boxed 'HashSet' HAMT leaf, per row. Worse,
 real graphs (this benchmark included, at 300 distinct source keys against
 ~200k dependent rows) see the *same* @(key, version)@ pair observed by many
 rows at once, so deduplicating those values -- rather than giving every
 row its own separate heap copy of an equal value -- matters.

 This module interns 'AnyCompSrcDep' values to small 'Int' ids (a per-def
 'SrcDepIntern': forward @'HashMap' 'AnyCompSrcDep' 'Int'@, reverse growable
 boxed vector for the @Int -> AnyCompSrcDep@ direction, ids assigned
 monotonically) and stores a row's src-dep set as an interned-id CSR arena
 -- the exact same 'EdgeArena' machinery @compDeps@/@rdeps@ already use
 above (stride 1, ids narrow enough to fit the arena's 'Word64' words with
 room to spare), rather than inventing a third storage shape. A row's set
 is a handful of unboxed words indexing into a small, heavily-shared table
 instead of a private boxed structure duplicated per row.

 __Ids are refcounted and recycled.__ A design that never recycles ids is
 tempting -- it needs no refcounting at all, just a monotonically growing
 counter -- but it only stays cheap if an id's *value space* stays small
 relative to row count, and that assumption fails here: 'AnyCompSrcDep'
 carries an *observed version*, so a long-running, high-churn engine sees
 an unbounded stream of distinct @(key, version)@ pairs over its lifetime
 even though only a handful are ever *referenced* at once. That gap between
 "value space size" and "currently-live set size" is exactly what turns a
 never-recycled table into an actual leak, not just a theoretical one.
 Each id in @sdi_refcount@ (parallel to @sdi_reverse@, one 'Int' per
 assigned id) counts how many rows' arena spans currently reference it.
 'DefTable.writeSrcDeps' (a row's whole
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
 liveness (refcount > 0): recycling means "in range" alone can no longer
 imply "was ever meant to be read right now", so the explicit zero-refcount
 check is required, not just belt-and-suspenders.

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

 __The interning is invisible outside this module.__ 'readSrcDeps'\/
 'writeSrcDeps' have plain, fully-typed signatures
 (@'DefTable' p a -> 'RowIdx' -> 'IO' ('HashSet' 'AnyCompSrcDep')@ and the
 writing dual) -- intern/refcount/decode happens entirely inside them, so a
 caller never sees an interned id ('freeRow''s signature is a plain
 @'DefTable' p a -> 'Hash128' -> 'RowIdx' -> 'IO' ()@ for the same reason:
 the extra release work happens inside it, against columns it already
 owns). "SimpleStateIf.hs"'s own @sifs_srcIndex@ bookkeeping (which rows
 currently depend on which source key, and when a key becomes wholly
 unclaimed and should be reported as garbage\/unregistered) only ever sees
 fully-decoded 'AnyCompSrcDep' values through this API, so it needs no
 awareness that interning happens at all; dead-source-key reporting and
 'CompSrc.compSrcUnregister' triggering work purely off the decoded values
 it reads back.

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

 = Invariants (summary)

 Each section above argues its own invariant in prose; this is the
 checklist form, for a reader who wants "what must stay true" without
 re-deriving it from the design narrative. Every one of these is exercised
 by this module's own test section (the sole exception is called out
 below).

 __Row lifecycle (alive \/ free \/ reuse):__

 * A row's @flags@ (specifically 'flagsAlive') is the single gate every
   other column's read goes through; nothing outside 'lookupOrInsertRow'
   \/ 'freeRow' may assume a column holds meaningful data without checking
   it first.
 * 'freeRow' clears @flags@ (not-alive) and releases the row's @srcDeps@
   span (refcounted, can't wait -- see above); it deliberately leaves
   @param@\/@value@\/@result_hash@\/@compDeps@\/@rdeps@ as untouched garbage.
   This is intentional, not an oversight: 'lookupOrInsertRow' is what
   re-establishes a safe state for a /reused/ row number, not 'freeRow'.
 * A freed row's hash-index entry is removed by the caller before
   'freeRow' is called (not by 'freeRow' itself); the row *number* is
   recycled via the free list, a *fresh* hash-index entry is created for
   whatever hash next claims that number.

 __Edge-arena append\/compact (@compDeps@\/@rdeps@\/@srcDeps@):__

 * A write never mutates a span in place; it always appends the new span
   at @ea_used@ and marks the row's immediately-previous span (if any) as
   dead by adding its length to @ea_dead@.
 * Compaction ('eaCompact') only ever runs from the write path
   ('maybeCompact', once @ea_dead * 2 > ea_used@ past a minimum arena
   size), never from a separate sweep or from 'freeRow' -- a dead row's
   orphaned span is simply inert until the next compaction walks past it.
 * Compaction keeps exactly the *current* span of every row 'isRowAlive'
   reports true for at the moment it runs; everything else (dead rows,
   alive rows with no edges) is zeroed in the offset\/len columns.

 __Src-dep refcounting (@dt_srcDepIntern@, "Src-dep interning" above):__

 * An id is live iff its refcount is @> 0@; a refcount reaching zero
   deletes the forward-map entry, clears the reverse slot (dropping the
   boxed value), and frees the id for reuse -- all three happen together,
   never partially.
 * __Retain-before-release, always.__ 'writeSrcDeps' retains every id in
   the row's /new/ span before releasing any id in the row's /previous/
   span. This is not a micro-optimization: 'writeSrcDeps' runs
   unconditionally on every finish (changed or not), so an id present in
   both spans (an unchanged dependency) is the common case; releasing
   first would let such an id's refcount transiently touch zero and be
   reclaimed out from under the retain that was about to re-establish it.
   See @test_overlappingOverwriteNeverDropsTheSharedId@.
 * 'sdiResolve' checks both range (against @sdi_count@) /and/ liveness
   (refcount @> 0@) -- recycling means "in range" alone can no longer imply
   "was ever meant to be read right now".

 __Hash index (@dt_index@, "Hash index" above):__

 * A probe candidate is verified against the row's own @param_hash@
   column entry, never against a stored key -- there isn't one.
 * Deletion is backward-shift (Knuth), not tombstones: every occupied slot
   is a live entry, always: 'hix_count' and "number of occupied slots" are
   the same number at every point in time, not just after a rehash.

 __@KeyIntern@ (@Utils\/SrcIndex.hs@, not this module -- included here since
 it mirrors @dt_srcDepIntern@ closely enough that the difference is the
 point):__ needs no refcounting, unlike @dt_srcDepIntern@, because a source
 /key/ id has exactly one owner at a time (the single @SrcKeyArena@ created
 on first dependent, released on last) rather than many rows independently
 retaining it -- plain assign-on-first-use \/ recycle-on-last-removal is the
 correct match for 1:1 ownership, not a shortcut that happens to avoid
 refcounting -- see @Utils\/SrcIndex.hs@'s own module haddock for the full
 argument, including why /versions/ are the opposite case and are
 deliberately *not* interned.
-}
module Control.Computations.CompEngine.Utils.DefTable (
  -- * Row identity
  DefIdx (..),
  RowIdx (..),
  unRowIdx,
  DefRef,
  packRef,
  unpackRef,
  refDefIdx,
  refRow,
  unDefRef,
  mkDefRefUnsafe,
  unDefIdx,
  mkDefIdx,
  RowCount (..),
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
  dt_param,
  dt_value,
  dt_srcDepIntern,
  new,
  rowCount,
  columnIsUnboxed,

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
  CompDepEdge (..),
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

  -- * Edge-arena compaction stats (debug\/report-only -- see
  -- "SimpleStateIf.hs"'s @debugCompactionStats@ and "Run.hs"'s
  -- COMP_ENGINE_LOCK_STATS report)
  ArenaCompactionStats (..),
  defArenaCompactionStats,

  -- * Hash index (exposed for this module's own moved-out tests; see
  -- test\/.\/DefTableTest.hs)
  HashIndex,
  hix_table,
  hix_count,
  GetHash,
  newHashIndex,
  hixLookup,
  hixInsert,
  hixDelete,

  -- * Src-dep interning (exposed for this module's own moved-out tests)
  SrcDepIntern,
  sdi_count,
  newSrcDepIntern,
  mkSrcDepId,
  sdiIntern,
  sdiRetain,
  sdiRelease,
  sdiResolve,
)
where

----------------------------------------
-- LOCAL
----------------------------------------

import Control.Computations.CompEngine.CompSrc (AnyCompSrcDep)
import Control.Computations.Utils.Hash (Hash128 (..))

----------------------------------------
-- EXTERNAL
----------------------------------------

import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Data.Bits
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Hashable (Hashable (..))
import Data.IORef
import Data.Int (Int32)
import qualified Data.LargeHashable as LH
import Data.Primitive.Types (Prim)
import Data.Type.Equality ((:~:) (Refl))
import Data.Typeable (Typeable, eqT)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Primitive as VP
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word (Word32, Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)

--
-- Row identity: DefRef = packed (DefIdx, RowIdx). Three Int-shaped
-- concepts, each its own distinct type -- see the module haddock's "Row
-- identity" section for the full rationale (in short: a transposed
-- @packRef row defIdx@, or a def index handed to a function expecting a
-- row, would typecheck silently if all three were bare 'Int's).
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
  deriving newtype (Hashable, Prim)

-- | 'DefRef''s 'VU.Unbox' instance, via vector's 'VU.UnboxViaPrim' -- see
-- 'unDefRef''s haddock for why this instance exists at all (the claim it
-- corrects) and where it's actually used. The recipe: give the newtype a
-- 'Prim' instance for free via 'GeneralizedNewtypeDeriving' (the deriving
-- clause above -- 'Prim' has no associated data family, so, unlike
-- 'VU.Unbox' itself, GND can derive it directly), back the unboxed
-- 'VU.MVector'\/'VU.Vector' representations with 'Data.Vector.Primitive'
-- (which already knows how to store any 'Prim' instance), and connect the
-- two with @deriving via@. Verified against the actual installed
-- vector-0.13.2.0 API (not just the module this pattern was sketched
-- from) by compiling this exact recipe standalone before landing it here.
newtype instance VU.MVector s DefRef = MV_DefRef (VP.MVector s DefRef)
newtype instance VU.Vector DefRef = V_DefRef (VP.Vector DefRef)
deriving via (VU.UnboxViaPrim DefRef) instance GM.MVector VU.MVector DefRef
deriving via (VU.UnboxViaPrim DefRef) instance VG.Vector VU.Vector DefRef
instance VU.Unbox DefRef

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

-- | Escape hatch to the raw 'Int' a 'DefRef' packs. Exists for this
-- module's own 'EdgeArena'-packed @Word64@ triples (see
-- 'flattenCompDeps'\/'unflattenCompDeps') -- a bulk, stride-major payload
-- 'DefRef' genuinely doesn't fit (see 'flattenCompDeps''s own haddock), not
-- a place a typed column would help. Not needed, and not used, by
-- "SimpleStateIf.hs" -- every container it keys by row identity (the stale
-- queue, the outputs map, the pending-outputs map) is already generic in
-- its key type, so 'DefRef' flows through them unwrapped.
--
-- __Not needed by "Utils/SrcIndex.hs"'s @SrcKeyArena@ either.__ Its
-- @ska_refs@ column stores 'DefRef' directly rather than a raw 'Int'
-- coerced at the edge, because 'DefRef' has a real 'VU.Unbox' instance
-- (see the instance a few lines below). That instance doesn't need a
-- hand-written 'Data.Vector.Unboxed.Unbox' definition, even though
-- 'GeneralizedNewtypeDeriving' cannot derive 'VU.Unbox' directly (it \/is\/
-- a method-less class over associated /data/ families, so there is no
-- dictionary shape for GND to reuse): vector >= 0.13 (this project is on
-- 0.13.2.0) ships 'VU.UnboxViaPrim'\/'VU.As'\/'VU.IsoUnbox' for exactly
-- this gap, no Template Haskell required.
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
-- Row count / arena stride: two more Int-shaped concepts, each with its own
-- distinct type rather than sharing a row index's -- "how many rows does
-- this def currently have" (the bound an alive-scanning compaction walks)
-- is not "which row" and is not "how many words make up one edge in this
-- arena".
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

-- | Words per edge in an 'EdgeArena'. A plain newtype around the numeric
-- word count; see the module haddock's "'EdgeArena''s stride is a phantom
-- type" section for why 'eaWrite'\/'eaRead'\/'eaCompact'\/'eaTakeRow'\/
-- 'maybeCompact' don't take a 'Stride' as an explicit argument -- each
-- recovers it from 'EdgeArena''s own phantom 'StrideKind' tag via
-- 'KnownStride' instead.
newtype Stride = Stride Int
  deriving (Eq, Ord, Show)

-- | The tag 'EdgeArena''s phantom parameter ranges over -- one promoted
-- constructor per arena /identity/, not merely per distinct numeric stride:
-- 'RdepsK' and 'SrcDepsK' both carry stride 1, but keeping them separate
-- tags means @'EdgeArena' ''RdepsK'@ and @'EdgeArena' ''SrcDepsK'@ are
-- genuinely different types, so a future call site can't pass @dt_rdeps@
-- where @dt_srcDeps@ was meant (or vice versa) and have it typecheck just
-- because the word-per-edge count happens to match.
data StrideKind = CompDepsK | RdepsK | SrcDepsK

-- | Recover an 'EdgeArena''s numeric 'Stride' from its phantom 'StrideKind'
-- tag, so 'eaWrite'\/'eaRead'\/'eaCompact'\/'eaTakeRow'\/'maybeCompact' never
-- take one as an explicit argument -- see the module haddock. Every instance
-- is marked 'INLINE', so @'strideOf' \@''CompDepsK'@ (etc.) compiles down to
-- the literal 'Stride' at each call site, not a runtime dictionary lookup.
class KnownStride (s :: StrideKind) where
  strideOf :: Stride

-- | 'dt_compDeps''s stride: target 'DefRef', observed-hash hi, observed-hash
-- lo.
instance KnownStride 'CompDepsK where
  strideOf = Stride 3
  {-# INLINE strideOf #-}

-- | 'dt_rdeps''s stride: one word (a target 'DefRef') per edge.
instance KnownStride 'RdepsK where
  strideOf = Stride 1
  {-# INLINE strideOf #-}

-- | 'dt_srcDeps''s stride: one word (an interned src-dep id) per edge --
-- numerically the same as 'RdepsK', on a deliberately separate tag; see
-- 'StrideKind''s haddock.
instance KnownStride 'SrcDepsK where
  strideOf = Stride 1
  {-# INLINE strideOf #-}

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
-- one of the fixed set of primitive types this module recognizes
-- (Word32\/Word64\/Int\/Char\/Bool\/Double -- this library's own benchmark
-- def bodies are @Word32@ param \/ @Word64@ result, so this exercises the
-- unboxed path directly) and 'ColBoxed' for everything else
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
-- of its own (unlike a @Data.HashMap.Strict@, which forces its value on
-- every insert), so a caller that hands this an unforced thunk gets a
-- per-row retained closure invisible to every type signature and every
-- correctness test, showing up only as extra time and allocation. In
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
-- @VU.Vector 'CompDepEdge'@ \/ @VU.Vector Int@ happens at the DefTable
-- column-access boundary further down.
--

-- | A row's current liveness, as a callback rather than a direct
-- 'DefTable' reference -- this section's functions are deliberately
-- decoupled from 'DefTable' (see the module haddock), so they ask for "is
-- this row alive" the same indirect way 'HashIndex' below asks for "what is
-- this row's hash" (see 'GetHash'). What 'isAlive' partially applied to a
-- concrete 'DefTable' looks like from an 'EdgeArena''s side.
type IsRowAlive = RowIdx -> IO Bool

-- | @s@ is a phantom parameter (see the module haddock's "'EdgeArena''s
-- stride is a phantom type" section): it appears in none of the fields
-- below, only in this data declaration's head, so it costs nothing at
-- runtime -- a @'EdgeArena' ''CompDepsK'@ and an @'EdgeArena' ''RdepsK'@
-- have identical machine representation, the type index exists purely for
-- the typechecker. 'data', not 'newtype', because 'EdgeArena' has five
-- fields; a phantom parameter is erased the same way regardless of which
-- keyword introduces the type.
data EdgeArena (s :: StrideKind) = EdgeArena
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
  , ea_compactions :: !(IORef Int)
  -- ^ number of times 'eaCompact' has run against this arena, ever. Bumped
  -- unconditionally inside 'eaCompact' itself, not gated by
  -- @COMP_ENGINE_LOCK_STATS@ -- see 'eaCompact''s haddock for why that's
  -- cheap: compaction is already the rare side of 'maybeCompact''s
  -- threshold check, so a counter bump here costs nothing on the actual
  -- hot path (every write goes through 'eaWrite', not this).
  , ea_rowsWalked :: !(IORef Int)
  -- ^ total rows scanned across every compaction this arena has ever run.
  -- Each compaction walks every row @0..rowCount-1@ once regardless of how
  -- many turn out to be alive (see 'eaCompact'), so this -- not e.g. words
  -- copied, which varies with liveness -- is the size measure that
  -- actually bounds one compaction's cost.
  , ea_compactNs :: !(IORef Word64)
  -- ^ total wall time spent inside 'eaCompact' for this arena, in
  -- nanoseconds via 'GHC.Clock.getMonotonicTimeNSec'.
  }

newEdgeArena :: IO (EdgeArena s)
newEdgeArena =
  EdgeArena
    <$> (newIORef =<< VUM.new 0)
    <*> (newIORef =<< VUM.new 0)
    <*> (newIORef =<< VUM.new 0)
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef 0

-- | Grow an edge arena's per-row @offset@/@len@ columns to at least
-- @needed@ rows. Does not touch the shared word arena itself -- that grows
-- independently, on append, in 'eaWrite'. No 'KnownStride' constraint --
-- unlike every function below, this one never needs the numeric stride.
eaGrowRows :: EdgeArena s -> RowCount -> IO ()
eaGrowRows ea needed = do
  growUnboxed (ea_off ea) (unRowCount needed)
  growUnboxedZeroed (ea_len ea) (unRowCount needed)

-- | Only compact once an arena has reached this many words -- below this,
-- the O(rowCount) scan a compaction costs isn't worth it.
compactMinWords :: Int
compactMinWords = 4096

-- | Compact iff the def's dead words are more than half of its used
-- words (and the arena is large enough to bother) -- see the module
-- haddock's amortized-cost argument. The 'Stride' this needs to pass on to
-- 'eaCompact' is recovered from @s@ via 'KnownStride', not taken as an
-- argument -- see the module haddock.
maybeCompact :: forall s. KnownStride s => EdgeArena s -> IsRowAlive -> RowCount -> IO ()
maybeCompact ea isRowAlive rows = do
  used <- readIORef (ea_used ea)
  dead <- readIORef (ea_dead ea)
  when (used >= compactMinWords && dead * 2 > used) $
    eaCompact ea isRowAlive rows
{-# INLINE maybeCompact #-}

-- | Rebuild the arena keeping only the current span of every currently-
-- alive row (in row order); every other row (dead, or alive with no
-- edges) has its offset/len columns zeroed. A dead row contributes nothing
-- regardless of whether it was ever given an explicit new write after
-- going dead -- see the module haddock for why that's the key property
-- that makes tying this to the write path (rather than row-free) correct.
--
-- NOTE: instrumentation below (t0/t1 and the three counter bumps at the
-- end) is unconditional, not gated by COMP_ENGINE_LOCK_STATS -- see
-- 'ea_compactions''s haddock for why that's safe: compaction is already
-- the rare branch of 'maybeCompact', so a timer pair and three IORef bumps
-- here are invisible next to the O(rowCount) scan they wrap, unlike the
-- per-write instrumentation "Run.hs" deliberately keeps opt-in. Only the
-- *reporting* of these counters (SimpleStateIf.hs's @debugCompactionStats@,
-- printed from Run.hs) is gated.
eaCompact :: forall s. KnownStride s => EdgeArena s -> IsRowAlive -> RowCount -> IO ()
eaCompact ea isRowAlive rows = do
  t0 <- getMonotonicTimeNSec
  let Stride stride = strideOf @s
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
  t1 <- getMonotonicTimeNSec
  modifyIORef' (ea_compactions ea) (+ 1)
  modifyIORef' (ea_rowsWalked ea) (+ unRowCount rows)
  modifyIORef' (ea_compactNs ea) (+ (t1 - t0))
{-# INLINE eaCompact #-}

-- | One arena's lifetime compaction stats, as read back by
-- 'defArenaCompactionStats'. A named record, not a bare triple: this
-- module already argues (see 'CompDepEdge''s haddock) that a tuple of
-- same-shaped positional fields invites exactly the kind of transposition
-- ("rowsWalked where compactions was meant") that naming the fields closes
-- off.
data ArenaCompactionStats = ArenaCompactionStats
  { acs_compactions :: !Int
  -- ^ see 'ea_compactions'
  , acs_rowsWalked :: !Int
  -- ^ see 'ea_rowsWalked'
  , acs_totalNs :: !Word64
  -- ^ see 'ea_compactNs'
  }
  deriving (Eq, Show)

-- | Snapshot one arena's compaction counters (see 'ArenaCompactionStats').
-- Debug/report-only, same status as 'srcDepInternLiveCount' -- never read
-- on the engine's hot path.
eaCompactStats :: EdgeArena s -> IO ArenaCompactionStats
eaCompactStats ea =
  ArenaCompactionStats
    <$> readIORef (ea_compactions ea)
    <*> readIORef (ea_rowsWalked ea)
    <*> readIORef (ea_compactNs ea)

-- | Read a row's edge span as a flat, stride-major 'VU.Vector Word64' (a
-- safe copy -- the arena keeps mutating after this call returns). Empty
-- for a row with no edges.
eaRead :: forall s. KnownStride s => EdgeArena s -> RowIdx -> IO (VU.Vector Word64)
eaRead ea row = do
  let Stride stride = strideOf @s
  lenV <- readIORef (ea_len ea)
  len <- VUM.read lenV (unRowIdx row)
  if len == 0
    then pure VU.empty
    else do
      offV <- readIORef (ea_off ea)
      off <- VUM.read offV (unRowIdx row)
      dataV <- readIORef (ea_data ea)
      VU.freeze (VUM.slice (fromIntegral off) (fromIntegral len * stride) dataV)
{-# INLINE eaRead #-}

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
eaTakeRow :: forall s. KnownStride s => EdgeArena s -> RowIdx -> IO (VU.Vector Word64)
eaTakeRow ea row = do
  flat <- eaRead ea row
  let Stride strideN = strideOf @s
  lenV <- readIORef (ea_len ea)
  offV <- readIORef (ea_off ea)
  len <- VUM.read lenV (unRowIdx row)
  when (len > 0) $ do
    modifyIORef' (ea_dead ea) (+ (fromIntegral len * strideN))
    VUM.write offV (unRowIdx row) 0
    VUM.write lenV (unRowIdx row) 0
  pure flat
{-# INLINE eaTakeRow #-}

-- | Overwrite a row's entire edge span with @flat@ (already stride-major
-- flattened; its length must be a multiple of @stride@). An empty write
-- just zeroes the row's offset/len -- clearing a row's edges never
-- appends to (or grows) the arena. Otherwise appends a fresh span at the
-- arena's current end, marks the row's previous span (if any) as dead
-- weight, then lets 'maybeCompact' decide whether the def's dead fraction
-- now warrants reclaiming it.
eaWrite :: forall s. KnownStride s => EdgeArena s -> RowIdx -> IsRowAlive -> RowCount -> VU.Vector Word64 -> IO ()
eaWrite ea row isRowAlive rows flat = do
  let Stride strideN = strideOf @s
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
  maybeCompact ea isRowAlive rows
{-# INLINE eaWrite #-}

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
-- gap that recycling opens up, see the module haddock). A zero refcount is
-- exactly the liveness signal a range check can't provide, so it's checked
-- explicitly here rather than inferred from the range.
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
  , dt_compDeps :: !(EdgeArena 'CompDepsK)
  -- ^ flat forward comp-dep edges, stride 3: (packed 'DefRef' this row
  -- depends on, the target's result hash *as observed* when this row last
  -- ran, split hi/lo) -- needed for "impure cap" detection, which must
  -- distinguish "I depend on the same target at
  -- the same version as before, yet produced a different result"
  -- (genuinely impure) from "I depend on the same target but at a newly-
  -- changed version" (an ordinary, expected recompute) -- a target-set
  -- comparison alone can't tell those apart. A target with no result at
  -- observation time (a failed dependency) is encoded as the sentinel
  -- @(maxBound, maxBound)@; colliding with a real MD5-derived hash is not
  -- a realistic concern. See the module haddock's "Edge storage" section
  -- for the CSR-arena layout this lives in.
  , dt_rdeps :: !(EdgeArena 'RdepsK)
  -- ^ flat reverse comp-dep edges (packed 'DefRef's depending on this
  -- row), stride 1.
  , dt_srcDeps :: !(EdgeArena 'SrcDepsK)
  -- ^ flat interned src-dep ids, stride 1 -- see the module haddock's
  -- "Src-dep interning" section.
  , dt_srcDepIntern :: !SrcDepIntern
  -- ^ this def's own @AnyCompSrcDep <-> Int@ table backing 'dt_srcDeps'.
  , dt_index :: !HashIndex
  -- ^ this def's own param-hash -> row index. Open addressing over row ids
  -- only -- no stored key -- see the module haddock's "Hash index" section;
  -- probes verify against the row's own @param_hash@ column entry.
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
  oldSrc <- eaTakeRow (dt_srcDeps dt) row
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

-- | One comp-dep edge: 'cdeTarget' is a packed 'DefRef' this row depends on
-- (kept as a raw 'Int', not a 'DefRef' -- see 'flattenCompDeps''s note
-- below), 'cdeObservedVer' is the target's result hash /as observed/ when
-- this row last ran, as the same opaque @(hi, lo)@ pair 'hashToPair'\/
-- 'pairToHash' already use. A named record rather than a bare
-- @(Int, Word64, Word64)@ tuple, specifically because that tuple's three
-- positionally-interchangeable slots would let a hi\/lo swap at a
-- construction or destructuring site compile silently. Keeping
-- 'cdeObservedVer' as one opaque pair field (not two named
-- @cdeHashHi@\/@cdeHashLo@ fields) closes the same hazard one level down:
-- splitting it into two named fields would force every call site to
-- reassemble @(t, hi, lo)@ from 'encodeVer''s result, or destructure
-- @(t, hi, lo) <- ...@ before calling 'decodeVer' -- exactly the point at
-- which a hi\/lo transposition would compile silently. 'encodeVer'\/
-- 'decodeVer' ("SimpleStateIf.hs") instead pass the pair through opaque,
-- never taking it apart: nothing outside
-- 'hashToPair'\/'pairToHash'\/'encodeVer'\/'decodeVer' ever needs to know or
-- reassemble which half is which.
data CompDepEdge = CompDepEdge
  { cdeTarget :: !Int
  , cdeObservedVer :: !(Word64, Word64)
  }
  deriving (Eq, Show)

-- | 'CompDepEdge''s 'VU.Unbox' instance, via vector's 'VU.IsoUnbox'\/'VU.As'
-- rather than 'VU.UnboxViaPrim' ("DefTable.hs"'s 'DefRef' instance, or
-- "Utils/SrcIndex.hs"'s haddock pointing back to it, for that recipe):
-- unlike a single-field newtype over a 'Prim' scalar, 'CompDepEdge' is a
-- genuine two-field product with no 'Data.Primitive.Types.Prim' instance of
-- its own to borrow -- but it \/is\/ isomorphic to the
-- @(Int, Word64, Word64)@ tuple that already has 'VU.Unbox' (vector
-- provides 'VU.Unbox' for tuples of 'VU.Unbox' elements), so 'VU.IsoUnbox'
-- (an explicit @toURepr@\/@fromURepr@ isomorphism to an /existing/ 'VU.Unbox'
-- type, as opposed to 'VU.UnboxViaPrim''s "borrow a 'Prim' instance") is the
-- right member of the same vector>=0.13 family for this shape.
instance VU.IsoUnbox CompDepEdge (Int, Word64, Word64) where
  toURepr (CompDepEdge t (hi, lo)) = (t, hi, lo)
  fromURepr (t, hi, lo) = CompDepEdge t (hi, lo)
  {-# INLINE toURepr #-}
  {-# INLINE fromURepr #-}

newtype instance VU.MVector s CompDepEdge = MV_CompDepEdge (VU.MVector s (Int, Word64, Word64))
newtype instance VU.Vector CompDepEdge = V_CompDepEdge (VU.Vector (Int, Word64, Word64))
deriving via
  (VU.As CompDepEdge (Int, Word64, Word64))
  instance
    GM.MVector VU.MVector CompDepEdge
deriving via
  (VU.As CompDepEdge (Int, Word64, Word64))
  instance
    VG.Vector VU.Vector CompDepEdge
instance VU.Unbox CompDepEdge

-- | Flatten a comp-dep edge set into stride-3 words (target, hash-hi,
-- hash-lo) for 'eaWrite'. The target component is a packed 'DefRef' stored
-- as a raw 'Word64' -- like the rest of this section, a bulk unboxed
-- payload deliberately left in its raw machine representation (see
-- 'unDefRef''s haddock: an 'EdgeArena''s shared @Word64@ arena is a
-- different shape from 'CompDepEdge' above -- stride-major, pushed through
-- 'IntSet'\/'IntMap' algebra in "SimpleStateIf.hs", with a ref packed
-- alongside two hash words rather than each edge as its own addressable
-- element -- 'DefRef' doesn't fit that layout and there is no positional
-- hazard for a named type to close there, unlike 'CompDepEdge' above, which
-- /is/ addressed element-wise). 'DefTable'\'s own row-identity API
-- ('packRef'\/'unpackRef'\/'refDefIdx'\/'refRow') is the actual 'DefRef'
-- boundary; callers reconstruct a typed 'DefRef' from a target 'Int' via
-- 'mkDefRefUnsafe' only where they cross back into scalar, single-ref logic
-- (e.g. "SimpleStateIf.hs"'s @resolveRefToAny@).
flattenCompDeps :: VU.Vector CompDepEdge -> VU.Vector Word64
flattenCompDeps xs = VU.generate (3 * VU.length xs) go
 where
  go i = case i `divMod` 3 of
    (n, 0) -> fromIntegral (cdeTarget (xs VU.! n))
    (n, 1) -> fst (cdeObservedVer (xs VU.! n))
    (n, _) -> snd (cdeObservedVer (xs VU.! n))

-- | Inverse of 'flattenCompDeps'.
unflattenCompDeps :: VU.Vector Word64 -> VU.Vector CompDepEdge
unflattenCompDeps flat = VU.generate (VU.length flat `div` 3) go
 where
  go n =
    CompDepEdge
      (fromIntegral (flat VU.! (3 * n)))
      (flat VU.! (3 * n + 1), flat VU.! (3 * n + 2))

readCompDeps :: DefTable p a -> RowIdx -> IO (VU.Vector CompDepEdge)
readCompDeps dt row = unflattenCompDeps <$> eaRead (dt_compDeps dt) row

writeCompDeps :: DefTable p a -> RowIdx -> VU.Vector CompDepEdge -> IO ()
writeCompDeps dt row xs = do
  n <- rowCount dt
  eaWrite (dt_compDeps dt) row (isAlive dt) n (flattenCompDeps xs)

-- | Just the target refs (raw, packed 'DefRef' 'Int's -- see
-- 'flattenCompDeps''s haddock) of a comp-dep edge set, discarding the
-- observed version -- what the rdeps graph (add/remove edges, GC liveness)
-- cares about.
compDepTargets :: VU.Vector CompDepEdge -> VU.Vector Int
compDepTargets = VU.map cdeTarget

readRdeps :: DefTable p a -> RowIdx -> IO (VU.Vector Int)
readRdeps dt row = VU.map fromIntegral <$> eaRead (dt_rdeps dt) row

writeRdeps :: DefTable p a -> RowIdx -> VU.Vector Int -> IO ()
writeRdeps dt row xs = do
  n <- rowCount dt
  eaWrite (dt_rdeps dt) row (isAlive dt) n (VU.map fromIntegral xs)

-- | Decode a row's interned src-dep arena span back into a 'HashSet' of
-- full 'AnyCompSrcDep' values -- see the module haddock's "Src-dep
-- interning" section. Callers outside this module never see an interned
-- id.
readSrcDeps :: DefTable p a -> RowIdx -> IO (HashSet AnyCompSrcDep)
readSrcDeps dt row = do
  flat <- eaRead (dt_srcDeps dt) row
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
  oldFlat <- eaRead (dt_srcDeps dt) row
  newIds <- mapM (sdiIntern sdi) (HashSet.toList s)
  mapM_ (sdiRetain sdi) newIds
  mapM_ (sdiRelease sdi . mkSrcDepId . fromIntegral) (VU.toList oldFlat)
  n <- rowCount dt
  eaWrite (dt_srcDeps dt) row (isAlive dt) n (VU.fromList (map (fromIntegral . unSrcDepId) newIds))

-- | Test/debug-only: number of currently-live interned src-dep ids in this
-- def's table -- see 'sdiLiveCount'. Exported (unlike the rest of
-- 'SrcDepIntern''s internals) purely so the engine-level churn regression
-- test (Tests/TestStateIf.hs, via a small debug accessor in
-- "SimpleStateIf.hs") can assert the intern table stays bounded through
-- "SimpleStateIf.hs"'s real read/write API, not just this module's own
-- unit tests.
srcDepInternLiveCount :: DefTable p a -> IO Int
srcDepInternLiveCount = sdiLiveCount . dt_srcDepIntern

-- | This def's compaction stats for each of its three edge arenas, tagged
-- with the arena kind's name. Debug/report-only, mirroring
-- 'srcDepInternLiveCount''s "one per-def accessor, summed/tabulated by the
-- caller" shape -- exists purely to feed "SimpleStateIf.hs"'s
-- @debugCompactionStats@ (which adds the owning def's name) and, from
-- there, "Run.hs"'s @COMP_ENGINE_LOCK_STATS@ close-time report.
defArenaCompactionStats :: DefTable p a -> IO [(String, ArenaCompactionStats)]
defArenaCompactionStats dt = do
  cd <- eaCompactStats (dt_compDeps dt)
  rd <- eaCompactStats (dt_rdeps dt)
  sd <- eaCompactStats (dt_srcDeps dt)
  pure [("compDeps", cd), ("rdeps", rd), ("srcDeps", sd)]

