# The reference engine at 1M caps — benchmark notes (Haskell)

> Working notes, not polished prose. Companion to
> `rust-computations/docs/persistence-benchmark-notes.md`, which benchmarks the
> Rust port of this engine through five optimization stages and *estimated* this
> reference implementation's numbers on paper (its "Interlude" sections). This
> doc holds the real, measured numbers. Same machine: Apple-silicon Mac
> (Darwin 25.5). Reproduce with:
> `stack exec -- incremental-computations-exe bench +RTS -T -N`
> (`PERSIST_BENCH_SCALE=0.1` for the ~100k variant; the benchmark self-reports
> wall times, rerun counts, RSS, and GHC RTS stats.)

## System under test

- This repository as-is: the reference implementation of the coarse-grained
  self-adjusting computation engine from Wehr's FUNARCH '23 paper. `fullCaching`
  cache behavior, `SimpleStateIf` state (five persistent containers in a
  `TVar`), stock driver loop. **No persistence exists and none is simulated** —
  so the comparable Rust figures are its *engine-only* numbers (its phases 5
  and 7), never the with-persistence ones.
- Benchmark graph: identical to the Rust `persist_bench` topology, formula for
  formula — 50 definitions × 10 levels, fixed fan-in 3 via the same
  modular-arithmetic edge formulas over `Word32` params, 300 in-memory KV
  inputs (`HashMapFlow` as source), 1000 top-level sink outputs (a second
  `HashMapFlow` as sink), front-loaded level sizes
  `[205000 ×5, 128945, 101885, 61090, 27060, 1000]` → **999,760 instances**
  achieved at scale 1.0 (bit-identical to the Rust achieved count — the
  strongest available check that the two benchmarks build the same graph).
  Bodies are trivial `Word64` additions — deliberately, so the numbers measure
  the *engine overhead floor*, not body cost.
- Benchmark code: `app/Control/Computations/Demos/Bench/Main.hs` (`bench`
  subcommand of the demo executable). Two phases, one process:
  1. **cold eval** — engine start to initial evaluation settled;
  2. **live incremental** — mutate exactly one source key (`"0"` →
     `"13371337"`) on the still-running engine, time until the propagation
     round *fully* completes.

## Measurement methodology (differs from Rust in two places worth knowing)

- **Rerun counting is done at the engine's interface boundary, not inside
  bodies.** `CompM` has no `liftIO`, and an `unsafePerformIO` counter inside a
  "pure" body is unsound (GHC may float/share/eliminate it — the first version
  of this benchmark hit exactly that). Instead `countingStateIf` wraps the
  `CompEngineStateIf` record so `capEvaluationStarted` — which `Impl` calls
  exactly once immediately before each real body invocation — bumps an
  `IORef` before delegating. Same event the Rust benchmark's
  `run_counter.fetch_add` observes, observed from outside.
- **Settle detection must survive time-sliced rounds.** The driver processes at
  most `rcif_maxLoopRunTime` (10 s) of stale caps per loop iteration, so at
  scale 1.0 a single mutation's ~80k-rerun cascade spans multiple `RunStats`
  "runs", each carrying leftover `rs_staleCaps` into the next. A naive
  wait-for-next-run reported 10.0 s / 34,356 reruns — a partial round.
  `waitForFullSettle` polls run by run until a run reports an *incoming*
  backlog of zero. (The Rust benchmark's per-round tracing signal has no
  equivalent slicing, so it never needed this.)
- RSS via `ps -o rss=` (same as Rust's `rss_mb`); live heap via `GHC.Stats`
  (`+RTS -T`): `max_live_bytes` is the GC-measured peak live set,
  `max_mem_in_use_bytes` what the RTS actually took from the OS.

## Stage 0 — the reference engine, unmodified (commits `7da35b2`, `46f68b7`, `5e4ab4c`)

| Scale | Instances | Cold eval | RSS after settle | Live reruns | Live update |
|---|---|---|---|---|---|
| 0.02 | 20,254 | 0.48–0.50 s | 149 MB | 1,677 | 47–50 ms |
| 0.1 | 98,982 | 2.56–2.85 s | 448 MB | 8,248 | 0.41–0.45 s |
| 1.0 (run A) | **999,760** | 42.80 s | 2,571 MB | **80,767** | 24.40 s |
| 1.0 (run B) | **999,760** | 42.98 s | 1,783 MB | **80,767** | 31.44 s |

At 1.0, both runs: `max_live_bytes` **1,501.3 MB** (bit-identical across runs),
`max_mem_in_use_bytes` ~3,592 MB, 18,516 GCs. Per-instance: **~1,502 B/cap
live** (`max_live_bytes` basis, the stable figure), ~1,783–2,571 B/cap on the
noisier RSS basis. Per-node timing: cold ~43 µs/instance; live ~300–390
µs/rerun.

### Findings

- **Interlude 1's paper estimate holds up.** The Rust notes statically tallied
  this engine's data types at ~1,350 B/cap (±25%) → ~1.35 GB live at 1M.
  Measured: 1,501 B/cap — **+11%, well inside the stated uncertainty**. Its RSS
  prediction ("~2.7–4 GB for a 1.35 GB live set") matches
  `max_mem_in_use_bytes` (~3.6 GB); `ps` RSS came in below that (1.8–2.6 GB),
  presumably address space the RTS holds but the OS hasn't resident-charged.
- **Topology fidelity is exact.** 999,760 cold instances and 80,767
  live-update reruns are the *same numbers* the Rust benchmark reports at this
  scale — the modular fan-in formulas, `Word32` wrapping and all, reproduce the
  identical graph and identical dirty set.
- **Where this lands vs. the Rust port** (its Stage 5 / Tier 2 engine-only
  figures, same graph):
  - live heap ~4.6× (1,502 vs ~330 B/node);
  - cold eval ~14× (43 s vs ~2.9–3.0 s);
  - live incremental ~50–60× (24–31 s vs ~0.5 s).
  The memory ratio is exactly the story the Rust notes' interludes told; the
  *time* ratios are new information — the interludes never estimated timing.
  The live-update gap (µs/rerun: ~300–390 vs ~6) is the most dramatic and the
  least explained; candidate suspects (unmeasured): `Data.Map`/HAMT traversal
  constants across the five containers per touched cap, `Ord AnyCompAp`'s
  `eqT`-per-comparison, `show`-rendering of every recomputed result into
  `ccm_logrepr` (`fullCaching`), and GC pressure from churn allocation
  (`mkCompAp` allocates fresh `CompAp`/`AnyCompAp` per parent eval call).
- **Run-to-run**: rerun counts and `max_live_bytes` are deterministic
  (bit-identical). Wall times varied <0.5% cold but ~29% on the live update,
  and RSS-after-settle ~30% — stop-the-world-GC-sensitive measurements on
  shared laptop hardware. More trials would tighten this.

## Caveats

- Bodies are trivial additions: this is the engine floor, same caveat as the
  Rust doc. Real workloads shift every comparison toward "body cost dominates".
- ~8% of the graph re-running for 1 changed input is the fan-in-3 topology,
  not overhead.
- Two runs at 1.0 is thin for the noisy measurements (live wall time, RSS).
- Single process, cold-then-live: RSS-after-settle is measured *after* the
  cold eval in the same process the live update then reuses — comparable to
  Rust's phase-7 setup, but not to its isolated process-per-phase RSS figures
  except phase 7's own.

## Runtime research — where the time goes (reasoning + one probe, before touching code)

Question: can the 14× cold / 50–60× live-update gap vs. the Rust port be
closed without abandoning the architecture? Findings, in order of confidence:

### Build/RTS configuration was leaving easy money on the table

- The build is stack's default `-O1`; `-O2` is set nowhere.
- The executable hardcodes `-with-rtsopts=-N`: every benchmark number above
  ran with all-cores parallel GC and the default 4 MB nursery — hence the
  18,516 GCs at scale 1.0. Probed at scale 0.1 (same build, `5e4ab4c`):

| RTS flags | cold eval | live update | GCs | RSS after settle |
|---|---|---|---|---|
| default (`-A4m -N`) | 2.48 s | 0.367 s | 1,043 | 447 MB |
| `-A16m` | 2.14 s | 0.359 s | 24 | 560 MB |
| `-A64m` | **1.65 s** | **0.306 s** | **4** | 1,122 MB |
| `-N1` | — | — | — | crashes |

  **−33% cold / −17% live from a nursery flag alone**, at a real memory
  price (nurseries are per-capability under `-N`; `-n` chunking or a small
  fixed `-N` would moderate it). The `-N1` crash is a genuine finding:
  "thread blocked indefinitely in an STM transaction" right after cold
  settle — the driver deadlocks on a single capability.

### The monad is not naively slow — the classic fixes are already in

`CompM` is a Haxl-shaped resumption monad: `CompFinished`/`CompSuspended`
mirror Haxl's `Done`/`Blocked` ("There is no Fork", ICFP'14), including the
applicative `CompReqCombined` batching; and `ContCompM` (`Types.hs`) already
implements the Jaskelioff/Rivas "smart view" — the same fix family as
"Reflection without Remorse" and the FTCQueue in "Freer Monads, More
Extensible Effects". The quadratic left-bind pathology that literature
addresses is already solved here; switching encodings (freer, Church/
Codensity à la "Free Monads for Less") would shave constants, not close a
50× gap.

### What actually looks structurally slow (unmeasured — profile first)

1. **A `HashSet` union per bind.** `compMBind`/`compMAp` thread the `DepSet`
   as a writer monoid (`w <> w'`, `Types.hs`) — every bind in every rerun
   allocates and unions. Haxl's lesson applies: `GenHaxl` is a monad *over
   IO* with mutable state; deps could accumulate into a per-cap mutable ref
   (the state-if is already `IO`), deleting per-bind unions outright.
2. **Every child eval is a suspend→engine→resume round trip, even on a
   cache hit**, and the cache lookup is `Data.Map` keyed by `AnyCompAp`
   whose `Ord` pays an `eqT` fingerprint comparison per step — ~3M edges ×
   ~20 comparisons at scale 1.0. Interning (Interlude 2's big item) is a
   *time* win too, not just memory.
3. **MD5 per `mkCompAp`** — every parent eval call MD5-hashes
   `(name, param)` via large-hashable; ~3M+ hashes on a cold eval. A
   non-crypto 128-bit hash (e.g. xxh3-128) is ~10× faster on small inputs;
   the Rust port uses truncated blake3.
4. **`fullCaching` `show`s and MD5s every recomputed result**
   (`CacheBehaviors.hs`) — the `ccm_logrepr` memory item is also a
   per-rerun time cost.

### If a restructure is ever warranted

The modern consensus (effectful/bluefin benchmarks) is that free-monad-family
interpreters lose 1–3 orders of magnitude to evidence-passing/IO-based
designs. The relevant move here is not a better free monad but **CompM over
IO with real suspension only on cache miss**: GHC ≥ 9.6 ships delimited
continuation primops (`prompt#`/`control0#`, GHC proposal 313 — the
machinery behind Alexis King's eff), and this repo is on GHC 9.10.3. Cache
hits become direct mutable-map reads; a continuation is captured only when
the engine genuinely needs to intervene. Biggest lever, deepest cut — gate
it on profiling evidence.

### Recommended order

1. Profile (`-fprof-late`) to split time across suspects 1–4.
2. `-O2` + RTS tuning (free, ~30%+ demonstrated).
3. MD5 → xxh3; drop the per-rerun `show`.
4. Interning + dense lookup (shared with the memory diet).
5. Only then judge the IO-based `CompM` rewrite.

## Stage 0.5 — -O2, -N2 default, and a profile (commit `ec706f4`)

Build changes: `-O2` added to the global ghc-options; the executable's
baked-in rtsopts changed `-N` → `-N2`. Measurements at scale 0.1.

### -O2 is a memory win and a small *time loss*

| Config | cold | live | GCs | RSS after settle | max_live |
|---|---|---|---|---|---|
| -O1, `-A4m -N` | 2.48–2.85 s | 0.37–0.45 s | 1,043 | 447 MB | 166 MB |
| -O1, `-A64m -N` | 1.65 s | 0.306 s | 4 | 1,122 MB | — |
| -O2, `-N2` | 2.60–2.89 s | 0.45–0.47 s | 1,040 | **309 MB** | 188 MB |
| -O2, `-N2 -A64m` | 2.01–2.05 s | 0.33–0.36 s | 33 | **446–453 MB** | 166 MB |
| -O2, `-N` | 2.63–2.66 s | 0.45 s | 1,040 | 371 MB | 188 MB |

Contrary to the expectation above, `-O2` cost +7–10% cold / +20–25% live at
matched RTS settings, while cutting RSS-after-settle 17–31% (and −60% under
`-A64m`). `-N2` vs `-N` (14 cores) barely moves anything — allocation is
effectively single-HEC here. `-A64m` remains the single most reliable
timing lever. Keeping `-O2` for the memory win; the timing regression is
within what the interning work below should dwarf.

### Profile (`-fprof-late`, scale 0.1) — the suspects, measured

Profiled binary: cold 11.4 s (≈4.5× slower — relative attribution only).
Bucket attribution of measured time:

| Suspect (from the section above) | %time | %alloc |
|---|---|---|
| (b) cache lookup/dispatch: `$mCompAp` pattern matcher **7.7% alone**, `SifCache` `Data.Map` traversal 5.6%, `Ord AnyCompAp`/`eqT` compare 3.8% | **≈27%** | ≈20% |
| (a) DepSet unions — dominated by *generic `Hashable` derivation* for `CompEngDep` (`ghashWithSalt` chains), not the union call itself | **≈17%** | ≈28% |
| (c) MD5 via large-hashable (`runLH`, `mkCompAp`) | ≈5% | ≈5% |
| (d) `fullCaching` show/logrepr | ≈3% | ≈3% |
| (e) residual: engine-loop plumbing (`fmap` ~2.3%, PAQ/IntPSQ ~4.6%), long tail, GC (invisible to cost centers) | ≈43–47% | — |

Verdict: **(b) and (a) convicted, ~48% combined**; the speculative ranking
had the right order but underestimated how close (a) and (b) are — and a
large share of (a) is *hashing to support the union* (derived-Generic
`Hashable` on `CompEngDep`), not set operations proper. Two consequences:

1. **Interning + dense lookup moves to the front of the queue** — it
   attacks (a) and (b) simultaneously, since both are downstream of
   hashing/comparing `AnyCompAp`/`CompEngDep` values; hot-path keys become
   `Int`s.
2. The MD5→xxh3 swap and the `show` drop are real but second-order
   (~5% and ~3%); do them opportunistically, not first.

## Stage 0.6 — hand-rolled Hashable for the DepSet types (commit `84915c7`)

The "10-line quick win" from Stage 0.5's verdict, tried first as planned:
replaced Generic-derived `Hashable` with hand-written `hashWithSalt` on
`CompEngDep`/`CompEngDepKey`/`CompEngDepVer` (Types.hs), `Dep` (CompSrc.hs
— found on the hot path via `CompDep`'s newtype-deriving chain),
and `Word128`/`Hash128` (Utils/Hash.hs, now feeding the two `Word64`s
straight into the salt). `Eq`/`Ord` untouched;
`AnyCompSrcDep`/`ForAnyCompFlow` were already hand-written.

Result: **the targeted mechanism died; the wall clock didn't care.**

- Re-profile: hash-*computation* cost centers (`ghashWithSalt`,
  generic `hashWithSalt`, `hashSum`, the `Hash128` hashUsing wrappers)
  went **~14.0% → ~2.5%** of profiled time (~25% → ~7% of alloc) — an
  ~82% reduction in exactly what Stage 0.5's profile named.
- Scale 0.1 wall clock: cold flat within noise (avg 2.72 → 2.66 s);
  live *worse* by ~7% at default RTS (0.460 → 0.493 s avg, consistent
  across 3 trials), flat at `-A64m`.
- What grew to fill the pie: the *structural* costs — `$mCompAp` pattern
  matcher 7.7→9.0%, SifCache `Map` traversal 5.6→6.1%, `HashSet`/`HashMap`
  trie walks, `eqT` compare flat at 3.8%.

Lesson: bucket (a)'s cost was never mainly *computing* hashes — it's the
set/map structural work and the `AnyCompAp` dispatch around them. The
interning + dense-lookup step is confirmed as the real lever; hashing was
the cheap-but-thin slice. Keeping the change (strictly less work per op,
clear alloc win, and it simplifies the interning diff to come).

Fresh scale-1.0 numbers on the -O2/-N2 build (one run each), replacing the
Stage 0 headline figures for time comparisons going forward:

| Config | cold | live | GCs |
|---|---|---|---|
| default RTS | 48.75 s | 24.80 s | 18,486 |
| `-A64m` | **37.50 s** | **37.93 s** | 596 |

⚠ New surprise: at scale 1.0 under `-N2`, `-A64m` *reverses* on the live
update (37.9 s vs 24.8 s default — the opposite of every scale-0.1
measurement). Unexplained; needs its own investigation before `-A64m`
becomes a recommendation at production scale. (Also note cold at 1.0 got
slower vs the -O1 Stage 0 runs (42.8 → 48.75 s default RTS) — consistent
with Stage 0.5's finding that -O2 costs time here, and those Stage 0 runs
used `-N` all-cores.)

## Stage 1 — interning + dense lookup (commit `9fe2db3`)

The load-bearing optimization the profiles kept pointing at. `AnyCompAp`
identities are interned to dense `Int` ids (new
`CompEngine/Utils/Intern.hs`: `HashMap AnyCompAp Int` forward — hashing via
the now-cheap `capI_hash`, `eqT` only on bucket collision — `IntMap`
reverse, monotonic counter, **ids never recycled**; `runGc` releases both
directions, `resolve` fails loudly on a dead id). The five state containers
re-key to `Int`: `SifCache` becomes an `IntMap` (plus an `IntMap CompId` so
GC-time deletes need no `AnyCompAp`), `SifDeps` becomes
`DepMap Int InternedDep` (`DepMap`/`VerList` needed zero changes — already
key-generic; invalidation semantics untouched), vermap and outputs likewise.
The old sharing machinery is gone deliberately: no more `normalizeDep`, no
`lookupLE`-returns-the-map's-own-key trick, no `reallyUnsafePtrEquality#`
in `validateSifState` — the id *is* the canonical identity, and the
validator now checks the new invariant instead (every id referenced by a
container is live in the intern table). `sifs_stale` (PAQ) and the pending
sets stay `AnyCompAp`-keyed — small/transient, follow-on target.

Tests: 70/70 (65 pre-existing + 5 new intern tests) plus the app-level
suite, no assertions weakened.

### Numbers

Scale 0.1 (vs Stage 0.6, same flags): cold **1.92–1.96 s (−27%)**, live
**0.32–0.35 s (−32%)** default; `-A64m` **1.61 s (−22%) / 0.311 s (−13%)**.

Scale 1.0 — measured against a **fresh same-session rerun** of the parent
commit, because the Stage 0.6 table's 1.0 numbers did not reproduce
(see correction below):

| Config | cold (base→interned) | live (base→interned) | RSS settle | max_live |
|---|---|---|---|---|
| default | 41.50 → **31.05 s (−25%)** | 16.31 → 17.78 s (+9%) | 3435 → **2790 MB (−19%)** | 1820 → 1686 MB (−7%) |
| `-A64m` | 32.25 → **22.08 s (−32%)** | 14.45 → 14.64 s (flat) | 2347 → **1983 MB (−16%)** | 1644 → 1847 MB (+12%) |

Cold eval and RSS improve solidly at both scales; the live update improves
sharply at 0.1 but is flat-to-+9% at 1.0 (not yet understood — the live
path's remaining costs evidently scale differently; candidate: the
still-`AnyCompAp`-keyed PAQ/stale machinery is proportionally hotter in
propagation than in cold eval). The `max_live_bytes` split (−7% default,
+12% `-A64m`, deterministic per config) vs. consistently-improved RSS is
flagged, unexplained; likely a peak-sampling artifact of far fewer major
GCs (605 vs 18.8k) — a `-hT` heap profile would settle it.

### Profile after (scale 0.1)

The convicted centers collapsed: `$mCompAp` matcher **9.0% → 1.2%**,
`SifCache` structural traversal **6.1% → 3.2%**, `Ord AnyCompAp`/`eqT`
compare **gone from the flat table entirely**. Top of the profile is now
generic `Data.HashMap` machinery (the intern table's own lookups — cheap
`Hash128` dispatch, no `eqT`), the PAQ (`IntPSQ.fold'` 4.0%), MD5 (`runLH`
3.5%), and `Impl.hs` monad plumbing — i.e. the long tail is now the story,
plus the suspects (c) and the CompM-over-IO idea from the research section.

### ⚠ Correction to Stage 0.6's 1.0-scale table

Stage 0.6's scale-1.0 numbers (48.75 s cold / 24.80 s live default;
37.50/37.93 `-A64m`) **did not reproduce** in a fresh same-session rerun of
the very same commit (41.50/16.31 and 32.25/14.45). In particular the
"live-update reversal under `-A64m`" flagged there (37.9 s) measured 14.4 s
this session — the reversal was session noise (machine load), not a real
effect; consider it withdrawn. Lesson for every number in this doc:
cross-*session* comparisons at scale 1.0 carry far more variance than the
within-session run-to-run spread suggests; conclusions should rest on
same-session A/B pairs, as this stage's do.

## Stage 2 — CompM over IO, GenHaxl shape (commit `08be56a`) — mixed result

The representation swap the research section sketched: `CompM a` becomes
`CompMEnv -> IO (CompYield a)` with `cme_deps :: IORef [CompEngDep]` as a
mutable accumulator. `tellDep` conses (strict fold + `modifyIORef'`);
`compMBind`/`compMAp` no longer thread or union `DepSet`s at all — both
sides of an `<*>` share the env's IORef, and the partial-deps-on-failure
and run-both-sides applicative semantics fall out of sequencing both IO
actions before dispatching on results. `CompYield`/`CompReq`/`ContCompM`
(suspension protocol, smart-view continuations) byte-for-byte unchanged;
`Impl.hs` allocates one fresh IORef per cap evaluation and dedupes once at
finish. No IO leak to user code: the public facade re-exports `CompM`
abstractly (verified by attempted misuse failing to compile); no
`MonadIO` instance exists. Tests: 70/70 + app suite, no adjustments.

### Numbers (same-session alternating A/B, parent = `8fb682c`)

| Scale/config | cold | live | RSS settle | max_live |
|---|---|---|---|---|
| 0.1 default | flat (2.68 s) | 0.411 → 0.524 s (**+27%**) | — | +1.9% |
| 0.1 `-A64m` | 1.94 → 1.57 s (**−19%**) | +8% | — | +2.9% |
| 1.0 default | −2.7% | 22.6 → 26.5 s (**+18%**) | 1907 → 2255 MB (**+18%**) | +2.6% |
| 1.0 `-A64m` | 28.3 → 22.4 s (**−21%**) | 19.4 → 23.1 s (**+19%**) | +14% | flat |

Cold eval: flat to **−21%**. Live update: **+8–27% worse at every
scale/config pair** — the opposite of the design's hope, and on the metric
this arc cares most about. RSS flat-to-worse at 1.0.

### Why (profile, scale 0.1)

The per-bind union cost really is gone — but it was replaced by a
per-suspend cost that is bigger in this regime. The engine loop's
`case runCompM gen r of ...` used to be a *pure, bind-free* pattern match;
it is now `yield <- liftIO (runCompM gen env)` — a real `>>=` in
`CompEngineM`'s `StateT`-over-IO stack at every suspend/resume step. Two
new cost centers (`$fMonadCompEngineM_$s$fMonadStateT_$c>>=` 3.9% + sibling
0.7%) appeared, exceeding the savings; `compMBind.\` barely moved (2.0 →
1.8%). Total profiled ticks +56% with alloc only +1.7% — the cost is
sequencing, not allocation. The live path has the worst ratio of
suspend/resume round trips to useful work, hence it regresses hardest.
The trivial-bodies caveat cuts both ways here: real workloads would dilute
this per-suspend tax.

### Disposition — kept, pending one targeted follow-up

Not reverted yet: the cold-eval win is real, the dep-union deletion is the
architecturally right direction (and prerequisite groundwork for the
delimited-continuations endgame), and the regression has a *specific,
identified* mechanism rather than being diffuse: `CompEngineM`'s
un-specialized monad-stack bind on the suspend path. Obvious next levers,
in order: `INLINE`/`SPECIALIZE` the `CompEngineM` bind (classic fix for
exactly this cost-center shape), restructure the loop to stay in IO across
suspend/resume and only re-enter `CompEngineM` at round boundaries, or
lazily allocate the per-cap IORef. If none of those recover the live-update
regression, revert this stage — the doc's own rule: same-session A/B
decides.

## Stage 2.5 — specialize CompEngineM's monad stack (commit `4aab454`)

The targeted follow-up Stage 2's disposition demanded, and it recovered
the regression. Changes (Lever 1 only; the plain-IO loop restructure was
not needed): `CompEngineM`'s GND-derived `Functor`/`Applicative`/`Monad`/
`MonadIO` instances replaced with hand-written, representationally
identical methods carrying `{-# INLINE #-}` (no custom `(>>)` —
`-Wnoncanonical-monad-instances` forbids it; the default via the
now-INLINE `Applicative` stands); `INLINE` on the small hot-path helpers
(`withCompState`, `tellGarbage`, `tellOutputs`, `runCompEngineM`*);
mtl's default **lazy** `StateT` swapped for `Control.Monad.State.Strict`;
`INLINE` on `CompM`'s instance methods. No `SPECIALIZE` pragmas — the loop
was already monomorphic; there was nothing to specialize from. Tests
70/70 + app suite.

Evidence, three independent signals agreeing:

- The dictionary-trampoline cost center
  (`$fMonadCompEngineM_$s$fMonadStateT_$c>>=`) is gone from the profile.
- Total profiled alloc 7.48 → **6.71 GB** — below even Stage 1's 7.36 GB;
  profiled ticks −28% vs Stage 2.
- Three-way same-session A/B (Stage 1 / Stage 2 / fix) at scale 1.0,
  `-A64m` (the low-noise config: ~590 GCs): **fix wins outright** —
  cold **20.2 s** (vs 31.1 / 22.6), live **14.8 s** (vs 18.1 / 18.7).
  That's −35% cold and −18% live against Stage 1, keeping Stage 2's
  cold win *and* beating Stage 1's live number.

Noise notes, for the record: scale-0.1 was too noisy this session to
separate the three builds (~15% cluster); default-RTS live at 1.0 remains
the noisiest metric in the doc (one Stage 2 sample swung 41.9 → 19.3 s
across identical-binary rounds). The `-A64m` rows carry the conclusion.

Disposition: **Stage 2 + 2.5 kept.** Cumulative best-config numbers at 1M
now: cold **20.2 s**, live **14.8 s** — versus Stage 0's 42.8 s / 24.4 s
(different sessions, so indicative only; but the direction is settled).

## Interlude — compiler upgrade: GHC 9.4.5 → 9.10.3, lts-24.51 (commit `b220764`)

Done before the columnar rework specifically to isolate compiler effects
from the rework's A/B. Three findings, one of them the headline.

**Correction first**: every stage above ran on **GHC 9.4.5** (resolver
`nightly-2023-05-08`), not the "GHC 9.10.3" the runtime-research section
claimed — that was ghcup's default toolchain, never what `stack build`
used. Consequence worth knowing: the delimited-continuation primops
(`prompt#`/`control0#`) landed in GHC 9.6, so the "CompM over delcont"
endgame was *not actually available* until this upgrade. It is now.

**Performance: flat.** Same-session A/B, old side built from a pristine
worktree at `9a1e675` on 9.4.5, new side `b220764` on 9.10.3 (isolation is
"resolver + minimal compat fixes": a few explicit imports for newer mtl,
`-Wno-x-partial`; nothing touching engine code). At `-A64m` — the doc's
conclusion-carrying config — both scales are within noise: 1.0 cold
18.66 → 18.62 s, live 13.86 → 13.96 s, max_live 1846 → 1854 MB. The
compiler bump neither pays nor costs; the optimization arc's numbers
carry over.

**⚠ Headline: GHC 9.10.3 exposes a latent driver race.** With the default
4 MB nursery, the driver deadlocks ("thread blocked indefinitely in an STM
transaction") immediately after cold-eval settle — **100% of scale-1.0
attempts (2/2), 5/6 at scale 0.1**; also reproduced at `-A8m` and `-N1`.
Not reproduced on 9.4.5 (4/4 clean, same code); vanishes at nursery
≥16 MB (`-A16m`/`-A64m` 3/3 clean). The compat fixes touch no
STM/concurrency code, so this reads as a GHC 9.4→9.10 scheduler/GC timing
change surfacing a pre-existing race in the driver — very plausibly the
same latent bug behind Stage 0.5's `-N1` deadlock on 9.4.5. Needs its own
investigation (deliberately not attempted inside the resolver-bump
change). **Benchmark policy until fixed: `-A64m` is the standard config
on 9.10.3; default-RTS rows can't be collected.**

## Interlude — the STM deadlock, root-caused (commit `a1c5933`)

Resolved before resuming any optimization work. **The engine is
exonerated: this was a benchmark-harness bug**, and every prior mention of
a "driver race" (Stage 0.5's `-N1` crash, the compiler interlude's
headline) should be read as retracted in favor of this diagnosis.

Mechanism, from a captured failing trace (in-memory buffered tracing,
dumped on exception — stdout logging perturbed timing enough to hide the
race): `compDriver'`'s `shouldStartNextRun` posts each run's `RunStats`
*eagerly*, tagged with the **upcoming** run number, before that run has
attempted its blocking wait. So `waitForRunAtLeast n` only ever guaranteed
`rs_run >= n` — and the bench's post-cold-settle wait broke that contract
by unconditionally asking for `rs_run rs1 + 1`. When the engine raced
ahead and `rs1` overshot to an already-settled run, the requested run
number could never be posted until a source mutation — which the bench
only performs *after* the wait returns. Both threads park; GHC's GC-based
deadlock detector converts the hang into `BlockedIndefinitelyOnSTM` only
when a GC happens to run in the window. That explains everything
observed: fires at cold settle (where the window opens), nursery-size
dependence (fewer GCs → fewer interleavings → and no detector runs),
`-N1` near-certainty (coarser scheduling → overshoot every time), and
9.4.5's apparent cleanliness (same latent bug, luckier cadence).
`HashMapFlow.waitChangesImpl`'s destructive-read-then-retry — the prime
suspect — was checked structurally and empirically: **not guilty**, no
lost wakeup.

Fix (root, not mitigation): seed the settle-wait with `rs1`'s own run
number (`waitForFullSettle` already re-checks `rs_staleCaps` before
advancing, so this is sufficient); `waitForRunAtLeast`/`waitForFullSettle`
promoted into `Driver.hs` proper with the "at least" contract documented,
since every `compDriver'` caller faces the same hazard; a deterministic
regression test (`TestDriver.hs`) proves the buggy pattern hangs and the
fixed one returns. Tests now 74/74.

Verification: 0.1 default RTS **10/10** (was 6/6 failing); 1.0 default
**3/3**; `-N1` **3/3** — confirming the Stage 0.5 `-N1` crash was the
same bug; `-A64m` sanity matches the doc's numbers within noise.

**Benchmark policy update**: the "`-A64m` only" restriction is lifted.
Default-RTS scale-1.0 rows are collectable again; for the record, on the
current build (GHC 9.10.3, post-fix): **cold 22.5–23.6 s, live
14.0–14.7 s, 80,767 reruns** — previously uncollectable on this
toolchain.

## Memory roadmap — the path to Rust parity ("nothing off the table")

Where we stand at 1M caps (Stage 1/2 measurements): `max_live_bytes`
~1.69–1.85 GB (**~1.7–1.85 kB/cap live**), RSS ~2.0–2.3 GB. Rust Stage 5
engine-only: 328–354 MB RSS (**~330 B/node**). Gap: **~6–7× on RSS**. The
Rust notes' Interludes 1–3 (written against this codebase) map the options;
this section turns them into a decision.

### Why the incremental diet alone stalls at ~2× (do not stop there)

The Interlude-2 items — delete `sifs_vermap` (−64 B/cap), move
`ccm_logrepr`/`ccm_approxCachedSize` onto per-def `CompCacheBehavior`
(−88, also kills a `show` per rerun), drop the `OM.insert key mempty`
empty-outputs entries (−56), flatten the five-box cache-value tower into
`data Cached = forall a. CachedOk !a !Hash128 | CachedHashOnly !Hash128 |
CachedFail` (−70–90), dict dedup + tag hoisting (−27), unboxed edge
vectors + dropping `VerList`'s version level for flat rdeps + a changed
bit (−~500, semantics change) — sum to a projected **~565–600 B/cap
live**. But live heap is not RSS: GHC's copying collector holds ~2× live
(`-F 2`) plus to-space during major GCs, so ~600 B/cap live is still
**~1.2–1.8 GB RSS — 4–5× Rust**, forever, no matter how disciplined the
boxed representation gets. That multiplier is the wall; the Rust notes'
"What no diet fixes" section called it precisely.

### The equalizer: columnar-unboxed state (Interlude 3's Tier 2)

The only identified move that reaches ~1×: replace `SimpleStateIf`'s
containers with per-def struct-of-arrays behind the existing
`CompEngineStateIf` seam (an interface record — `Impl.hs` doesn't change):

- `param_hash`/`result_hash`/`flags` → unboxed columns
  (`Data.Vector.Unboxed`/`MutablePrimArray`): 16/16/1 B per row, flat.
- Edges → per-row unboxed `Int` arrays or CSR-with-slack: ~40–90 B/cap
  both directions vs today's boxed-record-in-HAMT ~790 B.
- Typed value + param columns live on the per-def `Comp p a` record where
  both types are statically known — the same trick as Rust's
  `CompDef<P, R>` typed column; existentials survive only at the
  already-existing `AnyComp` boundary. (`Vector (Maybe a)` boxes every
  element — use a has-result bit + separate column instead.)
- Per-def index `HashMap Hash128 Int` (~40–64 B/cap); Stage 1's intern
  table shrinks to this role.
- Adopts flat-rdeps + changed-bit (the `VerList` semantics change) as part
  of the rewrite, as Interlude 3 assumed.

**The Haskell-specific payoff — why this reaches parity when nothing else
does**: large unboxed `ByteArray#`s live in the large-object area — never
scanned, never copied. The 2–3× GC multiplier then applies only to the
small boxed residue (~50 B/cap). Projected: **~170–250 B/cap, RSS
~250–350 MB ≈ Rust Stage 5's 328–354 MB.** Shake interning keys to `Int`
and storing flat records is the existence proof that this is idiomatic,
not exotic. Costs: `atomically` composability at the state layer (an
`MVar`/IO suffices — `stepCompEngine` is sequential), and the STM
free-snapshot advantage for a future persistence flusher is spent back.

### Recommended sequencing

1. **Go straight to columnar-unboxed** (Tier B). Do NOT burn a week on
   the boxed diet first: vermap, logrepr, empty-outputs, and the box
   tower all *die automatically* in the columnar rewrite; only the
   cache-behavior API change (logrepr → per-def) is worth landing early
   since it's user-visible API and kills per-rerun `show` work.
2. Stage the rewrite behind `CompEngineStateIf` with the test suite as
   the referee at each step (the seam held for Stage 1; trust it).
3. After: RTS pass (`-F`, `-A`, `--nonmoving-gc`, compact regions for the
   surviving identity objects) — small dials on a small boxed residue.
4. Optional floor below parity: the eviction knob is already typed —
   `CapMetaCached` vs `CapValueCached` (`Core.hs:140`), and
   `evalWithCapCached` handles the meta-only case by recomputing today.
   Demoting cold values at runtime is a state transition, not new
   machinery: ~60–100 B/cap floor, memory becomes a knob.

Honest risks: this is the deepest change yet (bigger than Stage 1 and 2
combined); mutable columns + laziness need the same id-lifecycle rigor
Stage 1 established (ids in columns, `AnyCompAp` only at boundaries);
GC of rows means free-lists and garbage-tolerant columns (Rust Stage 5's
row-reuse rules transfer nearly verbatim); and the 2× write-amplification
argument for persistent structures disappears — future persistence work
inherits mutable-snapshot complexity Rust already solved (Stage 2's
pending-map pattern ports back).

## Stage 3 — columnar-unboxed state (commits `a8ae7ff`…`be072a8`, harness fix `20a9c69`)

The Memory-roadmap rewrite, landed as five separately-buildable increments
(the earlier WIP branch's architecture adopted after review; its commit
sequence restarted and every claim re-verified — `wip/columnar-rework`
preserved as archive): (1) `DefTable` per-def struct-of-arrays skeleton
with packed `DefRef(defIdx, row)` ids + free-list; (2) unboxed
`param_hash`/`result_hash`/`flags` columns (`Data.Vector.Unboxed.Mutable`);
(3) the big wiring commit — `DefTable` replaces `SimpleStateIf`'s five
containers, flat rdeps + changed-bit replace `VerList`'s version
bucketing, typed param/value columns, `CompCacheMeta` shrunk to just the
hash, `SifCache.hs` deleted, state moves TVar/STM → MVar/IORef (plan
steps 3+4 fused: whole-module typechecking allows no halfway state once
the old containers are gone); (4) lifecycle hardening — dead-row resolves
fail loudly, garbage-tolerant row reuse (Rust Stage 5's rules) verified by
tests; (5) dead-code kills: `Intern.hs` (superseded by per-def indexes),
`VerList.hs`, `DepMap`'s container. Comp-dep edges now carry a per-edge
*observed version* — required by `test_modifcationWhileWorkingOnQueue`
(impure-cap detection), so that semantics moved rather than died. Tests
86/86 (net of suites whose subjects were deleted); the seam held —
`Impl.hs`'s algorithm and `CompEngineStateIf` unchanged.

Bonus find: a **second** eager-RunStats settle ambiguity (sibling of
`a1c5933`'s), previously masked by the old engine being slow — the
columnar engine settles fast enough that the bench's drain-wait could
match a pre-posted stats record and report "settled" with reruns still
climbing. Fixed in the bench harness (`20a9c69`); rerun counts re-verified
across 7 scales.

### Numbers (same-session alternating A/B vs `7a19093`, 2 runs/side)

Scale 1.0, `-A64m`:

| | cold | live | RSS settle | max_live | B/cap | GCs |
|---|---|---|---|---|---|---|
| old | 46.4–46.6 s | 15.5–16.0 s | ~3.26 GB | 1464.8 MB | 1465 | 177 |
| new | **7.29–7.31 s** | **10.9–11.0 s** | ~2.24 GB | **1132.0 MB** | 1132 | 66 |

Scale 1.0, default RTS:

| | cold | live | RSS settle | max_live | B/cap | GCs |
|---|---|---|---|---|---|---|
| old | 60.7–60.8 s | 20.6–22.9 s | 2.5–3.3 GB | ~1747 MB | 1747 | 37,497 |
| new | **10.1–10.3 s** | **13.3–13.6 s** | **1.53 GB** | **838 MB** | **838** | 14,022 |

**Cold eval 6.4× faster** (46.5 → 7.3 s best config — columnar locality
turned the biggest number in this doc into the smallest), live −25–35%,
`max_live` −23% to −52% depending on RTS config. 999,760 instances /
80,767 reruns, bit-identical to every prior stage. (This session's
absolute wall-clocks ran hotter than earlier sessions' — same-session
pairs carry the conclusions, as always.)

### Verdict vs. the ~250–350 MB parity projection

Short of it, for two architectural (not incidental) reasons, honestly
identified rather than papered over: the typed **param/value columns are
still boxed** (`p`/`a` admit no `Unbox` constraint — real param types in
the test suite include `ByteString`), and the **per-row unboxed edge
vectors live in the nursery**, not the large-object area — so the
copying-GC multiplier the roadmap aimed to escape still applies to a
large slice. Reaching parity needs per-def monomorphized value storage
and one shared CSR edge array per def — a materially bigger change,
deliberately not attempted in this stage. Standing vs. Rust Stage 5:
cold 7.3 s vs ~2.9 s (**~2.5×**, was ~14× at Stage 0); RSS 1.53 GB vs
~330 MB (~4.6×); live 10.9 s vs ~0.5 s (still the widest gap).

API/semantics changes: `CompCacheMeta` → hash-only newtype;
`fullCaching`/`hashCaching` drop their `Show` constraint (the per-rerun
`show` is finally dead); `initialSifState` → `newSifState :: IO`;
row ids are recycled (Stage 1 never recycled; the free-list + loud
dead-resolve contract covers it).

## Stage 4 — post-columnar optimization campaign (running log)

Ranked items from the Stage-3 code read + library research, tried one at a
time, benchmarked between, reverted if they don't improve. Ground rule for
this campaign: **no new type constraints on the public interface**
(opportunities that would need one get flagged, not implemented).

### 4a — RTS flag sweep: `-A64m` becomes the default (commit `e3f500a`)

Pure measurement, no engine code. Swept `--nonmoving-gc`, `-c`, `-F1.2/1.5`,
`-n4m`, `-A16/32/64m` and combinations, triaged at 0.1 and confirmed at 1.0
(2–4 runs each).

**Winner: `-A64m`**, now baked into the executable's rtsopts alongside
`-N2`. Scale 1.0: cold **9.69–10.21 → 7.93–8.08 s (~20%)**, live
**11.90–12.01 → 10.87–11.14 s (~7–8%)**, every run, no overlap; RSS never
worse than default's range. Tests 86/86.

Rejected, with reasons: `-c` (compacting) costs +25–30% cold;
`--nonmoving-gc` loses on both axes here **and reports a badly inflated
`max_live_bytes` — 2059 MB vs the moving collector's 842–871 MB (~2.4×)**,
almost certainly floating garbage under this workload's churn rather than
a real live set (worth remembering before trusting that stat under a
concurrent collector); `-n4m` no effect at `-N2` (needs many more
capabilities to matter); `-F` variants no better.

Gotcha found: Cabal's legacy `ghc-options` word-splits tokens, so
`-with-rtsopts=-N2 -A64m` as one entry **silently drops everything after
the space** — no build error, just a wrong runtime default. Two separate
`-with-rtsopts=` entries concatenate correctly (verified via
`+RTS --info`).

> **⚠ Correction (surfaced by Stage 5's work):** the claim above that two
> separate `-with-rtsopts=` entries concatenate is **wrong** — `-with-rtsopts`
> is single-valued; GHC keeps only the last occurrence, and a second entry
> silently drops the first rather than joining it. Per commit `24b3ad5`
> (blog-post review, prior to this doc's Stage 5): once `e3f500a` (Stage 4a)
> added `-with-rtsopts=-A64m` as a *second* entry alongside the executable's
> existing `-with-rtsopts=-N2`, `-N2` was silently dropped — "every
> measurement was on 1 capability" from that point on, not caught until the
> blog post's repro commands were run against a clean build. This doc's own
> stage sections were not individually re-audited against that fact; treat
> any `-N2`-attributed conclusion from Stage 4a onward with that in mind. The
> benchmark's own stanza got the correct quoted single-entry form
> (`'"-with-rtsopts=-A64m -T"'`) in Stage 5 below, precisely to not repeat
> this.

Also noted: `-A64m`'s RSS-after-settle is noisy run to run (1056–1492 MB);
the win is unambiguous on the time axis only. One `--nonmoving-gc` run
showed RSS *dropping* 1594 → 767 MB mid-run (pages returned to the OS),
observed once, not chased.

### 4b — CSR per-def edge arenas (commit `45899d2`) — **kept**

The biggest remaining memory item. Both edge columns (per-row boxed vectors
of unboxed vectors — each row's edges a separate nursery-resident heap
object, and worse, `VU.Vector` of tuples is structure-of-arrays so a
fan-in-3 row cost a `V_3` constructor plus **three** `ByteArray`s) became
two shared growable per-def arenas: flat unboxed `VUM.IOVector Word64`,
stride 3 for comp-deps (target `DefRef` + observed-hash hi/lo), stride 1
for rdeps, with unboxed `Int32` offset / `Word32` len columns per row.

Mutation: **append-new-span, mark-old-span-dead**, because every write
replaces a row's whole edge set (no single-edge splice exists on the hot
path, so per-row slack buys nothing). A per-def dead-word counter triggers
compaction once dead words exceed half the used length — peak bounded at
~2× live, amortized O(1) per write, and it hooks the write path rather
than needing a GC callback. Per-edge observed-hash semantics preserved
exactly. `DefTable`'s external API is unchanged, so `SimpleStateIf.hs`
needed **zero** edits.

| Scale | metric | before | after | Δ |
|---|---|---|---|---|
| 1.0 | max_live | 1107.0 MB | **886.5 MB** | **−20%** |
| 1.0 | RSS settle | ~2000 MB | **~1530 MB** | **−23%** |
| 1.0 | cold | 8.9 s | 8.2 s | −7% |
| 1.0 | live | 10.75 s | 10.64 s | flat |
| 0.1 | max_live | 104.6–112.8 MB | **75.2 MB** | **−28 to −33%** |

Instance/rerun counts bit-identical. Tests 89/89.

**The large-object hypothesis is confirmed**, which is the load-bearing
part: at equal total allocation (~60 GB), `+RTS -s` shows bytes copied
during GC **3.45 → 2.11 GB (−39%)** and GC time **2.67 → 1.51 s (−43%)`,
with max slop rising 6.6 → 33.5 MB (block-rounded large-object
allocations). The arenas really are sitting outside the copied path — the
mechanism the roadmap said was needed to escape the copying multiplier,
now demonstrated rather than assumed.

### 4c — open-addressed index replaces the per-def HAMT (commit `db24c13`) — **kept**

`dt_index :: IORef (HashMap Hash128 Int)` exploited a redundancy: the key
is *already* in the unboxed `dt_paramHash` column, so the index never
needed to store keys at all. Replaced with an unboxed open-addressing
table of row ids only (`VUM.IOVector Int32`, `-1` sentinel; slot =
`w128_first hash .&. (cap-1)`, power-of-two capacity; a probe is verified
by comparing the full 128-bit hash read from the row's own column via a
`getHash` callback). Growth doubles at load factor 0.7 — a rebuild copies
no keys, just re-slots row ids. **Deletion is backward-shift** (Knuth),
chosen deliberately over tombstones because the live-update path frees and
reuses rows constantly and tombstones would accumulate under exactly that
workload; occupied-slots always equals live-entries. `SifCache`-style API
stability held again: zero edits outside `DefTable.hs`.

| Scale 1.0 | before | after | Δ |
|---|---|---|---|
| cold | 8.10 s | **7.04–7.12 s** | **−13%** |
| live | 10.58–10.66 s | 10.61–10.69 s | flat |
| max_live | 886.5 MB | 853.9 MB | −3.7% |
| RSS settle | 1581.8 MB | 1457.6 MB | −7.8% |

Tests 89 → **99** (10 new: collision chains, mid-chain delete, grow
preserves entries, 500-cycle churn asserting capacity does *not* inflate,
plus a `DefTable`-level churn test proving a freed row's hash is
unfindable).

**Estimate correction**: this section's earlier guess of ~88 B/cap for the
index was too high — the real saving is ~33 B/cap (886.5 → 853.9).
Deterministic and reproducible, but the HAMT was cheaper than the
back-of-envelope suggested. The decisive win here was **time, not
memory**: −13% cold, with no run overlap.

### 4d — unboxed value/param columns via `Typeable` dispatch (commit `e6738cb`) — **kept**

The item the roadmap thought needed a public `Unbox` constraint. It does
not. `dt_param`/`dt_value` became a GADT existential:

```haskell
data Column e where
  ColBoxed   :: !(IORef (VM.IOVector e)) -> Column e
  ColUnboxed :: VUM.Unbox u => !(e :~: u) -> !(IORef (VUM.IOVector u)) -> Column e
```

`mkColumn` tests `e` with `eqT` at column-construction time against a
fixed set (`Word32`/`Word64`/`Int`/`Char`/`Bool`/`Double`) and captures a
`e :~: u` equality proof on a match; everything else falls back to boxed.
The `Refl` witness makes the unboxed read/write typecheck directly — no
conversion function, just one constructor-tag branch per access.

**Public API stayed constraint-free**, which was the hard requirement:
`DefTable.new` gained `(Typeable p, Typeable a)`, but that is an internal
module, and its only caller already had those in scope —
`IsCompParam p = (Show p, Typeable p, LargeHashable p)`, so `Typeable` was
always there transitively. `defineComp`/`wireComp`/`Comp`/
`CompCacheBehavior`/`IsCompParam`/`IsCompResult` are byte-for-byte
unchanged, and `ByteString`/`String` params keep working via the fallback
(tested explicitly).

| metric | before | after | Δ |
|---|---|---|---|
| 1.0 max_live | 853.9 MB | **817.3 MB** | **−4.3%** |
| 0.1 max_live | 85.5 MB | **68.2 MB** | **−20.3%** |
| cold / live / GCs | — | — | flat within noise |

Bigger win at small scale, where the param/value columns are a larger
share next to still-small edge arenas. Tests 99 → **103**.

### 4e — interned CSR arena for source deps (commit `40a754a`) — **kept, and the biggest surprise of the campaign**

`dt_srcDeps :: IORef (VM.IOVector (HashSet AnyCompSrcDep))` — the last
conspicuously boxed per-row column — became an `EdgeArena` (the same CSR
machinery 4b built, stride 1) storing **interned ids**, backed by a per-def
`SrcDepIntern` (forward `HashMap AnyCompSrcDep Int`, reverse growable
vector, monotonic counter). `readSrcDeps`/`writeSrcDeps` keep their exact
`HashSet AnyCompSrcDep` signatures — interning is entirely internal — so
`SimpleStateIf.hs` again needed zero edits.

| | before | after | Δ |
|---|---|---|---|
| 1.0 max_live | 817.3 MB | **375.7 MB** | **−54.0%** |
| 1.0 RSS settle | ~1395 MB | **784.9 MB** | **−44%** |
| 0.1 max_live | 81.7 MB | **38.0–40.1 MB** | **−51 to −53%** |
| cold / live | — | — | flat within noise |

**The estimate was wrong by 13×** — this section predicted ~34 B/cap;
actual saving is **~442 B/cap**. Root cause of the discrepancy, found by
the agent and worth recording as a bug-shaped finding rather than a tuning
result: `wrapCompSrcDep`/`compSrcId` **reconstruct a fresh
`CompSrcId`/`Text` on every call** instead of sharing one, so ~205k
populated rows referencing only 300 distinct source keys were carrying
~683× duplication. Interning collapses that to near zero. The lesson
generalizes: per-call reconstruction of "identity" values is invisible in
a data-layout audit and can dwarf the layout itself.

Accepted limitation, documented in the module haddock rather than hidden:
interned src-dep ids are **never recycled** (Stage 1's original tradeoff).
Because `AnyCompSrcDep` carries an observed version, ids scale with
distinct `(key, version)` pairs ever seen — negligible here, unbounded in
principle for a long-running high-churn system. Refcounted reclaim was
considered and deliberately deferred: memory-management complexity with no
correctness stakes.

Semantics verified rather than assumed: 5 new DefTable tests (round-trip,
dedup, same-key-different-version, compaction under 5000 overwrites, row
reuse); the existing `test_gc`/`test_impureComputation`/
`test_modifcationWhileWorkingOnQueue`/`test_olderVersionInsertedLater`
suites exercise src-dep versioning and GC through real round trips; and
the DirSync app test confirmed `compSrcUnregister` still fires
(`Deleting 3 deps of CompSrcId "FileSrc" "fileSrc"`). Tests **108/108**.

### Campaign scoreboard after 4a–4e

**375.8 B/cap live at 1M caps, vs Rust Stage 5's ~330 B/node — live-heap
parity is essentially reached** (1.14×), the roadmap's headline goal.
RSS 785 MB vs Rust's 328–354 MB is ~2.2×, and that residue is now
dominated by GHC's copying headroom rather than by data layout. Cold eval
6.8 s vs Rust ~2.9 s (2.3×). Live update 10.5 s vs ~0.5 s — **still ~20×,
and now conspicuously the only metric that has not moved all campaign.**

### 4f — the 20× live-update gap was an accidental quadratic (commit `3e817bb`) — **kept**

The hypothesis behind this investigation — "a gap that appears in one
phase but not the other, over identical data structures, is not a constant
factor" — was correct, and the culprit is a two-line-fixable O(n).

**Mechanism.** `Impl.hs`'s `stepCompEngine` calls
`withCompState staleQueueSize` **once per dequeued cap** — 80,767 times per
round at scale 1.0. `staleQueueSize` resolved to
`PriorityAgingQueue.size`, which summed `Data.HashPSQ.size` over four
priority sub-queues — and `Data.HashPSQ.size` is **O(n)**: confirmed by
reading `psqueues-0.2.8.3`'s `Data/HashPSQ/Internal.hs`, it folds the
entire tree (`IntPSQ.fold'`) because bucket sizes aren't tracked
incrementally. So every rerun paid a cost proportional to the current
queue size: quadratic in the size of a propagation round.

**Why cold eval never showed it**: `startCompEngine` evaluates recursively
via `execAp` and never goes through `stepCompEngine`/`dequeueNextCap`/
`staleQueueSize` at all. That is exactly why cold eval improved across
Stages 0–4e while the live update sat unmoved — they don't share this path.

**The scaling table is the proof** (µs/rerun, dirty-set fraction constant):

| Scale | reruns | before | after |
|---|---|---|---|
| 0.05 | 4,755 | ~14.5 | ~8.2 |
| 0.1 | 8,248 | ~19.7 | ~8.3 |
| 0.25 | 24,671 | ~49.4 | ~9.8 |
| 0.5 | 41,627 | ~71.8 | ~10.3 |
| 1.0 | 80,767 | **~130** | **~10.4** |

Per-rerun cost grew ~9× over a 20× scale range before; it is **flat at
every scale** after. Textbook O(n) → O(1).

Profile (live-phase-isolated via a new, off-by-default
`PERSIST_BENCH_LIVE_LOOPS` diagnostic, `-fprof-late`): `IntPSQ.fold'` and
friends were **22.5% of time / 24.4% of alloc — the largest cost-center
family in the profile**, more than double the next item. The old whole-run
profiles under-reported it at ~4% purely because they were cold-eval
dominated; isolating the phase was what made it visible.

**Fix**: an incrementally-maintained `paq_size` in `PriorityAgingQueue`
(updated in `enqueue`/`deleteView`/`dequeue`; `upgrade` only moves entries
between sub-queues so it doesn't touch the count). `size` becomes O(1).

| Scale 1.0 | before | after |
|---|---|---|
| live | 10.55–10.85 s | **0.80–0.85 s (~13×)** |
| cold | 6.89–6.97 s | 6.86–6.92 s (flat) |
| max_live | 375.7 MB | 375.7 MB (identical) |
| GCs | 925 | **388** |
| reruns | 80,767 | 80,767 (bit-identical) |

Everything else was **exonerated with evidence, not assumed**: the
src-index walk is bounded by the key's own dependents; `dequeueNextCap` is
O(log n) + O(1); `deleteDeadOutputs` runs only at `nRun==1`;
`commitPendingOutputsForKey` is bounded by the row's own outputs, and the
one genuinely O(total-outputs) function (`getCompSinkOuts`) is never on the
per-rerun path.

### Scoreboard after 4f — the arc, end to end

| metric | Stage 0 | now | vs Rust |
|---|---|---|---|
| cold eval | 42.8 s | **6.9 s** | 2.4× |
| live update | 24.4 s | **0.85 s** | **1.7×** |
| live heap | ~1,500 B/cap | **375.8 B/cap** | 1.14× |
| RSS | ~2.6 GB | **785 MB** | 2.2× |

Live update is now the *closest* metric to Rust, having been the worst by
an order of magnitude. Cold 2.4× and live 1.7× are believable constant
factors for a boxed-by-default runtime; the remaining RSS ratio is GHC's
copying headroom, not layout.

### 4g — refcounted src-dep ids: the 4e leak, closed (commits `6f5a8af`, `e0d0621`)

4e's "ids are never recycled" note was a real leak, not a footnote:
`AnyCompSrcDep` carries an observed version, so distinct `(key, version)`
pairs accumulate forever in a long-running high-churn system, in both the
forward `HashMap` and the boxed reverse vector — even though old versions
become unreferenced the moment their observing rows re-run.

Fixed with **reference counting** (chosen over a threshold sweep: precise,
prompt, and O(1) per edge on a path that already walks the span).
`SrcDepIntern` gains an unboxed refcount column parallel to the reverse
table plus an id free-list; the reverse element type became
`Maybe AnyCompSrcDep` so a released slot genuinely **drops the boxed
value** rather than orphaning it behind a stale forward entry — which was
half the leak. `sdiIntern` is now split from `sdiRetain`/`sdiRelease` so a
writer can intern its whole new span before retaining any of it.

Two subtleties worth recording:

- **The overlap-ordering hazard is the common case, not an edge case.**
  `writeSrcDeps` runs on every finish, changed or not, so a rewritten span
  usually shares ids with the old one. Retain-new-before-release-old is
  therefore load-bearing: releasing first would transiently zero a
  refcount that is about to be re-established, freeing an id out from
  under a live row. Tested explicitly.
- **`freeRow` releases the src-dep span eagerly** (via a new `eaTakeRow`
  that reads the span, then zeroes offset/len and marks the words dead) —
  unlike `compDeps`/`rdeps`, which stay garbage-tolerant until compaction.
  An unreferenced id must become reclaimable immediately, and the zeroing
  also prevents a double-release when the row is later reused.

`sdiResolve` now checks refcount > 0 as well as range: recycling reopens
an "in range but dead" gap that a never-recycled table couldn't have.

Cost, as expected for a correctness fix: **cold +1.2%, live flat, memory
flat** — all well under the 5% materiality bar. Public API unchanged.

### Test coverage audit (bundled with 4g)

Tests **108 → 126**. The audit's own finding was that *nothing tested the
leak* — the never-recycled table had simply been accepted. Added: refcount
retain/release/underflow; shared-id survival when one of two referencing
rows frees; the overlap-ordering hazard under repeated overwrite;
version-churn boundedness; reused-id-behaves-like-fresh; loud failure on
dead-id resolve and release-underflow; "no stale garbage in any column on
reuse"; index growth interleaved with frees; arena compaction with shared
ids and bystanders; a row freed and reused *between* two compactions; and
a **QuickCheck model-based property** (100 random alloc/free/write
sequences) asserting the intern table fully drains once everything the
model tracks as live is freed. Plus two engine-level churn tests through
real `capEvaluation` round trips (single cap, and 50 caps sharing one key)
confirming the live count tracks the referenced set, not the total ever
observed.

Nothing pre-existing was found broken. One gap left open deliberately and
recorded rather than papered over: no direct white-box test of
`resolveRefToAny`'s dead-row loud-failure contract — unmodified Stage-3
machinery, and constructing a dead-ref scenario would need more debug
surface than it justifies; it is exercised transitively (never triggered,
which is the correct outcome) by every engine test.

### 4h — columnar reverse source index (commit `376caaa`) — **kept**

The mirror of 4e, on the reverse side. `sifs_srcIndex ::
HashMap AnyCompSrcKey (HashMap DefRef AnyCompSrcVer)` (~205k boxed entries
in nested HAMTs) became a new `Utils/SrcIndex.hs`: a `KeyIntern`
(`AnyCompSrcKey → Int`) plus `IntMap SrcKeyArena`, where each arena holds
an unboxed `DefRef` column and a parallel boxed `AnyCompSrcVer` column.

Two design calls, both argued from ownership rather than habit:

- **No refcounting on `KeyIntern`** — and this is *not* repeating 4e's
  mistake. Unlike `SrcDepIntern` (many rows independently retain one id),
  a source-key id here has exactly **one owner at a time**: the single
  arena created on first dependent, released on last. Plain
  assign/recycle is the correct match for 1:1 ownership; refcounting
  would be ceremony.
- **Versions are not interned.** Per-dependent versions genuinely differ
  — that is the entire reason per-dependent tracking exists
  (`test_modifcationWhileWorkingOnQueue`) — so interning them would
  reopen the many:1 sharing problem for no established payoff.

Add/remove is single-entry (not `EdgeArena`'s whole-span replace), so
removal is scan-then-swap-with-last: no tombstones, no compaction.

| | before | after | Δ |
|---|---|---|---|
| 1.0 RSS settle | 785.8 MB | **751.0 MB** | **−4.4%** |
| 1.0 max_live | 375.7 MB | **365.4 MB** | −2.7% |
| 0.1 RSS settle | 157.8 MB | **146.0 MB** | −7.5% |
| cold / live / GCs | — | — | flat |

Tests **126 → 145**. Instance/rerun counts bit-identical.

#### ⚠ The bug this stage caught: a missing bang worth 10×

Mid-implementation, cold eval was **~10× slower with ~9× the allocation**
— while instance and rerun counts stayed bit-identical, i.e. a pure space
leak, not a correctness fault. Cause: **`Data.Vector.Mutable.write` has no
strictness contract**, unlike the `Data.HashMap.Strict` it replaced. An
unforced `ver` thunk retained each row's entire
`AnyCompSrcDep`/`DepSet` closure. One bang pattern
(`skaAppend ska ref !ver`) fixed it.

Generalizable lesson, and the reason it is recorded here rather than in a
commit message: **every migration from a `.Strict` container to a mutable
boxed column silently drops a strictness guarantee.** The columnar work of
Stages 3–4 did exactly that migration repeatedly. That makes a strictness
audit of the remaining boxed columns (`dt_param`/`dt_value`'s `ColBoxed`
path, `OutputsMap`) a high-confidence next move — this class of bug is
invisible to correctness tests and to any data-layout audit, and shows up
only as time and allocation.

### Job B — the `hashtables` head-to-head, finally measured

Standalone microbenchmark (300 keys, 205k ops, 5k churn cycles — sized to
the real tables' post-4e/4g steady state), `Data.HashMap.Strict` vs
`Data.HashTable.IO` `Basic`/`Cuckoo`, 3 runs each:

- **single-field key shape** (`AnyCompSrcKey`, i.e. `KeyIntern`'s table):
  `Basic` ~0.013 s vs HashMap ~0.020 s — a **consistent ~35% win**
  (Cuckoo ~25–30%).
- **two-field key shape** (`AnyCompSrcDep`, `sdi_forward`): all three tie
  at ~0.041–0.043 s — **a wash**; the extra version field erases the
  advantage.

So the earlier rejection was right for `sdi_forward` and *wrong in
general*: hashtables genuinely beats the HAMT for the simple-key table.
Not wired in this pass (measured, not landed) — a small contained swap for
`KeyIntern`, listed in open items.

### 4i — strictness audit, heap attribution, hashtables verdict (commits `799ce43`, `2692bc0`; `tried/hashtables-keyintern`)

**Job 1 — strictness audit: kept, but it found no live bug.** Audited every
`VM.write`/`VUM.write`/`writeIORef`/`modifyIORef` of a non-primitive across
`DefTable`, `SrcIndex`, `SimpleStateIf`, `OutputsMap`,
`PriorityAgingQueue`. Real gaps existed (`colWrite`'s `ColBoxed` path,
`sdiIntern`'s `dep`, bare `writeIORef` of computed `HashMap`/`IntMap`
updates, `enqueueRefs`' lazy tuple) and were fixed — but **every one was
already protected incidentally**: `Word32`/`Word64` take 4d's unboxed
path, and where boxed values do flow, an earlier `largeHash128` on the
same value forces it deeply as a side effect of hashing. A/B: flat within
noise on every metric. Kept as zero-cost hardening, since that protection
is an unenforced accident of call order — precisely 4h's bug class waiting
to recur.

Agent self-correction worth recording: it first "fixed" `OutputsMap`/`PAQ`
record fields believing them lazy, then found **`-XStrictData` is set
project-wide** and they were already strict; it reverted the change and
fixed its own misleading comments rather than leaving false claims in the
code.

**Job 2 — `+RTS -hT` heap attribution** (scale 0.25, cross-checked at 0.1;
percentages matched, so the split is stable). Of the ~94.5 MB live set:

| Category | % | Read |
|---|---|---|
| Arena/column payload (`ARR_WORDS`) | **68.3%** | exactly what the columnar design intends — unboxed columns, CSR arenas, hash index |
| Existential/`Typeable` cluster (`ForAnyCompFlow`, `TrApp`, dictionaries, `CompSrcId`, `Word128`, `Text`) | **18.1%** | scale-proportional, not CAF-like: the quantified price of 4h's deliberate choice *not* to intern `AnyCompSrcVer` — a documented tradeoff, not an oversight |
| Generic boxed thunks/closures | **11.6%** | **the biggest remaining reducible item**, unattributed |
| RTS bookkeeping / container internals | 1.8% | floor |

Explicitly confirmed: **param/value columns are not boxed in this
benchmark** — 4d's unboxed path covers `Word32`/`Word64`, so the boxed
residue is source-dependency existential wrappers, not param/value as the
roadmap had assumed. Limitation reported rather than hidden: `-hi` with
`-finfo-table-map` could not resolve addresses to source locations with
the available tooling, so the thunk cluster's origin is unpinned.

**Job 3 — `hashtables` for `KeyIntern`: tried, reverted, archived.**
Implemented cleanly, 145/145 green, and **flat within noise at every
metric** at both scales — despite 4h's standalone microbenchmark showing a
consistent ~35% win for this exact key shape. Root cause: `KeyIntern`'s
table holds one entry per distinct *source key* (300), small enough to sit
in cache under any implementation, and its op volume is a rounding error
against per-row work. **The microbenchmark did not transfer** — a useful
negative result, and a caution about sizing microbenchmarks by op count
rather than by working-set pressure.

### 4j — the 11.6% thunk cluster: understood, not a leak (no code change)

Pure diagnosis, and it dissolves the last open lead — **the 11.6% was an
artifact of how `-hT` classifies, not a distinct allocation source.**

`-hT` buckets by RTS closure *representation*. A typeclass dictionary
(`Show`/`Hashable`/`Eq`, bundled inside `ForAnyCompFlow`'s existential)
compiles to a FUN/PAP closure, not a CONSTR — so `-hT` necessarily
reported the dictionary payload of **the same existential cluster it
already named at 18.1%** as a separate, unlabeled "generic boxed thunks"
bucket. Re-sliced by cost centre (`-hc`, `-fprof-late`) and cross-checked
by type (`-hy`), the heap resolves with no residual mass: arena/column
62–70%, existential/dictionary cluster 25–31%, and a handful of named
items each under 3%.

Sampled across time in both phases (cold at 0.25; live at 0.1 with
`PERSIST_BENCH_LIVE_LOOPS=200`, ~2.1M reruns over 238 s) to separate
"growing" from "bounded". The live-phase heap is **essentially flat
(41.5 → 44.0 MB across 2M reruns)** — rows are being recomputed, not
created. Every non-arena item plateaus and stays flat: the ~4.9% per-call
`wrapCompSrcDep`/`compSrcId` reconstruction (4e's known *construction*
cost, never fully closed), MD5 scratch, `HashMapFlow` I/O,
`PriorityAgingQueue` nodes. The one item with any sustained growth
(+1.3 MB/150 s) was checked at the source: both `HashMapFlow` TVars are
bounded and drained every round — census noise, not retention.

Verdict: **(b) legitimate in-flight working state and (c) profiling
artifact — not (a) a leak.** Contrast with 4h's real bug, which announced
itself as a 10× time and 9× allocation blowup, not a diffuse few-percent
heap fraction.

Honest note on tooling: `-fprof-late` attached a spurious
`htf_..._thisModulesTests` ancestor frame to ~2.4% of live bytes in arena
call stacks. Traced and dismissed — the real call chain
(`withRow`/`withDefFor`/`mkSimpleCompEngineStateIf`) is production code and
nothing test-related runs in `bench` mode; it is GHC mislabeling a
floated binding with a name borrowed from a nearby QuickCheck property.
A naming coincidence, not a distinct source.

## Campaign closed — final scoreboard

| metric | Stage 0 (start) | final | vs Rust Stage 5 |
|---|---|---|---|
| cold eval | 42.8 s | **6.7 s** (6.4×) | 2.3× |
| live update | 24.4 s | **0.82 s** (30×) | **1.7×** |
| live heap | ~1,500 B/cap | **365 B/cap** (4.1×) | **1.11×** |
| RSS | ~2.6 GB | **751 MB** (3.5×) | 2.2× |
| tests | 70 | **145** | — |

Kept: 4a (`-A64m`), 4b (CSR edge arenas), 4c (open-addressed index),
4d (unboxed columns via `Typeable` dispatch), 4e (interned src-deps),
4f (**the O(n)→O(1) queue-size fix, 13× on live update**), 4g
(refcounted ids + coverage), 4h (columnar reverse index), 4i (strictness
hardening). Tried and reverted: `hashtables` for `KeyIntern`
(`tried/hashtables-keyintern`) — microbenchmark didn't transfer.

**The remaining gap is structural, and every piece of it is accounted
for**: GHC's copying-GC headroom over the live set (RTS-flag space:
`-F`, `--nonmoving-gc`, `-c` — surveyed in 4a, all lost on time);
the boxed existential/dictionary values 4h deliberately chose not to
intern (now confirmed to be the *entire* remaining non-arena mass); and
4e's bounded, non-leaking per-call `CompSrcId` reconstruction. No further
data-layout work reaches any of them.

Three findings outlived their stages and are the real transferable
lessons: **(1)** an asymmetry between two phases over identical data
structures means an algorithmic bug, not a constant factor — 4f;
**(2)** migrating from a `.Strict` container to a mutable boxed column
silently drops a strictness guarantee, and the resulting leak is
invisible to correctness tests — 4h; **(3)** per-call reconstruction of
"identity" values is invisible to a data-layout audit and can dwarf the
layout itself — 4e, where it cost 683× duplication.

## Not tried yet / open items

- **The Interlude-2 diet, measured.** The Rust notes list a ranked, concrete
  diet for this codebase (delete `sifs_vermap`; move `ccm_logrepr`/
  `ccm_approxCachedSize` to per-def `CompCacheBehavior`; drop the
  `OM.insert key mempty` for output-less caps; then interning + flattening the
  cache-value box tower) with a projected ~1,350 → ~565–720 B/cap. This
  benchmark is the before/after instrument for that work.
- RTS-flag sensitivity: `-F` (old-gen factor), `--nonmoving-gc`, `-c`
  (compacting), heap-size hints — the copying-GC multiplier is a large share
  of RSS and entirely untested.
- `+RTS -s` max residency vs. `max_live_bytes` cross-check; a heap profile
  (`-hT`) to attribute the 1.5 GB live set by closure type against
  Interlude 1's per-container tally.
- More 1.0 trials for variance on the live-update time.
- Timing attribution for the ~300–390 µs/rerun live-update cost (see
  suspects above).
- The persistence port itself (Interlude 3 sketches it: LMDB store, revival
  from `(Comp, typed param)`, STM-based debounced flush) — out of scope until
  the engine-only numbers are understood.

## Stage 5 — concurrent execution of applicative source batches (commits `b933dd1`…`71bf2a2`)

A different kind of change from Stages 0–4: not a diet or a constant-factor
squeeze over the existing engine, but a new capability — running the source
leaves of an applicative batch (`CompReqCombined`) concurrently, bounded by a
width knob, for sources that declare themselves safe for it. Off by default
(width 1 reproduces every prior stage's numbers exactly, unchanged code
path). Alongside it: a second benchmark purpose-built to exercise this (the
existing one structurally cannot, see below), an engine-state-lock
instrumentation flag, and a `stack bench` packaging fix.

### Before: `CompReqCombined` was fully sequential

`doSuspended`'s `CompReqCombined reqA reqB` case (`Impl.hs`) recursed into
`doSuspended` for each branch in turn — `resA <- doSuspended env reqA return;
resB <- doSuspended env reqB return` — sharing `env`'s dependency accumulator
but doing each branch's suspend/resume work strictly one after the other, on
the engine thread, regardless of what either branch actually was (a cap
eval, a cache lookup, a source read, a sink write). The code carried a literal
`-- TODO: Make this bit parrallel` at that case. A deeper batch (nested
`CompReqCombined`, e.g. a wide `traverse`-built applicative) recursed the
same way node by node.

### Design

**Declared per-source concurrency.** `CompSrc` gained a defaulted method,
`compSrcConcurrency :: s -> FlowConcurrency` (`FlowSerial | FlowConcurrent`,
default `FlowSerial`) — opt-in, not opt-out, so an existing instance is
unaffected until its author actively marks it safe. The three shipped
sources were marked `FlowConcurrent`, each justified from its own
`executeImpl`: `HashMapFlow` is a `readTVarIO` plus a pure lookup, `TimeSrc`
is a single `atomically` block, `FileSrc`'s only in-process state is the
`TVar`-guarded `FileWatch` — the read/stat pair around it is a filesystem
TOCTOU race that already exists single-threaded (the background poll thread
can change the file in between), so concurrency doesn't make it worse.

**A width knob on the registry.** `CompFlowRegistry` gained
`CompFlowConcurrency` (an `Int` wrapper floored at 1 by
`mkCompFlowConcurrency`) plus `setCompFlowConcurrency`/
`readCompFlowConcurrency`. Default width 1 — no worker pool, every leaf
inline — is exactly the historical behaviour. The width lives on the
registry rather than on `CompEngineIfs`/`RunCompEngineIf`/`compDriver`'s own
type specifically so nothing else has to change signature to thread it
through: `Impl.hs` already has the registry in hand at every flow-request
site, and `compDriver`/`compDriver'` already construct a fresh registry and
hand it to the caller's `withRegisteredFlows` callback before running
anything — so a caller sets the width from inside that same callback, no
driver fork required.

**Flattening a batch to its leaves.** `CompReqLeaf` (`CompLeafEval`/
`CompLeafCache`/`CompLeafSrc`/`CompLeafSink` — everything a `CompReq` can be
built from except `CompReqCombined` itself, which only nests) plus
`traverseCompReq :: Applicative f => (forall r. CompReqLeaf r -> f r) ->
CompReq a -> f a` walk a `CompReq` tree down to its leaves left to right in
one traversal, recombining through `f`, instead of the old node-by-node
recursion. Ordering is `Applicative`'s own left-to-right sequencing, not
anything `CompReq`-specific.

**`Prep`, a four-layer applicative built by hand** (`IO (SrcJobs, CompEngineM
(CompM a))`, not pulled from `Data.Functor.Compose` — a composition of
lawful applicatives is itself lawful, so `Prep`'s laws follow structurally):
the outer `IO` layer is where `prepSrcLeaf` decides, per source leaf, whether
its `compSrcExecute` becomes a queued job or still runs inline; `SrcJobs` (a
difference-list, `[IO ()] -> [IO ()]`, so a thousand-leaf batch's job list is
still built in one pass rather than quadratically re-copied) accumulates
queued work across leaves; `CompEngineM` is the engine-thread computation
(cache lookups, `compSrcExecute`/`compSinkExecute` calls, recursive cap
eval, or — for a leaf whose work was queued — reading that job's already-
finished result back out of its slot); `CompM` is the leaf's own deferred
result, combined via `CompM`'s own `compMAp` so a batch built this way keeps
`compMAp`'s left-error bias and "both sides always run" guarantee.

**Dispatch-then-drain, never overlapped with anything else.**
`dispatchJobs width jobs` (`ConcUtils.hs`) runs a fixed pool of at most
`width` workers — each pops the next job off a shared `IORef` queue via
`atomicModifyIORef'`, so a ten-thousand-leaf batch still spawns at most
`width` threads — built on `Async.replicateConcurrently_` (not a bare
`forkIO` per worker) so an asynchronous exception unwinding the caller tears
down every still-running worker with it, and blocks until every job has
finished before returning. The `CompReqCombined` case calls
`dispatchJobs` and only then runs `enginePhase` (the `CompEngineM` action
`Prep` built), for three reasons: `allCompSrcChanges` folds every source's
`compSrcWaitChanges` into one STM transaction on the engine thread and must
never race a concurrently-running `compSrcExecute`; a nested batch (an eval
leaf whose own body issues another wide batch) can't starve the pool, since
every worker from the outer `dispatchJobs` call has already exited by the
time `enginePhase` could recurse into `doSuspended` again; and reading a job
leaf's slot back can then never block, because the job that fills it has
unconditionally already finished. Each job is guarded by `trySync`
(`ConcUtils.hs`): catches a synchronous exception into the leaf's own slot,
but re-throws anything that `fromException`s to `SomeAsyncException` (so a
genuine `Async.cancel` reaching a worker still propagates instead of being
swallowed).

**A leaf becomes a job only when three things hold**: `width > 1`, the
leaf's resolved `CompSrc` instance reports `FlowConcurrent`, and
`rtsSupportsBoundThreads` (without real OS-thread concurrency, handing work
to the pool would only add `replicateConcurrently_`'s bookkeeping for no
actual overlap — width > 1 is then a no-op rather than a pessimisation).
Otherwise the leaf runs inline, byte-for-byte the same code path as before
`Prep` existed. The pre-existing two-leaf fast path (`f <$> a <*> b`, the
overwhelmingly common batch shape, kept as the old pre-`Prep` recursive code
rather than routed through `traverseCompReq`/`Prep` — measured at ~1.1M cap
evaluations to cost ~1.5–2% cold-eval wall time through `Prep` for no memory
change) is gated on `width == 1` (`7246a03`): at width 1 it is the *only*
path such a batch could take anyway, since no leaf could ever be dispatched
as a job regardless; above width 1 a two-leaf batch falls through to the
general `Prep` path so its source leaves can actually overlap.

**Why only source leaves move off the engine thread.** Everything else a
batch's leaves can be — cap evaluation, a cache lookup, a sink write — reads
or writes the engine's single mutable state (`SifState`/`DefTable`, guarded
by the one `MVar` `ssif_withState` takes — see the lock measurement below)
and stays on the engine thread inside `enginePhase`, unchanged. A source's
`compSrcExecute`, by contrast, is the one piece of work a `CompSrc` instance
can *itself* promise is safe to run overlapped with itself
(`compSrcConcurrency`) — the engine never has to reason about it, the source
author does, once, per instance. That is also why sink writes and cap
evaluation are explicitly not part of this change (see "What is not done"
below): nothing analogous to `compSrcConcurrency` exists for them yet, and
inventing it means reasoning about the shared engine state itself, not just
one instance's own I/O.

**Honest behavioural difference at width > 1**, documented on
`setCompFlowConcurrency`: if one leaf throws, every other leaf in that batch
has still had its `compSrcExecute` run by the time the exception is
observed — every queued job is dispatched and drained together before any
result is inspected. At the `Fail` level this already matches today's
behaviour (`compMAp` runs both sides of every applicative combination
unconditionally, by design). At the *exception* level it's new: at width 1,
no worker pool exists, so a throwing leaf aborts the batch immediately and
nothing after it runs. Both cases still surface only the leftmost failing
leaf's exception, matching `compMAp`'s left-error bias.

Tests: a new module, `TestCompFlowConcurrency.hs` (registry-level: blocking,
throwing, and overlap-counting `CompSrc` fixtures, exercised at several
widths), plus three invariant tests added to `TestCompReqCombined.hs` ahead
of this change (`B.1` wide-batch dedup, `B.2` a golden leaf-ordering trace
across source/eval/sink, `B.3` `compMAp`'s left-error bias) and a width-8
companion to `B.2` asserting only *source* trace entries may float relative
to the frozen golden order — everything else must not reorder. Full suite: **152 tests, all passing** (verified this session by running
`stack test`). Not re-derived as a before/after delta against Stage 4i's
145, the way earlier stages track it: this session did not capture a
same-commit baseline before this stage's first commit, and Stage 1's own
correction elsewhere in this doc is the standing reminder that a delta
computed across sessions rather than within one is not reliable enough to
report as a clean number.

### The engine-state lock, measured

`COMP_ENGINE_LOCK_STATS=1` instruments `setupSimpleStateIf`'s
`ssif_withState` (`Run.hs`) with a wall-clock timer around the whole
critical section (`withMVar lock $ \() -> ...`), read once at setup so the
choice between the plain and instrumented closure is baked in rather than
branched on every call — this is on the hottest path in the engine, millions
of calls per run. It reports **hold** time (time spent inside the lock),
not wait time — under today's fully-sequential `stepCompEngine`, calls into
`CompEngineStateIf` never contend, so wait time is definitionally zero and
hold time is the whole story. That stops being true the moment something
makes concurrent calls into the same state interface (a future
multi-threaded engine); at that point this number alone would understate
lock cost.

Existing benchmark, scale 1.0, default width (1):

| metric | value |
|---|---|
| lock acquisitions | 7,577,638 |
| total hold time | 4.731 s |
| cold eval | 6.757 s |
| live update | 0.818 s |
| hold time / (cold + live) | **62.5%** |

**What this does and does not mean.** 62.5% of measured wall time is spent
inside the single global engine-state lock — but that is time doing
state-layer *work* (cache reads/writes, dependency tracking, queue
operations), not lock *overhead*: an uncontended `withMVar` costs
nanoseconds, and nothing here makes the lock contended (there is exactly one
caller). It matters because it bounds what a future multi-threaded engine
could gain from running multiple cap evaluations concurrently, while that
work stays serialised behind one lock: by Amdahl's law, about **1.5x** — a
ceiling, not a target this stage's own change is trying to hit (this
stage's concurrency is entirely on the source-I/O side, which never touches
this lock while a leaf's `compSrcExecute` is actually running).

### The new benchmark, and why the existing one cannot measure this

The existing benchmark's bodies fan in dependencies with `forM_`/`mapM_`,
which for `CompM` discard results via `(>>)` — the ordinary monadic
`compMBind`, never the engine's own `<*>`/`compMAp`. It builds **zero**
applicative batches, so `CompReqCombined` never exists in its graph and its
width knob (wired for symmetry via `PERSIST_BENCH_CONCURRENCY`) is a
documented no-op regardless of value. Its source (`HashMapFlow`) also has no
latency to hide, so even a hypothetical batch would have nothing for
concurrency to overlap.

The Hospital benchmark (`bench/Control/Computations/Demos/Bench/Hospital.hs`,
`HOSPITAL_BENCH=1 stack bench`) exists specifically to fix both: it enables
`{-# LANGUAGE ApplicativeDo #-}` and fans in with `traverse` (never
`mapM_`/`forM_`), so independent binds inside one comp body desugar into a
real `<*>`-combined `CompReqCombined` batch; most source reads are
genuinely multi-key against the same source instance (`vitalComp` reads 3
keys, `labResultComp` 3, `medOrderComp` 2, `noteComp` 2), and
`patientSummaryComp` is a deliberate 8-leaf batch reading one key from each
of five separate source instances. `SystemSrc` (a `HashMapFlow`-shaped
source with a configurable per-call `threadDelay`, standing in for real
service latency, plus a call counter and a concurrency high-water mark)
backs five instances, one per simulated clinical system
(admissions/vitals/labs/pharmacy/notes). Graph: 1,000 patients across 20
wards at the default scale, ~976 instances/patient (an exact analytic
target, not sampled).

Full scale, latency 0, width 1:

| metric | value |
|---|---|
| instances | 976,063 (exactly the analytic target) |
| patients / wards | 1,000 / 20 |
| cold eval | 20.5 s |
| live update | 0.0071 s / 8 reruns |
| RSS | 5,265 MB |
| `max_live_bytes` | 3,957 MB |
| bytes/instance | 4,054 B |
| source calls | 1,612,013 (99.94% inside a dispatchable batch) |

For comparison, the existing benchmark, unchanged throughout (the
no-regression guard, not a measurement of this feature): ~1.14M instances,
cold ~6.5 s, live ~0.78 s, RSS 747 MB, `max_live_bytes` 365 MB.

### The width × latency grid

Scale 0.05 (50 patients), source latency 500 µs:

| width | cold wall | speedup |
|---|---|---|
| 1 | 109.1 s | 1.00x |
| 2 | 70.7 s | 1.54x |
| 4 | 42.6 s | 2.56x |
| 8 | 42.1 s | 2.59x |

The plateau at width 4 is the expected shape, not a limitation of the
dispatcher: the widest batch in this graph is 5 leaves
(`patientSummaryComp`'s cross-system read), and most batches are 2–3 leaves,
so width 8 has nothing left in any single batch to overlap.

At latency 0, width > 1 costs roughly **1.2x** — dispatch overhead
(`dispatchJobs`'s queue/pool machinery, the extra `IORef` slot per job) with
no latency for concurrency to hide.

### The source-key interning finding — a real opportunity, not a bug

The Hospital benchmark's memory tracks *source-call count*, not instance
count, at roughly **2,500 bytes per distinct source key** — very different
from the existing benchmark's per-instance figure, because of what the two
graphs' key distributions actually look like. The existing benchmark's
300-key `make_kv` is shared across all ~1.14M instances; the Hospital graph
deliberately never shares or pre-populates a key (every reading is its own
clinical fact), so its ~1.6M source reads intern ~1.6M genuinely distinct
keys. Each distinct key is interned **twice**: once in
`SrcIndex`'s `KeyIntern` (the reverse source-key index) and once again in
`DefTable`'s own per-def `SrcDepIntern` (the source-dependency table) — both
boxed `HashMap` entries over a `ForAnyCompSrcDep`/`AnyCompSrcKey`
existential. Both interning tables are correct, deliberate designs *for a
shared-key workload* (see Stage 4e/4h above) — they simply are not being
asked to do that here. That is why this benchmark needs ~4 GB where the
existing one needs 365 MB, and it is a real, identified optimisation
opportunity (deduplicating or restructuring how a genuinely-unshared key
gets interned across two independent tables), not a defect in this stage's
own code — `src/` is untouched by this stage.

### What is NOT done

- **Source-request deduplication and bundling.** Two leaves in the same
  batch requesting the same key against the same instance still run as two
  separate `compSrcExecute` calls (queued as two separate jobs, if both are
  eligible) rather than being coalesced into one. Bundling would need a
  batch-scoped identity for "same source, same request" recognized *before*
  `prepSrcLeaf` decides how to run each leaf — today each leaf is prepared
  independently, with no visibility into its siblings.
- **Sinks moving off the engine thread.** `doCompSinkReqValue` still runs
  `compSinkExecute` inline inside `enginePhase`, unconditionally. Nothing
  analogous to `compSrcConcurrency` exists on `CompSink` — adding one would
  need the same per-instance safety contract this stage built for sources,
  plus deciding how a sink's own output-tracking bookkeeping
  (`trackOutput`, `tellOutputs`) — currently engine-thread-only state
  writes — behaves when the write itself has moved off-thread.
- **Parallel cap evaluation.** Only source leaves — genuinely external I/O a
  `CompSrc` instance vouches for — ever leave the engine thread; a
  `CompLeafEval`/`CompLeafCache` leaf's cap evaluation always runs inside
  `enginePhase`, still serialized behind the single engine-state lock
  measured above. Running cap evaluations themselves concurrently would mean
  making `SifState`/`DefTable` safe for concurrent access (today they
  assume one caller, and the columnar mutable-column work of Stages 3–4 was
  built on that assumption) — the lock measurement above is exactly the
  ceiling (~1.5x) that work would be racing against, for a materially larger
  engineering cost than this stage's source-leaf dispatch.

### Tooling fixes landed alongside this stage

- **`stack bench` was broken for the published command.** The benchmark's
  `-with-rtsopts=-A64m` entry lacked `-T`, so `GHC.Stats.getRTSStats` threw
  before the memory section ever printed — a user-visible bug, since
  `-with-rtsopts=-A64m -T` cannot be two separate `ghc-options` entries (see
  the correction above the Stage 4a section: a second `-with-rtsopts=`
  entry replaces the first, it does not concatenate) and cannot be
  unquoted either (Cabal's `ghc-options` field word-splits on whitespace, so
  an unquoted `-A64m -T` becomes two GHC arguments and `-T` is rejected as
  unknown). Fixed with a single quoted entry,
  `'"-with-rtsopts=-A64m -T"'` (`0aca394`).
- **`HOSPITAL_BENCH`** dispatches `bench/Main.hs` between the two
  benchmarks (unset/`"0"` runs the existing one, still the default and the
  no-regression guard; anything else runs the Hospital benchmark). Never
  both in one process.

### Disposition: kept

Width 1 is the default and reproduces every prior stage's numbers exactly
(no code path changes at width 1 beyond the two-leaf fast path's new width
check, itself a no-op when width is already 1). The width knob is genuine,
measured speedup up to 2.6x on a workload actually shaped to use it, at a
bounded, honestly-reported cost (1.2x) when there is nothing to overlap.
The three explicitly-not-done items above are each a materially separate
piece of work, not oversights — deduplication needs batch-scoped leaf
visibility that does not exist yet, sinks need their own safety contract,
and parallel cap evaluation needs the engine's single state lock to stop
being single first, which is exactly what the lock measurement in this
stage quantifies the ceiling for.
