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

### Profiling the hospital benchmark — a verified recipe

Two approaches that look reasonable both fail here, and one plain command
works. All confirmed by actually running them (GHC 9.10.3, this machine),
not derived from the flags' documented behaviour:

- **`stack bench --profile` does not work.** It installs into the
  profiling install root but the resulting benchmark binary rejects `-p`
  outright: `the flag -p requires the program to be built with -prof`.
- **Adding `--ba "+RTS -p -RTS"` on top of `--profile` is also wrong**,
  not a fix — `--profile` already appends `+RTS -p -RTS` itself, so the
  duplicated `-p` makes the RTS print its help text and exit non-zero
  instead of running the benchmark.
- **What actually works: build into a separate work directory, then run
  the profiled binary directly**, bypassing `stack bench`'s runner
  entirely:

  ```
  stack build --profile --work-dir .stack-work-profile
  .stack-work-profile/dist/aarch64-osx/ghc-9.10.3/build/incremental-computations-bench/incremental-computations-bench +RTS -p -A64m -T -RTS
  ```

  The `aarch64-osx/ghc-9.10.3` path component is this machine's arch/GHC
  pair, not a portable path — expect it to differ on another machine or
  toolchain.

- **`+RTS -p` always writes `<program>.prof` into the current working
  directory, under a fixed name.** Consecutive profiling runs silently
  overwrite each other's output — this destroyed one run's results during
  this investigation. `cd` into a scratch directory before running, or
  rename the `.prof` file immediately after each run, before starting the
  next.
- **Profiling slowdown, measured on the hospital benchmark: cold eval
  20.5 s unprofiled → 211 s profiled, roughly 7–10×.** Budget accordingly
  before reaching for `+RTS -p`.
- **To weight a profile toward the rerun path instead of cold
  evaluation**, use `HOSPITAL_BENCH_RERUN_LOOPS` (see above). At
  `HOSPITAL_BENCH_RERUN_KEYS=4000 HOSPITAL_BENCH_RERUN_LOOPS=40`, the
  achieved split was **449 s rerun against 211 s cold — 68% rerun**.

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

## Stage 6 — source-request bundling's +10% peak, measured (commits `86733b0`,
`1b4dbee`, `51e78a7`) — investigated, not fixed

Bundling (dedup same-instance source requests within one applicative batch
via `compSrcExecuteBatch`, `SrcGroup`/`getOrCreateGroup`) raised the hospital
benchmark's `max_live_bytes` at full scale, latency 0, `-A64m`: **3,918.3 MB
→ 4,310.3 MB, +392,051,080 bytes (+10.01%)**, bit-identical across repeated
runs. This session's job was to confirm or refute a prior conclusion — that
the peak is structural to bundling because it materialises the whole `Prep`
tree, one closure per leaf, before any leaf runs — using `+RTS -hT` on a
normal (non-`-fprof-auto`) build instead of the `-hc`/reduced-scale profile
the prior conclusion rested on. Measurement only; nothing in `src/` or
`bench/` changed.

### `-hT` works on the production binary — no `-prof` build needed

The benchmark's `ghc-options` already carries `-rtsopts
"-with-rtsopts=-A64m -T"` (added for `getRTSStats`, Stage 5's tooling
fixes). That's sufficient for `-hT` (heap census by closure type): it needs
`-rtsopts`, not `-prof`, unlike `-p` or `-hc`/`-hb`. Recipe:

```
stack build incremental-computations:bench:incremental-computations-bench
cd <scratch dir>   # .hp lands in cwd under a fixed name, same trap as .prof
HOSPITAL_BENCH=1 <path-to>/incremental-computations-bench +RTS -hT -A64m -T -s -RTS
mv incremental-computations-bench.hp <renamed>.hp   # immediately
```

Full-scale cold eval: ~25 s unprofiled → ~113 s with `-hT` (**~4.5×**,
against `-fprof-auto`'s measured ~7–10×). Budget for it but it is far more
usable than a profiling build, and it profiles the actual binary being
shipped rather than a `-fprof-auto` distortion.

### Hypothesis 1 — sampling artifact: refuted

`max_live_bytes`/"maximum residency" is a running max updated only at
**major** GCs (`stat_endGC`'s `residency_samples` counter, confirmed
empirically: `+RTS -s`'s own `(N sample(s))` count only ever moves with
`Gen 1` collections, never `Gen 0`). Bundling's reported GC-count increase
(544→584, mostly minor `Gen 0` collections) does not by itself imply more
*residency* samples, and that turned out to be exactly right:

| build | `-A` | major-GC (residency) samples | `max_live_bytes` |
|---|---|---|---|
| pre-bundling (`71f9756`) | 64m | 11 | 3,918,275,064 |
| pre-bundling (`71f9756`) | 256m | 9 | 3,918,275,064 |
| post-bundling (`51e78a7`) | 8m | 16 | 4,310,326,152 |
| post-bundling (`51e78a7`) | 64m | 11 | 4,310,326,144 |
| post-bundling (`51e78a7`) | 256m | 9 | 4,310,326,144 |

Sample count varies 9→11→16 (a 78% swing) on the post-bundling side alone;
the reported peak moves by **8 bytes** — noise, not signal. On the
pre-bundling side, 9 vs. 11 samples: **bit-identical to the byte**. The peak
is a real, stable value the GC hits regardless of how densely it's sampled,
on both sides of the change. Hypothesis 1 does not survive.

### Where the +392 MB actually is (and isn't)

`GHC max_live_bytes` is read via `getRTSStats` *before* the benchmark's
phase 3 (rerun-heavy live update) ever runs, so it can only reflect phases 1
(cold eval) and 2 (single-key live update) by construction — not evidence
that phase 3 is irrelevant on its own. Cross-checked against the process's
*own* end-of-run `+RTS -s` dump (which does include phase 3): identical to
the byte in both builds, so phase 3's ~1 GB of further allocation never sets
a new peak either way. The peak belongs to cold eval.

More surprising: **the extra 392 MB is not a bigger steady state.** RSS
after cold settle is *lower* on the post-bundling build (4,926.1 MB vs.
5,058.2 MB, −132 MB), and RSS at the very end is lower too (7,048.5 MB vs.
7,065.6 MB). Bundling's dedup does what it's supposed to once everything
has settled. The +392 MB is a **transient peak reached and released during
cold eval**, not a larger resting footprint.

### Does the "materialised `Prep` tree" explanation survive? No — and the
depth-bound argument is what the data supports

`SrcGroup`/`SomeSrcGroup` (`Impl.hs`, new in `86733b0`, confirmed absent
from `71f9756`'s closure-type list entirely) are exactly the closures that
would hold per-batch bookkeeping if a whole tree were being materialised.
Their observed size, `-hT` envelope max across every sample of the entire
run: **`SrcGroup` 40 bytes, `SomeSrcGroup` 24 bytes.** The shared
`CompReq*` batch/tree types top out at `CompReqEval` 144 bytes,
`CompReqCombined`/`CompReqCache` 24 bytes, `CompReqFlow` 32 bytes — in
*both* builds, never growing past double digits to low hundreds of bytes at
any sampled instant. Nothing here is remotely close to hundreds of
megabytes. This is consistent with, and supports, the ~11-level nested-batch
depth bound: batch-tree bookkeeping genuinely never accumulates to a
material size in this benchmark. The literal "one closure per leaf of the
whole tree, materialised before any leaf runs" mechanism is not what's
retaining the extra memory.

### What the data cannot settle: which closure type actually holds it

This is the honest gap. `-hT`'s own closure-type census systematically
**undercounts** the RTS-tracked true peak for this workload, in both
builds, by a large and roughly similar margin:

| build | `-hT` census peak (single sample) | `-hT` envelope (per-type max, not simultaneous) | true `max_live_bytes` | census / true |
|---|---|---|---|---|
| pre-bundling | 3,435.1 MB | 3,453.2 MB | 3,918.3 MB | 88% |
| post-bundling | 3,441.3 MB | 3,455.3 MB | 4,310.3 MB | 80% |

The envelope-max diff across *every* closure type bucket is only **+2.1 MB**
— nowhere near the real +392 MB gap. Whatever holds the extra memory is
either (a) something the per-closure census structurally doesn't attribute
to a Haskell closure type (GC/block-level bookkeeping is the leading
suspect, unconfirmed), or (b) a spike brief enough that `-hT`'s own sampling
misses it in both builds alike. Two attempts to tighten this: `-i100`
(disable interval-based forcing, to piggyback purely on naturally-occurring
major GCs) produced only 2 samples, both empty — heap census is **purely
timer-driven**, it does not additionally census every natural major GC, so
this doesn't help. `-i0.02` (20× finer than the 0.1 s default) was tried and
aborted after 8 minutes without finishing — forcing a major GC on a
multi-GB heap dozens of times per second is not tractable. **Retainer
profiling (`-hr`) would attribute this precisely but requires a `-prof`
build**, reintroducing the exact distortion this investigation exists to
avoid; not attempted for that reason.

### Report

- **Real, not a sampling artifact** — confirmed by varying `-A` over 9/11/16
  major-GC samples on both sides of the change; the peak moves by ≤8 bytes.
- **Transient, not a permanent regression** — post-bundling's settled RSS is
  *lower* than pre-bundling's, both mid-run and at exit. The +392 MB is
  reached and released during cold eval.
- **Not the "materialised `Prep` tree"** — `SrcGroup`/`SomeSrcGroup`/`CompReq*`
  bookkeeping never exceeds a few hundred bytes at any sampled instant in
  either build. The ~11-level nested-batch depth-bound argument is what
  survives; the tree-materialisation explanation does not.
- **Not attributable to a specific closure type from available data** —
  `-hT`'s census undercounts the true peak by ~12–20% in both builds and its
  own per-type deltas (+2.1 MB) don't remotely explain the real +392 MB gap.
  Finer sampling was tried and is computationally infeasible here; retainer
  profiling would need `-prof` and was avoided for that reason. This is a
  genuine measurement dead end, not a hedge.
- **Removability: unknown.** It looks structurally unlike the previously
  suspected mechanism, and it disappears on its own by end of run — but
  without knowing what's retained, no removal path can be assessed
  responsibly. Next step, if pursued, is a `-prof`/`-hr` retainer census
  accepting the profiling distortion for this one question, or manual
  instrumentation around cold eval's tail.

## Stage 7 — `SrcKeyArena` small-size optimisation, measured — kept

The source-key-interning finding from Stage 5 ("~2,500 bytes per distinct
source key... a real, identified optimisation opportunity") pointed at two
tables: `DefTable.hs`'s forward-side `SrcDepIntern` and `SrcIndex.hs`'s
reverse-side `KeyIntern`/`SrcKeyArena` pair. This stage attacks the second
half — `SrcKeyArena` itself, not the interning table it hangs off of.

Before this change, every `SrcKeyArena` — one per distinct source key
currently depended on by at least one row — was unconditionally three
`IORef`s wrapping two growable vectors, regardless of how many dependents
that key actually had. That is an excellent trade for the existing
persistence benchmark's shape (300 keys sharing ~683 dependents each) and a
bad one for the Hospital benchmark's (~1.6M keys, ~1 dependent each — every
vitals/lab/pharmacy/note field is read by exactly one computation). The fix:
`SrcKeyArena` becomes a small-size optimisation over three states —
`SrcKeyZero` (no dependents), `SrcKeyOne` (a single `(DefRef, AnyCompSrcVer)`
pair held inline, no vector at all), and `SrcKeyMany` (the old
always-allocated structure, renamed `ManyArena` but otherwise unchanged).
Promotion is one-way: a key that reaches `SrcKeyMany` never demotes back to
`SrcKeyOne`\/`SrcKeyZero` on shrink (only tried and rejected — see
`Utils/SrcIndex.hs`'s own haddock for why), even though `SrcKeyOne` ->
`SrcKeyZero` on removal is free and always taken. No public API changed —
`SrcIndex.hs` is internal, not in `exposed-modules` — and all 153 existing
tests (including the per-dependent version-tracking test and the intern-table
churn regression tests) pass unmodified.

Both benchmarks, same machine, same session, stashed/popped for a true
before/after (not cross-session numbers):

| metric | existing bench (control) — before | after | hospital — before | after |
|---|---|---|---|---|
| `max_live_bytes` | 365.4 MB | 365.5 MB | 4,310.3 MB | **4,012.4 MB** |
| bytes/instance (`max_live_bytes` basis) | 365.5 B | 365.5 B | 4,416.0 B | **4,110.8 B** |
| `allocated_bytes` (cold eval) | 24,250.1 MB | 24,250.1 MB | 38,135.9 MB | 37,625.3 MB |
| `allocated_bytes` (live update, 1 key) | 3,043.4 MB | 3,043.5 MB | 0 | 0 |
| `allocated_bytes` (rerun-heavy, 400 keys) | — | — | 1,071.0 MB | 1,071.0 MB |
| `allocated_bytes` (rerun-heavy, 4000 keys) | — | — | 3,085.9 MB | 3,086.1 MB |
| cold wall (range, n=2–3) | 6.6–6.7 s | 6.7 s | 22.5–22.6 s | 17.2–18.0 s |
| live wall, 1 key | 0.81 s | 0.81 s | ~0.01 s (8 reruns) | ~0.01 s (8 reruns) |

**Existing benchmark (the control): no regression.** `max_live_bytes` moved
by +14,400 bytes (+0.004%) — one extra `IORef` indirection and pattern match
per operation on a workload that hits the `SrcKeyMany` path from each key's
second dependent onward, same as the old code from there on. `allocated_bytes`
for both phases is unchanged to within a few thousand bytes. This is exactly
the "must not regress" shape the control was chosen for.

**Hospital: `max_live_bytes` down 297.9 MB, −6.91%** (4,310.3 → 4,012.4 MB;
4,416.0 → 4,110.8 B/instance), reproduced bit-identically (`4,012,390,144`
bytes) across three separate runs, including with `HOSPITAL_BENCH_RERUN_KEYS
=4000`. Cold-eval `allocated_bytes` also drops, 38,135.9 → 37,625.3 MB
(−1.34%) — fewer bytes ever allocated, not just a lower peak. The
rerun-heavy phase's `allocated_bytes` is unchanged (as expected: re-triggering
an existing dependent only calls `skaUpdateVer`, never a representation
transition, on either version of the code). Wall time dropped more than the
15% noise floor this machine's timing can resolve (cold: 22.5–22.6 s →
17.2–18.0 s) but per this doc's own methodology note, that is reported, not
relied on — `max_live_bytes` and `allocated_bytes`, both deterministic here,
are the metrics this stage's conclusion rests on.

**A win, but smaller than the naive per-key arithmetic suggests.** Removing
two vectors' worth of allocation (each grown to an initial capacity of 4 on
first use) from ~1.6M single-dependent keys is a real, measured ~300 MB, not
the ~1.6M × (three-`IORef`-plus-two-4-slot-vectors) figure a back-of-envelope
calculation would suggest — most of a `SrcKeyArena`'s old footprint was the
per-key overhead (three separately-allocated `IORef` boxes plus two vector
wrapper records), which the `SrcKeyOne` case still pays *some* of (one
`IORef`, one constructor cell) even though it drops the vectors entirely; the
saving is the two vectors and two of the three `IORef`s, not the whole
structure. The remaining ~3.7 GB of Hospital's live set is unaccounted for by
this stage and was not investigated further here — Stage 5's other named
half of the same finding (deduplicating `SrcIndex`'s `KeyIntern` against
`DefTable`'s own `SrcDepIntern`, so a genuinely-unshared key isn't interned
twice) remains open, as does whatever `-hT` couldn't attribute in Stage 6.

**Demotion:** not implemented. `SrcKeyMany` never shrinks back down once
promoted, even to zero-then-rebuilt — the whole arena is discarded and a
fresh one created from `SrcKeyZero` in that case, but a key that drains from
many down to one (without hitting zero) keeps its `ManyArena` forever. This
was a deliberate choice, not an oversight: this codebase's own workloads
never exercise an oscillating-dependent-count key, so the added complexity
and repeated promote/demote allocation churn such a case would risk have no
demonstrated payoff here. If a future workload shows this pattern,
`Utils/SrcIndex.hs`'s own haddock documents exactly where a demotion path
would need to slot in.

### Disposition: kept

The control regressed by noise only, the target workload's `max_live_bytes`
dropped by a real, reproducible 6.9%, and the module's own test suite —
including the version-tracking and intern-churn tests this area's own
haddock calls out by name — passes unmodified. Smaller than hoped, given
where the remaining ~3.7 GB actually is, but a clean, low-risk win with no
observed downside.

## Stage 8 — the remaining ~3.7 GB, `-hT`-attributed: hypothesis confirmed, fix not contained (no code change)

Picking up Stage 7's open item: where the Hospital benchmark's remaining
live set actually is, using the Stage 6 `-hT` recipe (no `-prof`, so no
`-fprof-auto` distortion) against the current build (commit `01aef23`, tree
clean, branch `blog/optimizations-i-never-got-to`). Baseline reconfirmed
bit-identical before profiling: existing benchmark `max_live_bytes`
365,453,456 B (365.5 B/instance), Hospital `max_live_bytes` 4,012,390,144 B
(4,110.8 B/instance), cold `allocated_bytes` 24,250,124,904 B / 37,620,245,800
B respectively — all match Stage 7's reported figures to within session
noise, confirming a clean, unperturbed starting point.

### Recipe and honest caveats

```
stack build incremental-computations:bench:incremental-computations-bench
cd <scratch dir>
HOSPITAL_BENCH=1 <path>/incremental-computations-bench +RTS -hT -A64m -T -s -RTS
mv incremental-computations-bench.hp <renamed>.hp
```

Cold eval: 77.1 s profiled at full scale (vs. ~20–25 s unprofiled, ~3.3×;
consistent with Stage 6's "far more usable than `-p`, budget for it"
finding). Cross-checked at `HOSPITAL_BENCH_SCALE=0.1` (97,609 achieved instances) in a
separate run to confirm the split isn't a scale-specific artifact — see
"Stability across scale" below.

**A new timing finding, worth recording for future `-hT` work on this
benchmark**: the `.hp` file's `BEGIN_SAMPLE` clock is cumulative *mutator*
time, not wall-clock. At full scale the last real sample landed at
t=9.048 s against a final `+RTS -s` report of `MUT time 9.171s` — a 98.7%
match — while `GC time` alone was 71.857 s (the heap-census machinery
itself, at 88.7% of total wall time). Practical consequence: the `.hp`
sample count says nothing about wall-clock coverage, and a peak-by-total
sample picked from the file is a snapshot from *whenever the mutator had
run 9s of cumulative work*, not from a specific wall-clock instant — fine
for closure-type attribution (the question this stage asks), not fine for
correlating a sample against a specific phase by wall time.

### The closure-type breakdown at peak

Peak `-hT` census sample: 2,997.7 MB (full scale) — 78.3% of the true
`max_live_bytes` (4,012.4 MB), consistent with Stage 6's documented 12–20%
undercount for this workload.

| Closure type | Bytes (full scale) | % of census |
|---|---|---|
| `THUNK_1_0` | 810,987,120 | 25.80% |
| `CompFlow.ForAnyCompFlow` | 309,331,200 | 9.84% |
| `ARR_WORDS` | 280,605,208 | 8.93% |
| `FUN_1_0` | 232,273,440 | 7.39% |
| `GHC.Classes.C:Eq` (dictionary) | 231,936,312 | 7.38% |
| `GHC.Classes.CTuple4` | 193,280,200 | 6.15% |
| `Hashable.Class.C:Hashable` (dictionary) | 154,624,224 | 4.92% |
| `GHC.Internal.Show.C:Show` (dictionary) | 154,624,160 | 4.92% |
| `CompSrc.CompSrcId` | 115,978,776 | 3.69% |
| `HashMap.Internal.Leaf` | 103,178,848 | 3.28% |
| `IntMap.Internal.Bin` | 64,335,160 | 2.05% |
| `GHC.Types.I#` (boxed Int) | 60,904,384 | 1.94% |
| `ByteString.Internal.Type.BS` | 51,735,520 | 1.65% |
| `Text.Internal.Text` | 51,677,344 | 1.64% |
| `THUNK_2_0` | 51,238,720 | 1.63% |
| `CompSrc.Dep` | 38,733,936 | 1.23% |
| `IntMap.Internal.Tip` | 38,601,192 | 1.23% |
| everything else (116 more types) | 188,666,376 | 6.00% |

**Stability across scale.** Re-run at `HOSPITAL_BENCH_SCALE=0.1` (97,609
instances) reproduces the same shape closely: census/true ratio 78.5%
(vs. 78.3%), `THUNK_1_0` 25.73% (vs. 25.80%), `ForAnyCompFlow` 9.82% (vs.
9.84%), `ARR_WORDS` 9.06% (vs. 8.93%), `Eq`/`CTuple4`/`Hashable`/`Show`
dictionaries and `CompSrcId`/`Dep` all within 0.1 percentage points of the
full-scale figures. This is not a sampling fluke of one run; it's the
workload's actual shape.

### The denominator: confirmed to be source keys, not instances — and the intended machinery is a small minority

`ARR_WORDS` — the unboxed columns and CSR arenas that `DefTable`'s whole
design (Stages 3–4e) is built around, and that hold **68.3%** of the live
set on the *existing* benchmark's own `-hT` profile (Stage 4i) — is **only
8.9–9.1%** here. That inversion is the headline: on a shared-key workload
the columnar design works exactly as intended and dominates the heap; on
Hospital's genuinely-unshared-key workload, the columnar arenas are a small
minority and something else dominates. Everything else in the top of the
table — `ForAnyCompFlow`, its three-dictionary tax (`Eq`/`Hashable`/`Show`,
bundled as `CTuple4` for the `IsCompFlowData` constraint tuple),
`CompSrcId`, `Dep`, `HashMap.Leaf`/`IntMap.Bin`/`Tip` (the interning and
reverse-index containers), `Text`/`ByteString` (the key/value payloads
themselves) — is the *identity representation* of a source key/dependency,
not per-instance state. Summed, that identifiable cluster is **48.0% of the
census** (1,508,036,872 B); add `THUNK_1_0` (25.8%, almost certainly the
same family — see "What `THUNK_1_0` probably is" below) and it's **73.8%**.
Scaled to the true peak by the census/true ratio (×1.276): roughly
**1.8–2.8 GB of the 4.0 GB peak**, against 1,612,013 source requests
(essentially 1:1 with distinct keys per the brief) — **~1,190–1,840 bytes
of identity-representation overhead per key**, out of the observed
~2,490 B/key (`max_live_bytes` / source requests) noted in this
investigation's brief (which put it at "~2.5 KB per key" against the
1.6M-key count). This is the "wrong denominator" the brief predicted, now
measured rather than assumed: **the memory tracks distinct source
keys/deps, and specifically their boxed existential identity
representation, not instances and not the columnar arenas.**

### What `THUNK_1_0` probably is

Not directly attributable without `-prof`/`-hr` (the same wall this
investigation hit at Stage 6) — but circumstantial evidence points at the
same family as everything above it, not a separate leak. `THUNK_1_0` is a
generic single-pointer-field thunk, consistent with the unforced
intermediate closures a chain of `ForAnyCompFlow`-rewrapping calls
(`depKey`, `depVer`, `wrapCompSrcDep`) leaves behind before something
forces them, and with `Data.Text`/`Data.ByteString` key-construction
thunks upstream in `SystemSrc.hs`'s per-request `Dep key (fmap
largeHash128 mVal)`. It sampled at a near-identical percentage at both
scales (25.73% vs. 25.80%), which argues for "proportional to distinct
keys" over "growing leak" — consistent with Stage 4j's finding on the
*other* benchmark that a `-hT`-only investigation eventually hits an
attribution wall that only `-hr` (retainer profiling, `-prof`-only) can
cross. Not pursued further here for the same reason Stage 6 didn't:
`-prof` reintroduces the exact distortion (~7-10x) this recipe exists to
avoid, and the headline finding (denominator = source keys via boxed
existential identity, not instances) doesn't depend on resolving this.

### Step 2's hypothesis: confirmed, directionally — and the layering problem is worse than "an extra table"

The brief's hypothesis — `SrcIndex.hs`'s `KeyIntern` and `DefTable.hs`'s
`SrcDepIntern` both independently store a full boxed existential for what
is, for Hospital's unshared keys, the same identity — is **confirmed by
this profile**, not refuted. Tracing the actual call graph (not just the
type signatures) turns up a third copy the brief didn't name:
`SimpleStateIf.hs`'s `SrcKeyArena` (the *reverse*-index's own per-dependent
column) stores its own freshly-rewrapped `AnyCompSrcVer` per (key,
dependent) pair, via the same `depVer (ForAnyCompFlow i p d) = ForAnyCompFlow
i p (depVer d)` pattern that `depKey` uses (`CompSrc.hs`) — so it's a
triple, not a double: `KeyIntern`'s `AnyCompSrcKey`, `SrcDepIntern`'s
`AnyCompSrcDep`, and `SrcKeyArena`'s `AnyCompSrcVer`, three independent
`ForAnyCompFlow` boxes (each dragging its own `Eq`/`Hashable`/`Show`
dictionary bundle) for one logical (key, dependent, version) fact.

**Why no fix was implemented this session.** The brief's own proposed
direction — have `SrcDepIntern` store the already-interned `SrcKeyId` (from
the global `KeyIntern`) plus the version, instead of a whole
`AnyCompSrcDep` — requires the interning step to move from
`DefTable.writeSrcDeps`/`readSrcDeps` (today fully self-contained: no
dependency on `SrcIndex.hs`) to `SimpleStateIf.hs`'s `updateEdges`, the one
place both `DefTable` and the global `KeyIntern` (via `SifState`) are
already in scope — exactly as the brief anticipated. Two things, found only
by tracing the real call sites rather than reasoning from the type
signatures alone, make this bigger than "move a function call":

1. **A hard module-cycle constraint, not just a style preference.**
   `SrcIndex.hs` already imports `DefTable.hs` (for `DefRef`), so
   `DefTable.hs` cannot import `SrcIndex.hs`'s `SrcKeyId` back — GHC has no
   mutual-recursive-module story here without `.hs-boot` files. Hoisting the
   interning to `SimpleStateIf.hs` is therefore not optional convenience,
   it's the *only* direction that doesn't require restructuring the module
   graph, and it means `readSrcDeps`/`writeSrcDeps` can no longer take/return
   a self-contained `HashSet AnyCompSrcDep` — some caller-supplied,
   already-interned representation has to cross that boundary instead.
2. **`DefTable.hs`'s own test suite is built on exactly the signature that
   would have to change.** `DefTableTest.hs` constructs real `AnyCompSrcDep`
   values through a local `TestSrc` fixture with no access to a
   `KeyIntern`, and round-trips them through `writeSrcDeps`/`readSrcDeps`
   directly (`test_srcDepsRoundTrip`, `test_srcDepsDistinctValuesGetDistinctIds`
   — which specifically asserts that the *same key* at two *different*
   versions gets two distinct interned ids, both independently readable
   back out) and inspects `SrcDepIntern`'s own fields (`sdi_count`) as part
   of the assertions. Changing what `SrcDepIntern` stores means rewriting,
   not just re-running, a double-digit slice of that suite — the
   "intern-table churn regression tests... must keep passing" constraint is
   satisfiable, but "keep passing unmodified" is not, and reworking that
   suite correctly (preserving the refcounting semantics, the
   retain-before-release ordering hazard, and the distinct-version coverage)
   is real design and review work in its own right, not a mechanical
   follow-on.

Both `readSrcDeps` call sites in `SimpleStateIf.hs` (`freeRowCascade`,
reached via `updateEdges`'s `removeRdep` cascade, and `updateEdges` itself)
turned out, on inspection, to use
only `depKey` on the result and discard the version entirely — and
`notifyDepChange` (the function `test_modifcationWhileWorkingOnQueue`
actually exercises) reads per-dependent versions from `SrcKeyArena`, never
from `dt_srcDeps`. That's a genuine simplification opportunity, but not a
free one: `DefTable.hs`'s own module contract (its haddock and its test
suite) is written to guarantee full `AnyCompSrcDep` fidelity independent of
what today's two callers happen to do with it, so narrowing that contract
is itself the same "real architectural change" — just discovered from a
different angle than the layering issue above.

**A second, independent lever found via this profile, also not
implemented.** `CompSrcId` (115,978,776 B, 3.69% of census) is
reconstructed fresh on every `wrapCompSrcDep` call
(`compSrcId s = unTypedCompSrcId (typedCompSrcIdOf s)`, called once per
source request — ~1.6M times here) even though Hospital's whole graph uses
only 5 distinct `CompSrcId` values (one per `SystemSrc` instance).
`CompFlowRegistry.hs`'s `registerSrc`-shaped code already computes
`compSrcId src` once, at registration time — that value is never threaded
back down to where `wrapCompSrcDep` runs. A fix would cache a
`CompSrcId`/`TypedCompSrcId s` per registered instance and thread it
through `Impl.hs`'s and `CompFlowRegistry.hs`'s three `wrapCompSrcDep` call
sites instead of re-deriving it per request — additive to `CompSrc.hs`'s
API (no existing signature needs to change) and doesn't touch
`DefTable`/`SrcIndex` at all, but it does require plumbing a cached value
through the registry and the request-dispatch path in `Impl.hs` — its own
multi-file change, and one that needs care to confirm the cached value
can never drift from what a live instance would report, on a path that
runs once per source request.

### Disposition: not fixed — findings recorded, no code change

Per this investigation's own ground rule ("the bar for acting is a
number") and this session's explicit instruction to stop and report rather
than half-build a large or risky change: the hypothesis is confirmed, the
mechanism is understood down to specific closure types and call sites, and
two candidate fixes are scoped (the `SrcDepIntern`/`KeyIntern` unification
the brief proposed, and the independent `CompSrcId`-caching lever this
profile surfaced) — but both cross module boundaries this codebase
deliberately keeps acyclic, and the first also means reworking
`DefTable.hs`'s own test suite beyond "still passes unmodified". Neither is
a contained, single-sitting change with the confidence this investigation's
own standard demands. `src/` and `bench/` are untouched by this stage.
Numbers are therefore identical to Stage 7's: Hospital `max_live_bytes`
4,012.4 MB / 4,110.8 B/instance, existing benchmark 365.5 MB / 365.5
B/instance, both reconfirmed bit-identical at the top of this stage before
profiling began.

**If pursued**, the recommended order is the `CompSrcId`-caching lever
first — smaller blast radius, no test-suite rework, and it isolates
whether the registry-threading alone recovers a meaningful slice of the
3.69% (and whatever share of `THUNK_1_0` correlates with it) before
committing to the larger `SrcDepIntern`/`KeyIntern` unification, which
should be scoped as its own dedicated change with `DefTableTest.hs`'s
rework budgeted in up front, not discovered mid-implementation.

## Stage 9 — the `CompSrcId`-caching lever, implemented and measured (commit `b3231ae`)

Picked up Stage 8's recommended first move: `wrapCompSrcDep`
(`CompSrc.hs`) re-renders a `TypeRep` to `Text` (`identifyProxy`) on every
call, and it is called once per dependency, not once per distinct key —
~1.6M times on Hospital's cold eval, all landing on 5 distinct ids. Every
call site turned out to already have that id in hand for free, no new
plumbing required:

- `Impl.hs`'s `doCompSrcReq` and `prepSrcLeaf` both take a
  `TypedCompSrcId s` parameter that is *exactly* the id `withTypedCompSrcId`
  \/ `withTypedCompSrcIdIndexed` used to resolve the live instance — the
  registry only returns `Ok` when the requested key matches the key
  `registerCompSrc` stored the instance under (which is itself
  `compSrcId src`, computed once, at registration). So
  `unTypedCompSrcId srcId` (or `sid`) computed once per request\/leaf and
  reused across that request's whole `mapM_ (dependOn . wrapCompSrcDep ...)
  inputs` loop is, by construction, the identical value `wrapCompSrcDep`
  would have recomputed per element.
- `CompFlowRegistry.hs`'s `allCompSrcChanges` (the third call site Stage 8
  found) had the same shape one level up: `rs_srcs`'s own `HashMap` is keyed
  by `CompSrcId`, and `getChanges` was iterating `HashMap.elems`, throwing
  that key away before rebuilding it from the instance via `wrapCompSrcDep`
  over every changed dep in the set. Switched to `HashMap.toList` and
  threaded the key through instead.

**Shape chosen**: a new `wrapCompSrcDepWithId :: CompSrc s => Proxy s ->
CompSrcId -> CompSrcDep s -> AnyCompSrcDep`, with `wrapCompSrcDep` rewritten
as `wrapCompSrcDep s = wrapCompSrcDepWithId (Proxy @s) (compSrcId s)` — the
"one-off, does the derivation itself" case now written in terms of the
"caller already knows the id" case, not the reverse. The `Proxy` argument
(rather than none, or a live `s`) is load-bearing, not decorative:
`CompSrcKey`\/`CompSrcVer` are non-injective type families, so a signature
of just `CompSrcId -> CompSrcDep s -> AnyCompSrcDep` fails GHC's ambiguity
check outright (`s` appears only inside type-family applications) — the
`Proxy s` argument is what fixes `s` for the compiler, exactly the role
`unsafeMkTypedCompSrcId`'s own `Proxy` argument already plays. No live
instance needed passing through, since the id is precomputed.

`wrapCompSrcDepWithId` stays out of the public
`Control.Computations.CompEngine` facade (hidden alongside `compSrcId`,
same reasoning: it trusts the caller's `sid` unchecked, which is exactly
the kind of shortcut the facade doesn't want to encourage) — `compSrcId`,
`typedCompSrcIdOf`, and `wrapCompSrcDep`'s own signatures are all
unchanged, so this is a strictly additive change to the internal
`CompSrc` module's export list.

### The sink side: not fixed, and shouldn't be

Checked `wrapCompSinkOuts` (`CompSink.hs:279`) as the brief asked. It has
the identical shape (`compSinkId s` computed inline) but a different call
pattern: both its call sites (`doCompSinkReq`, `doCompSinkReqValue` in
`Impl.hs`) invoke it exactly **once per sink request**, wrapping the whole
output set in one `Map.singleton` — there is no per-element loop
comparable to `wrapCompSrcDep`'s `mapM_ ... inputs`. A sink request already
pays for a `TypeRep`-to-`Text` render once, the same as it always has; there
is nothing to amortize across, so a `wrapCompSinkOutsWithId` twin would add
API surface (another facade-hidden export, another haddock, another call
site to keep in sync) for a call count this change cannot reduce. Left
alone.

### `identifyProxy` itself: not memoized, deliberately

Considered caching `identifyProxy`'s `TypeRep -> Text` render globally
(e.g. a `Typeable`-keyed table behind `unsafePerformIO`), which would fix
every call site at once, including ones this stage didn't touch. Rejected:
per-type memoization in Haskell needs either a global mutable table keyed
on `TypeRep` (a `Data.Map`/`HashMap` behind an `IORef`, guarded with
`unsafePerformIO` — real global mutable state with its own GC-retention and
thread-safety story, for a codebase that otherwise has none) or a
`reflection`-style type-class dictionary trick, and `identifyProxy` is used
well outside this hot path (every `CompSrcId`\/`CompSinkId` construction
anywhere) where the one-render-per-registration cost is already
negligible. The targeted, call-site-local fix above gets the actual
measured win without introducing global state; not pursued further.

### Numbers

Same machine (Apple-silicon Mac, Darwin 25.5), same recipe as Stage 7\/8
(`stack bench`, `HOSPITAL_BENCH=1 stack bench`). Two reps per config
(cold-eval figures below are bit-identical run-to-run within a config, as
in every prior stage); RERUN_KEYS=4000 has one rep per side, noted as such.

| | Hospital `max_live_bytes` | Hospital B/instance | Hospital `allocated_bytes` (cold) | existing `max_live_bytes` | existing B/instance | existing `allocated_bytes` (cold) |
|---|---|---|---|---|---|---|
| before (Stage 7\/8, HEAD `d6d3810`) | 4,012.4 MB | 4,110.8 B | 37,625.3 MB | 365.5 MB | 365.5 B | 24,250.1 MB |
| after (this stage) | 3,909.2 MB | 4,005.1 B | 36,096.9 MB | 328.7 MB | 328.8 B | 24,049.7 MB |
| Δ | −2.57% | −2.57% | −4.06% | −10.06% | −10.06% | −0.83% |

Both benchmarks improved on both metrics — no regression on the existing
benchmark, which is the control's actual requirement. The existing
benchmark's movement is larger than the brief's own prior ("barely
exercises this path, expect little movement") anticipated: its 300 source
keys are distinct, but `wrapCompSrcDep` is called once per *request*, not
once per *distinct key*, and this graph's bottom levels (1,025,000 raw
def-slots before dedup, 999,760 achieved) generate close to a million
source-reading requests against those 300 keys — enough repeated-key
traffic for the per-call saving to compound. Hospital's cold eval — the
`~1.6M calls, 5 distinct ids` case the brief specifically targeted — shows
the larger absolute drop (1,528 MB of the ~37.6 GB allocated, 103 MB of the
~4.0 GB live) and confirms GHC was **not** already floating the
`identifyProxy` call away: the win materialized, exactly because the
existential `AnyCompSrc` blocks the specialization that would have let GHC
do this on its own (see `wrapCompSrcDep`'s haddock).

Live-update side (`HOSPITAL_BENCH_RERUN_KEYS` default of 400, two reps):
`allocated_bytes/rerun` dropped from ~348,971 B to ~327,107 B (−6.3%),
consistent with the same per-dependency saving applying to the rerun path
(reruns re-enter `doCompSrcReq`\/`prepSrcLeaf` exactly like cold eval).
At `HOSPITAL_BENCH_RERUN_KEYS=4000` (one rep per side) the same metric
showed no measurable change (150,417 B before vs. 150,425 B after) — most
plausibly single-rep noise rather than a real effect boundary, but not
re-verified with more reps in this session; flagged here rather than
smoothed over.

Wall times, both benchmarks: overlapping ranges session to session (Hospital
cold eval 13.2–13.4 s, existing-benchmark cold eval 5.46–5.77 s, before and
after), consistent with this doc's standing note that wall time on this
machine has too wide a range to judge a change by — not used as evidence
either way here.

### Correctness

The ids `wrapCompSrcDepWithId` now tags dependencies with are the *same
value*, not just an equal one: the registry only ever resolves a request
successfully when the caller's key matches the key `registerCompSrc` stored
the instance under (`compSrcId src`, computed once at registration), so
`unTypedCompSrcId srcId`\/`sid` at every one of this stage's call sites is
that exact stored value, not a re-derivation that merely happens to compare
equal. `stack test --flag incremental-computations:werror` — 153 tests,
including the `debugSrcKeyInternLiveCount`\/`debugTotalSrcDepInternLiveCount`
churn tests and `TestCompReqCombined.hs`'s golden ordering trace (left
untouched) — passes unmodified.

## Stage 10 — the `-hb` biography, `depKey`/`depVer` reboxing, and a mostly-null result

A `-hb` biography profile of Hospital (profiling build, `PERSIST_BENCH_SCALE=0.1`)
put peak heap at 254.3 MB **VOID** (allocated, never read at all) out of
303.9 MB total — 83.7%, dwarfing `INHERENT_USE` (31.5 MB) and `DRAG`
(18.0 MB) combined. `-hc` restricted to VOID+DRAG localised 70% of it to
three cost centres: `depVer`\/`updateEdges` (67.6 MB, 24.8%), `evalCompAp.
prepSrcLeaf` (67.4 MB, 24.8%), `depKey`\/`updateEdges` (56.4 MB, 20.7%).
`-hy` on the same selection: `ForAnyCompFlow` 29.5 MB, `Eq` 22.1, `CTuple4`
18.4, `Show` 14.7, `Hashable` 14.6, `CompSrcId` 7.4 — i.e. the existential's
own dictionary bundle (`ForAnyCompFlow c i k`'s packed `(Typeable s, c s,
IsCompFlowData (k s))` context — `IsCompFlowData` is exactly `(Show, Eq,
Typeable, Hashable)`, a `CTuple4` constraint tuple), not the payload,
dominates the byte count.

### The mechanism, and what turned out to be two different things

`CompSrc.hs`'s `IsDep AnyCompSrcDep` instance:

```haskell
instance IsDep AnyCompSrcDep where
  depKey (ForAnyCompFlow i p d) = ForAnyCompFlow i p (depKey d)
  depVer (ForAnyCompFlow i p d) = ForAnyCompFlow i p (depVer d)
```

allocates a fresh `ForAnyCompFlow` existential *every call*, dictionary
bundle and all — a rebox of a value that already exists, purely to
project out its key or version as a same-shaped existential. `SimpleStateIf.
hs`'s `updateEdges` haddock (written for Stage 3's `9fe2db3`-era by-key
diff) already reasoned carefully about this cost for the *new*-side of the
diff (`classifySrcDep`, one `depKey`/`depVer` per `newSrc` element,
correctly called "unavoidable"), but the *old*-side still built
`oldSrcKeys = HashSet.map depKey oldSrc` — one fresh `AnyCompSrcKey` rebox
per *old* dependency, for a comparison set where the overwhelmingly common
case (same key, new version — the whole reason this by-key diff exists)
only ever probes it, never otherwise uses the result.

`prepSrcLeaf`'s 24.8% (`Impl.hs:756`, `mapM_ (dependOn . wrapCompSrcDepWithId
(Proxy @s) cid) inputs`) turned out to be a **different mechanism**, not
the same one reached from another angle: Stage 9 already eliminated the
redundant `TypeRep`-to-`Text` render at this call site (passing the
group's `cid` once instead of re-deriving it per dependency), but the
`ForAnyCompFlow` construction itself is not a rebox of an existing
`AnyCompSrcDep` — it's the *first* wrap of a freshly-returned, still-typed
`CompSrcDep s` (from `compSrcExecute`) into the erased representation
`dependOn` needs to store it. There is no already-built `AnyCompSrcDep`
here to share; every dependency the source just reported is, definitionally,
a new value. Nothing in this stage touches `prepSrcLeaf` as a result — the
one-time construction cost is intrinsic to the existential boundary, not
waste.

### The fix: compare by key without reboxing

`AnyCompSrcDepByKey` (`CompSrc.hs`), a zero-cost newtype over `AnyCompSrcDep`
with `Eq`\/`Hashable` that read the id and the underlying `Dep`'s key field
directly (`Eq` reuses `applyIfEqualIds` — the same id-then-cast dance
`ForAnyCompFlow`'s own `Eq` instance already uses, just comparing `dep_key`
instead of the whole value; `Hashable` pattern-matches once and hashes id
+ key, skipping the version). `updateEdges` now builds `oldSrcByKey =
HashSet.map AnyCompSrcDepByKey oldSrc` — wrapping, not reboxing, every
`oldSrc` element — and `classifySrcDep`'s membership test probes that set
with `AnyCompSrcDepByKey dep` directly. A real `AnyCompSrcKey` is still
built exactly where it was already unavoidable: once per `newSrc` element
(for the two mutation calls), and now additionally once per element of the
*removed* set at the very end (typically small, not full-`oldSrc`-sized).
Net effect: the old side of the diff goes from *N* `AnyCompSrcKey` reboxes
to zero, at the cost of nothing new. `depVer` is untouched — it was already
minimal, one call per `newSrc` element, matching the existing haddock's
reasoning.

### Numbers — and the headline metric didn't move

Same machine, same recipe as Stages 7–9. Two same-session reps each side
(`HOSPITAL_BENCH=1`, default `HOSPITAL_BENCH_RERUN_KEYS=400`), plus one rep
at `RERUN_KEYS=4000`, plus the control (`stack bench`, scale 1.0).

**Hospital:**

| | `max_live_bytes` | B/instance | `allocated_bytes` (cold) | `allocated_bytes`/rerun (400 keys) | `allocated_bytes`/rerun (4000 keys) |
|---|---|---|---|---|---|
| before (HEAD `ce6fe80`) | 3,909,222,144 (3909.2 MB) ×2 reps, bit-identical | 4,005.1 B | 36,096.9 MB (36,096,881,968 / 36,096,888,000) | ~327,107 B (327,106.1 / 327,107.7) | 150,424.9 B |
| after (this stage) | 3,909,222,144 (3909.2 MB) ×2 reps, bit-identical | 4,005.1 B | 36,028.7 MB (36,028,709,200 / 36,028,714,128) | ~327,034 B (327,034.3 / 327,034.5) | 150,409.2 B |
| Δ | **0.00%** | 0.00% | **−0.19%** | −0.022% | −0.010% |

`+RTS -s` on the full run (cold + live + rerun-heavy) agrees: total bytes
allocated in the heap 37,196,157,312 → 37,155,179,920 (−0.11%), bytes
copied during GC 14,496,035,400 → 14,493,400,104 (−0.018%, noise-level),
**maximum residency identical to the byte, 3,909,222,144, both sides,
11 samples each** — GHC's own residency sampler agrees with `GHC.Stats`'s
`max_live_bytes` exactly.

**Existing (control), `stack bench`, scale 1.0:**

| | `max_live_bytes` | B/instance | `allocated_bytes` (cold) |
|---|---|---|---|
| before | 328,690,680 (328.7 MB) | 328.8 B | 24,049,698,656 (24,049.7 MB) |
| after | 328,689,264 (328.7 MB) | 328.8 B | ~24,048.8 MB (24,048,770,664 / 24,048,772,072) |
| Δ | −0.0004% (noise) | flat | −0.004% |

No regression on the control — the requirement — and if anything both its
numbers moved a hair in the same direction as Hospital's, consistent with
this path being exercised (at much lower relative weight, 300 distinct
keys vs. Hospital's larger per-row dependency sets).

### Verdict: real, but not the win the profile promised

This is **not** the "twice today a change looked real in profile and was
an exact no-op in production" pattern from earlier in this campaign —
`allocated_bytes` genuinely, reproducibly drops (−0.19% Hospital cold,
consistent across reps and corroborated independently by `+RTS -s`'s own
counters). The rebox the profile pointed at is real and this stage removes
it. But it is also **not** the memory-roadmap win the 124 MB of combined
VOID/DRAG (`depKey`+`depVer`\/`updateEdges`) suggested: `max_live_bytes` —
the metric every stage in this doc's memory roadmap actually tracks — is
**bit-identical**, both by `GHC.Stats` and by the RTS's independent
`-s` residency sampler, at both benchmarks.

Read together, the honest explanation is that the reboxed `AnyCompSrcKey`
values were short-lived nursery garbage — dead well before any GC's
residency sample could ever catch them, allocation churn and copying-GC
pressure, not peak occupancy. Removing them lowers total allocation (a
real cost: alloc-rate-bound code pays for it in `MUT` time and minor-GC
frequency, `534` Gen 0 collections vs `535` here) but was never going to
move `max_live_bytes`, because it was never part of what got sampled as
live. The profiling build's `-hb` biography, built without the
optimizations `-fprof-auto` disables, cannot distinguish "large but
transient churn" from "large and actually retained" the way `GHC.Stats`'s
production sampling does — a caveat this doc's own "confirm against
production" rule exists precisely to catch, and did.

**Disposition: kept.** Non-regressive on the control (the hard
requirement), a real and reproducible small win on total allocation with
no measurable downside, correctly scoped (adds one newtype and its two
instances, touches one function's internals, no public API change), and
directly continues this campaign's established practice of not leaving a
known, provably-real redundant allocation in place merely because its
impact turned out smaller than the profile implied. `prepSrcLeaf`'s 24.8%
is left alone, having turned out to be a different, non-redundant
mechanism (see above) — not a follow-up target.

### Correctness

`AnyCompSrcDepByKey`'s `Eq` reuses `applyIfEqualIds`, the exact machinery
`ForAnyCompFlow`'s own `Eq` instance is built on, so the id-then-cast
discipline that makes that instance sound applies here unchanged; `Hashable`
reads the same `dep_key` field `Eq` compares against, so the two instances
agree on one equivalence relation (by id and key, ignoring version) as
required. The version each dependency carries is never dropped or altered
anywhere in this change — `AnyCompSrcDepByKey` is used only to decide *set
membership*, never written back, read for logging, or substituted for a
real `AnyCompSrcVer`/`AnyCompSrcKey` at any call site that needs one.
`stack test --flag incremental-computations:werror` — 153 tests, including
`test_modifcationWhileWorkingOnQueue` (per-dependent version tracking) and
the intern-table churn tests — passes unmodified; `TestCompReqCombined.hs`'s
golden ordering trace was not touched.

## Stage 11 — info-table profiling: the recipe works, the coverage does not (no code change)

`THUNK_1_0` has been the largest single closure type in the Hospital heap
since Stage 8 measured it at 810,987,120 bytes, 25.80% of a production-build
`-hT` census, against `ARR_WORDS` -- the unboxed columnar payload the whole
`DefTable`/`SrcIndex` design exists to hold -- at 8.93%. Three separate
attempts to localise it failed, all for the same reason: every tool that maps
heap to source needs `-fprof-auto`, and on this codebase `-fprof-auto`
attribution has been directionally useful and **wrong about magnitude every
time** (Stages 6, 8 and 10).

Info-table profiling is the first tool tried here that does not have that
flaw. `-finfo-table-map` emits an address-to-source map for every info table
at compile time, and `+RTS -hi` censuses the heap by info table, both on an
ordinary `-O2` build with no profiling runtime.

### The recipe

Build the benchmark with `-finfo-table-map` (`-fdistinct-constructor-tables`
separates constructor allocations that would otherwise share one table), into
a work dir of its own so the normal build is not clobbered. Run with **both**
`-hi` and `-l`: `-hi` writes the census to `<program>.hp`, but the
address-to-source map is emitted into the *eventlog*, so without `-l` the
`.hp` is a list of bare hex addresses and nothing can be resolved. Read the
map back with `ghc-events show <file>.eventlog`, which prints lines of the
form:

```
Info Table: 100707a48:16:sat_sDKc_info - src/Control/Computations/Utils/SourceLocation.hs:41:1-52
```

Then join: parse the largest `BEGIN_SAMPLE`/`END_SAMPLE` block out of the
`.hp`, strip the `0x` from each entry's address, and look it up in the map.
NOTE: the join is the part worth writing down. The `.hp` and the eventlog are
separate artifacts with no cross-reference, addresses appear as `0x…` in one
and bare hex in the other, and neither file is useful alone.

NOTE: `.hp` and `.eventlog` are fixed filenames written into the working
directory, exactly like `.prof` -- consecutive runs overwrite each other
silently. Run from a scratch directory. This has destroyed one
investigation's results already (Stage 6).

### The result: 6.7% attributed

Peak census sample 3,040.2 MB, of which **203.2 MB (6.7%) resolved to a local
source location and 2,837.0 MB (93.3%) did not**, against 29,429 info-table
map entries. Top attributed sites:

| Bytes | % of peak | Site |
|---|---|---|
| 77.1 MB | 2.54% | *(map entry present, empty source span)* |
| 51.6 MB | 1.70% | `Impl.hs:(798,5)-(901,51)` |
| 38.6 MB | 1.27% | *(empty source span)* |
| 25.8 MB | 0.85% | *(empty source span)* |
| 4.0 MB | 0.13% | `Utils/Fail.hs:411:32-65` |

Only code compiled with `-finfo-table-map` gets entries, and the Stackage
dependencies were not. So the 93.3% is `containers`, `vector`, `hashable`,
`bytestring` and the RTS -- precisely where a `HashMap`/`IntMap`/boxed-vector
heavy engine would be expected to hold its bytes. **The tool works; the
coverage is what fails.** At local-package scope this cannot answer the
question it was reached for.

Settling it means rebuilding every dependency with the flag --
`ghc-options: {"$everything": -finfo-table-map}` in `stack.yaml` -- which is
a full world rebuild. Not attempted here; recorded as the one remaining lever
that would actually resolve `THUNK_1_0`.

### One real finding, measured as nothing, reverted

The attributed `Impl.hs` span led to a genuine strictness hole in
`updateEdges` (`SimpleStateIf.hs`): `foldM` is not strict in its accumulator,
so `(HashSet.insert byKey) <$> classifySrcDep dep` chains one unforced
`HashSet.insert` thunk per element of `newSrc`, held until
`HashSet.difference` finally demands the result. Forcing the accumulator with
a bang is a five-line change.

Measured on the production build:

| | Hospital | existing (control) |
|---|---|---|
| `max_live_bytes` | 3,909,222,144 -> 3,909,222,144 (**bit-identical**) | 328,689,264 -> 328,689,264 (**bit-identical**) |
| `allocated_bytes` (cold) | 36,028.7 -> 36,161.6 MB (+0.37%) | 24,048.8 -> 23,982.2 MB (-0.28%) |

Peak did not move at all, and both allocation deltas sit inside the ~0.27%
run-to-run band Stage 7 measured for `allocated_bytes`. The reason is
structural: those thunks live and die inside a single `updateEdges` call, so
no heap census ever sees them -- the same reason Stage 10's reboxing fix cut
allocation without touching peak.

Reverted, per this document's own rule: no improvement in a step means mark
it tried and revert. The laziness is real and would chain proportionally to
the size of a row's source-dependency set, so it is worth revisiting if a
graph with wide per-row source-dep sets ever shows up -- this one averages
about 1.65, which is why it is invisible.

### Disposition

No code change. The recipe and its coverage limit are the deliverable: the
next person to chase `THUNK_1_0` should start by rebuilding dependencies with
`-finfo-table-map` rather than repeating a local-scope profile.

## Stage 12 — the tiered benchmark: heterogeneous latency does not re-rank bundling against concurrency

Two structural gaps in the scale and Hospital benchmarks made a question
unanswerable. Both graphs are **stratified** -- leaves read sources,
interior computations read only other computations, and exactly one body in
either mixes the two -- so a batch almost never contains both leaf kinds,
and the engine's dispatch-then-drain choice (Stage 5) could not cost
anything. And both use **one global latency** for every source, so no batch
is worth more than any other.

`bench/Control/Computations/Demos/Bench/Tiered.hs` keeps Hospital's
skeleton for comparability and changes exactly those two things: interior
bodies read sources in the same applicative batch as their computation
dependencies, and each source carries its own latency tier. Risk scoring
reads a single model-coefficients key shared by every patient, exercising
the `SrcKeyMany` path that Hospital -- where nearly every key has one
dependent -- never touches.

### `threadDelay` cannot express sub-millisecond latency here

Discovered while validating the tiers, and worth recording on its own.
`threadDelay` floors at about 1.28 ms on this machine regardless of the
delay requested, so the intended 25/100/250/500/1000 us tiers were all
identical. It is **not** the RTS timer tick -- neither `-V0.0001` nor `-V0`
moves it.

| requested | `threadDelay` | safe-FFI `usleep` | busy-wait |
|---|---|---|---|
| 25 us | 1159 | **47** | 30 |
| 100 us | 1180 | **133** | 100 |
| 250 us | 1189 | **319** | 257 |
| 500 us | 1226 | **636** | 500 |
| 1000 us | 1248 | **1288** | 1000 |

Overlap, 8 threads x 50 x 1000 us, where fully serial is ~400 ms:
`threadDelay` 58.6 ms, safe-FFI `usleep` 62.6 ms, busy-wait 150.1 ms.

Busy-wait is the most accurate and the least usable: it holds a capability
while spinning, so eight spinners on `-N4` serialise. `unsafe` FFI is as
accurate as `safe` but also never releases the capability. **Safe-FFI
`usleep` is the only option that gives both resolution and overlap**, at
about 25% overhead with a floor near 20 us (commit `8f62218`).

### Shape

180 patients, 4 wards, **175,695 instances against an analytic target of
175,695** (+0.00%), depth spanning levels 1-11 (106,020 instances at level
1 thinning to a single root). `defaultScale = 0.18`, re-derived after the
`usleep` swap -- 2.25x more patients than the `threadDelay` floor allowed
in the same wall time.

### The matrix, latency multiplier 1.0

Cold eval wall time, seconds. Bundling-off rows are medians of three; a
first-run 109.4 s at width 4 did not reproduce (90.3, 91.9) and was noise.

| bundling | width 1 | width 2 | width 4 | width 8 |
|---|---|---|---|---|
| on | **61.0** | 62.2 | 62.7 | 61.7 |
| off | 92.1 | 91.9 | 91.9 | 93.0 |

Overhead control at multiplier 0: bundling on 2.45 s / off 2.47 s at width
1; both about 2.8 s at width 4.

### Findings

**Bundling is worth 1.51x** (61.0 vs ~92) and is the only lever that moves
anything.

**Concurrency contributes nothing, at any width, with or without
bundling.** This is the second measurement of that result -- Stage 5 found
the width grid flat under *uniform* latency, and the hypothesis was that a
slow source among fast ones would re-rank the two. It does not.

The reason is a design consequence, not an artifact. Dispatch groups source
leaves **by instance and runs one worker per group**, so same-instance
leaves are serialised by construction; only batches spanning *several*
instances can overlap. In this graph each body reads one source
(`vitalWindowComp` vitals, `interactionComp` pharmacy, `riskScoreComp`
labs, `wardCensusComp` adt); only `patientSummaryComp` reads all five, once
per patient against roughly 1,935 batch calls per patient. There is
essentially nothing to overlap, and heterogeneous latency does not create
any -- it changes what a batch *costs*, not how many instances it spans.

**Dispatch-then-drain therefore costs nothing measurable here**, which is
the question this benchmark was built to answer -- but for a duller reason
than expected. The barrier can only cost what overlapping would have saved,
and overlapping saves nothing on this shape. Mixed leaves were necessary to
make the question askable and turned out not to be sufficient: the binding
constraint is instances-per-batch, not leaf kinds per batch. At multiplier
0 the dispatch machinery costs about 12% (2.45 -> 2.75 s) purely in
overhead.

### Disposition

The `FlowConcurrency` declaration and the `CompFlowConcurrency` width knob
are now measured as dead weight on two independent realistic graph shapes,
under both uniform and heterogeneous latency. They cost a public class
method, a registry field, and a dispatch path. Worth deciding whether they
earn that; bundling subsumes them for every workload measured so far.

## Stage 12a — correction: the benchmarks had no `-N`, and it does not matter here

Stage 12's grid was measured with the `benchmarks:` stanza set to
`-with-rtsopts=-A64m -T` — **no `-N`** — while the `tests:` stanza has always
had `-with-rtsopts=-N`. Some cells were run with an explicit
`--ba "+RTS -N -RTS"` and some were not, so the grid was confounded. Commit
`3f1d170` puts `-N` inside the stanza's single quoted token and makes all three
benchmarks print `getNumCapabilities`, verified on the built binary rather than
the cabal file (`+RTS --info` reports `("Flag -with-rtsopts", "-A64m -T -N")`;
this machine gives 14 capabilities).

### Stage 12's conclusion survives, re-measured with every cell identical

Cold eval, seconds, two reps per cell agreeing within 0.5%:

| bundling | width 1 | width 2 | width 4 | width 8 |
|---|---|---|---|---|
| on | **71.4** | 74.2 | 74.2 | 74.1 |
| off | 108.9 | 111.3 | 111.4 | 111.3 |

**Bundling 1.53x** (Stage 12 measured 1.51x). **Concurrency still contributes
nothing at any width**, and width 1 -> 2 costs about 4%. The explanation stands:
dispatch groups source leaves by instance and runs one worker per group, so
same-instance leaves serialise by construction and only multi-instance batches
can overlap — which this graph almost never produces.

### `-N` does nothing for this benchmark, and that is expected

Same-session sweep at bundling on, width 1:

| `-N1` | `-N4` | `-N8` | `-N14` |
|---|---|---|---|
| 72.5 s | 72.1 s | 71.3 s | 71.5 s |

Flat. The workload is blocked in safe-FFI `usleep`, and a `safe` FFI call
already releases its capability at `-N1` — that is exactly why safe-FFI was
chosen over busy-wait in Stage 12. Extra capabilities have nothing to do until
something makes the *engine* concurrent.

NOTE: the grid above reads ~17% slower than Stage 12's (71.4 vs 61.0 at
bundling on, width 1). That gap is **cross-session drift, not an `-N` effect** —
the same-session sweep is flat across every setting. An idle-capability-spinning
hypothesis was raised and refuted by that sweep. Wall-time numbers on this
machine are only comparable within a session; the ~2x range documented in
Stage 5 applies across them.

### `max_live_bytes` is `-N`-dependent — baselines are not portable across settings

| | at `-N1` | at `-N14` | |
|---|---|---|---|
| persist `max_live_bytes` | 328,689,264 | 363,080,112 | **+10.5%** |
| hospital `max_live_bytes` | 3,909,222,144 | 3,909,375,296 | +0.004% |

Not a regression — a different measurement configuration. At `-N14` the
aggregate nursery is 14 x `-A64m` ~= 896 MB, which swamps persist's 363 MB live
set and is negligible against hospital's 3.9 GB. Any future "bit-identical"
memory claim must name the capability count it was measured at.

Wall time improved on both from parallel GC: persist cold 6.5 -> 5.7 s, hospital
14.5 -> 10.5 s. An earlier `-N` sweep also showed hospital's very short
live-update phase getting ~4x *worse* (0.0062 -> 0.0268 s), where parallel GC's
fixed per-collection sync cost dominates.

## Stage 13 — the tiered plateau: measuring where the parallelism goes

The tiered benchmark's cold eval goes 60.5 s at eval-concurrency width 1 to
18.4 s at width 8, then plateaus (width 16 measures 18.34 s -- statistically
identical to width 8). Two explanations were already ruled out by
measurement before this stage: **not permits** (width 16 offers double the
pool, per-source concurrency high-water marks do reach 16, wall time does
not move), and **not the state lock** (commit `b47306e` cut lock wait time
31% with zero wall-time effect, and 5.8 s of aggregate wait across 8
threads is ~4% of aggregate thread-time). The remaining hypothesis was that
the constraint is the *shape* of the traversal -- specifically, that most
batches contain too few eval leaves to fork at all. This stage instruments
the engine to measure that directly rather than argue it.

### What was instrumented

Four counters, gated behind the existing `COMP_ENGINE_LOCK_STATS` env var
(read once at `initCompEngine`, exactly like `Run.hs`'s own lock-stats
flag, so the flag-off path adds no per-call branch beyond a `Maybe` case
already accepted for `ce_par`) and reported at teardown from
`Impl.stopCompEngine`, immediately before `Run.hs`'s own
`reportLockStats` tables print:

1. **Fork attempts/successes/permit-starved failures**, bumped around
   `prepEvalLeaf`'s existing `tryAcquirePermit` call.
2. **Time-weighted mean permits in use** -- an integral of occupancy over
   elapsed time, folded into a running sum on every acquire and release
   (one `getMonotonicTimeNSec` and one small STM transaction each), not a
   peak (the per-source high-water marks already show peaks, and this
   project's own numbers above show why a peak is misleading here).
3. **Fork-depth histogram**, keyed by the *forking* thread's own
   `EvalChain` ancestor-chain size (the forked child sits one level
   deeper), recorded on every successful fork.
4. **Eval-leaf-count-per-batch histogram**, keyed by a pure structural
   walk (`countEvalLeaves`) over the `CompReq` tree -- counting
   `CompReqEval` leaves, not running anything -- recorded once per
   `CompReqCombined` batch reaching `doSuspended`, *independent of
   `ce_par` entirely* (unlike the first three, this is a property of the
   graph, not of forking, and is exactly as interesting at width 1, where
   no permit pool even exists).

`Impl.stopCompEngine` is now called from `Run.hs`'s `main` via
`Control.Exception.finally` rather than a bare sequential call, so the
report is guaranteed to run on every teardown path, including
`Async.cancel` (every benchmark's own shutdown mechanism) -- the same
guarantee `reportLockStats` already relies on, extended to cover this new
report too.

**Cost.** Persist (`allocated_bytes` cold: 22,987,773,984 B, target
~22,988 MB) and Hospital (4,005.3 B/instance) both reproduce their
documented baselines exactly with the flag off -- expected, since neither
benchmark ever calls `setCompEvalConcurrency`, so `ce_par` is `Nothing`
throughout and every new counter in items 1-3 sits behind that `Just`
branch regardless of the flag. Item 4 does run at width 1 (it doesn't gate
on `ce_par`), but `whenDiag` never forces its argument when diagnostics
are off, so `countEvalLeaves` is never evaluated either. On the tiered
benchmark itself, flag-on and flag-off runs at width 8 measured 17.944 s
and 18.308 s respectively -- flag-on was *faster*, i.e. the difference is
inside this benchmark's own run-to-run noise band, not a measurable cost.

### The four metrics, widths 1/8/16 (`TIERED_BENCH_RERUN_KEYS=0`, default scale, same session)

Batch-call counts (and therefore the leaf histogram) were bit-identical
across all three widths, confirming metric 4 is structural, not a function
of concurrency:

| | width 1 | width 8 | width 16 |
|---|---|---|---|
| cold eval wall time | 59.220 s | 17.944 s | 18.414 s |
| fork attempts | 0 | 158,712 | 155,370 |
| fork successes | 0 | 34,371 (21.7%) | 55,335 (35.6%) |
| permit-starved failures | 0 | 124,343 (**78.3%**) | 99,939 (**64.3%**) |
| time-weighted mean permits in use | 0.000 | 4.681 / 7 (66.9%) | 8.296 / 15 (55.3%) |
| lock wait time (existing metric, for reference) | 0.039 s | 6.380 s | 14.591 s |

Fork-depth histogram, both widths dominated by one depth:

| depth | width 8 | width 16 |
|---|---|---|
| 1 | 29 | 53 |
| 2 | 14 | 26 |
| 3 | 372 | 332 |
| 4 | 1,032 | 2,296 |
| 5 | 180 | 477 |
| **6** | **32,747** | **52,163** |

Eval-leaf-count-per-batch histogram (identical at every width):

| eval leaves | batches | comp shape |
|---|---|---|
| 0 | 105,841 | `vitalComp`/`medOrderComp` -- source-only |
| 2 | 27,722 | `interactionComp` -- 1 src + 2 evals |
| 3 | 182 | `labResultComp`-like, source-only |
| 5 | 9,001 | `vitalWindowComp` -- 1 src + 5 `vitalComp` evals |
| 12 | 1 | (one-off, cold-start shape) |
| 45 | 12 | `wardCensusComp`/`wardOccupancyComp`/`wardRiskBoardComp` -- per-ward fan-in, 4 wards x 3 comps |
| 140 | 180 | (per-patient, unidentified exactly; scales with patient count) |
| 360 | 3 | `transferCandidatesComp` -- two 180-patient traverses combined |
| **383** | **181** | **`riskScoreComp` -- 50 `vitalWindowComp` + 180 `labTrendComp` + 153 `interactionComp` evals, one per patient** |

(143,123 of 143,477 total batch calls accounted for; the remainder are
bare single-request leaves like `admissionComp`'s lone source read, which
never reach `CompReqCombined` at all and so are outside this histogram by
construction, not missing data.)

### Cross-check: source-seconds / wall against metric 2

Using this session's own measured per-tier `usleep` costs (vitals 47 us,
notes 133 us, labs 319 us, adt 636 us, pharmacy 1288 us) against the
batch-call counts above (identical at every width): total source-seconds =
(186 * 636) + (54184 * 47) + (32763 * 319) + (30962 * 1288) + (25382 * 133)
microseconds = **56.37 source-seconds**, matching the 56.4 s figure this
stage was framed against.

| | width 1 | width 8 | width 16 |
|---|---|---|---|
| source-seconds / wall | 0.952x | **3.142x** | **3.062x** |
| metric 2 (mean permits in use) | 0.000 | 4.681 | 8.296 |
| metric 2 + 1 (calling thread) | 1.000 | 5.681 | 9.296 |

The two independent measurements corroborate the headline number (~3.1x
at both width 8 and width 16, confirming the plateau is real and not a
measurement artifact) but disagree on magnitude: metric 2 implies ~5.7-9.3
threads doing *something* concurrently on average, well above the ~3.1x
of that work that is actually latency-bound overlap. The gap widens with
width (5.7 vs 3.14 at width 8; 9.3 vs 3.06 at width 16) and tracks lock
wait time growing from 6.4 s to 14.6 s over the same step -- consistent
with a growing share of *held* permits going to threads contending for the
state-if lock rather than doing overlapping I/O, on top of (not instead
of) the fan-out ceiling below. This is a secondary drag, not the primary
one: it is still a small fraction of aggregate thread-time (14.6 s over
16 x 18.4 s = 294.6 s of aggregate thread-time is ~5%), the same order of
magnitude Stage 12's lock investigation already measured and ruled out as
insufficient on its own.

### Verdict: fork failures are common, and the fan-out ceiling is the answer

**Permit-starved failures are the majority outcome at every width tested**
(78.3% at width 8, 64.3% at width 16) -- the opposite of "rare," and
exactly the result flagged in advance as the one that would contradict the
width-16 result. It doesn't contradict it; it explains it. The
eval-leaf-count-per-batch histogram shows why: wall time here is dominated
by pharmacy (39.9 of the 56.4 total source-seconds, 71%), and pharmacy is
paid almost entirely inside `riskScoreComp`'s 181 per-patient batches --
each one **383 eval leaves wide**. `prepEvalLeaf` never forks a batch's
first leaf, so each of those batches offers up to 382 fork attempts, made
during a fast, non-blocking, strictly sequential IO collect phase (see
`prepLeaf`/`traverseCompReq`): whichever leaves are visited before the
shared, engine-global permit pool empties get forked; every leaf visited
afterward -- for that entire batch's remaining lifetime, since a starved
attempt never retries -- runs serially, inline, on the one thread that
owns the batch. A pool of 7 (width 8) or 15 (width 16) against a fan-out
of 382 forks at most 1.8-3.9% of it concurrently; the rest, however wide
the pool, is paid serially on one thread. The fork-depth histogram
confirms this is where nearly every successful fork actually happens too:
depth 6 -- `riskScoreComp`'s own evaluation depth, one level above its
`interactionComp`/`vitalWindowComp`/`labTrendComp` children -- accounts
for 96-97% of all successful forks at both widths measured.

This also explains why doubling the pool (width 8 to 16) barely moves
wall time (17.944 s to 18.414 s, flat within noise) even though mean
occupancy nearly doubles (4.681 to 8.296) and successes nearly double
(34,371 to 55,335): doubling a small slice of a 382-wide fan-out is still
a small slice, and the *nested* batches competing for the same shared
pool (`interactionComp`'s own 2-eval-leaf sub-batches, one per pharmacy
read) inherit the same starvation, since a hot outer batch can and does
exhaust the pool before any inner batch gets a turn.

The eval-leaf-count-per-batch histogram also shows the milder form of the
same story structurally, independent of any of this: 93% of all
`CompReqCombined` batches (133,563 of 143,123) carry 0 or 2 eval leaves --
trivial or barely-forkable on their own. But those are not the batches
wall time is spent in; the 181 batches with 383 eval leaves each are, and
those are exactly the ones the fixed-size global permit pool cannot come
close to servicing.

### What a fix would have to change

Not the width knob -- this graph's dominant batch already asks for a
fan-out (382) an order of magnitude past even the widest pool tested
(15 permits at width 16), so further raising width chases a ceiling that
recedes at the same rate for any width small enough to be practical.
Raising it substantially might help *this* graph, but shifts the same
problem to whatever graph has a still-wider single batch -- it does not
generalize. Two changes that would: **(a)** let a starved leaf retry once
a permit frees up later in the *same* batch's run phase, rather than
permanently falling back to inline the moment its own collect-phase visit
finds the pool empty -- today's policy only lets the first
`permits`-many-eligible leaves in traversal order ever get a chance,
regardless of how long the batch's own run phase (and thus the window
during which permits free up) actually lasts; **(b)** give each batch (or
each top-level fork site) its own bounded sub-reservation of the shared
pool, so one 382-wide batch cannot starve every sibling and nested batch
competing for the same global `TVar`. Both are policy changes at the
fork/permit layer, not resource increases -- consistent with this stage's
own finding that the bottleneck is how forking is *rationed* under a wide
fan-out, not how many permits exist.

### Disposition

Measurement only, per this investigation's own charter -- no fix applied.
Instrumentation added: `Control.Computations.CompEngine.Impl`'s
`EngineDiag` (fork attempts/successes/failures, time-weighted occupancy,
fork-depth histogram, eval-leaf-per-batch histogram), gated on
`COMP_ENGINE_LOCK_STATS`, reported at teardown via `stopCompEngine`
(now guaranteed to run under `Async.cancel` via a `finally` in `Run.hs`'s
`main`, matching `reportLockStats`'s own guarantee). `stack test --flag
incremental-computations:werror` -- 158 tests, run twice, both clean;
`TestCompEvalConcurrency`'s character is unchanged (no flakiness observed
in either run). `TestCompReqCombined.hs`'s golden ordering trace was not
touched.

## Stage 14 — the sliding fork window: breaks the plateau at width 16, a mixed result at width 8

Stage 13's own recommended fix (a): let a starved leaf retry once a permit
frees up later in the same batch's run phase, instead of being condemned
the instant its own collect-phase visit finds the pool empty.

### The shape chosen, and why it isn't a pure "defer everything to run phase"

The obvious form the investigation was framed against -- an ordered list of
leaf actions plus a cursor, topped up before joining leaf *k* -- turned out
to need one addition to survive contact with an existing test. A first
implementation deferred *every* fork decision to the run phase (leaf's own
join action calls `topUpWindow`, nothing forks during collect at all). That
version broke
`test_hashCachingCapReferencedTwiceInOneBatchEvaluatesOnceAtEvalWidth8`
close to half the time: the test's dedup depends on a repeated
`hashCaching` reference's *second* leaf racing the *first* leaf's
still-in-flight evaluation (see `doAnyEvalReqValue`), and that race is only
winnable if the second leaf starts *during collect*, before the first
leaf's own run-phase join action has run to completion and released its
claim. Deferring every leaf's first attempt to its own join point loses
that race by construction for a batch's first eligible leaf, since nothing
tops the window up before it ever runs.

The shape landed on instead is a **hybrid**: `prepEvalLeaf` still attempts
`tryAcquirePermit` immediately, during collect, exactly reproducing the old
collect-time-only decision's own timing -- so a leaf that wins this first
race is forked at exactly the point in the traversal it always was, and
narrow batches (few eligible leaves, permits to spare) behave identically
to before this window existed, `hashCaching` race included. Only a leaf
that *loses* this first race is queued into a shared, per-batch
`PendingEvalLeaf` window (a difference-list built during collect, finalised
into a plain list once collect finishes, mirroring `SrcGroup`'s own
`sg_fetches` idiom) instead of being condemned. Every subsequent
fork-eligible leaf's own run-phase join action calls `topUpWindow` before
consulting its own state: `topUpWindow` starts as many of the queue's
front-most not-yet-started leaves as there are free permits for, strictly
in traversal order, stopping at the first failure so a still-stuck leaf is
retried by the *next* call rather than skipped past. A leaf whose own
`stateRef` is still empty by the time its own join point arrives (window
never reached it) pops itself off the queue's front -- which, by
`topUpWindow`'s own invariant, it must still be -- and falls back to the
same un-forked `doAnyEvalReqValue` call it always would have. **Only starts
float; joins stay exactly where `Prep`'s own `Applicative` already
sequences them**, left to right, unchanged. The window's size is bounded
by the shared, engine-global permit pool alone -- no new knob.

### The bug the first measurement caught: releasing at join time, not completion time

Wiring the hybrid design up exactly as `docs/benchmark-notes.md`'s Stage 13
described it -- `releasePermit` inside the same `finally` as the joining
leaf's `Async.wait`, unchanged from the original collect-time-only fork --
produced a measurement that contradicted its own premise: fork *successes*
at width 8 **fell** from the unmodified baseline (32,790-34,831) to 7,090,
even though time-weighted occupancy *rose* (4.71/7 to 6.37/7) and average
per-fork hold time roughly *sextupled* (≈3.4 ms to ≈20 ms, computed from
occupancy × window ÷ successes). The mechanism: joins are strictly
left-to-right, but a fork's *actual work* finishing is not -- a fast leaf
started right after a slow one (routine, given this graph's per-source
latency tiers: 47-1288 μs) finishes its real work quickly, but under
"release at join," its permit stays reserved for however long every leaf
*before* it in join order takes to be reached and joined, not for how long
its own work actually took. That idles permits on already-finished work
instead of freeing them for `topUpWindow` to hand to the next queued leaf
-- the opposite of the fix's own goal. The repair: release the permit (and
the matching occupancy bump) **inside the forked action itself**, via a
`finally` wrapped around the actual `runCompEngineM'` call passed to
`forkTracked`, not around the joining leaf's `Async.wait`. The join still
happens in order; `Async.wait` on an already-finished `Async.Async` (the
common case once the window runs ahead of the join cursor) just reads a
result. This also happens to close a latent gap in the pre-existing
collect-time-only fork (not introduced by this stage, not chased further):
a permit whose owning leaf's join action is never reached at all -- an
earlier sibling threw first -- previously never called its `finally`
either, since that `finally` lived on the join side; tying release to the
fork's own completion means `cancelAllTracked`'s cancellation on the
exception path now also guarantees the permit comes back.

### The four metrics, before vs after (same session, `TIERED_BENCH_RERUN_KEYS=0`, default scale, `COMP_ENGINE_LOCK_STATS=1`, 2 reps/cell)

| | width 1 | width 8 (before -> after) | width 16 (before -> after) |
|---|---|---|---|
| cold eval wall time | 72.49 -> 71.93 s | 21.51-21.83 s -> **19.17-19.76 s** | 22.37-22.47 s -> **9.67-10.04 s** |
| fork attempts | 0 | 157,912-158,807 -> 330,536-333,658 | 155,825-156,474 -> 361,975-370,474 |
| fork successes | 0 | 32,790-34,831 (21-22%) -> **16,496-20,946 (5-6%)** | 53,194-53,879 (34%) -> **60,370-66,569 (17-18%)** |
| mean permits in use | 0.000 | 4.709-4.715 / 7 (67%) -> **6.314-6.335 / 7 (90%)** | 8.490-8.511 / 15 (57%) -> **12.316-12.398 / 15 (82%)** |
| source-seconds / wall (56.37 s numerator) | 0.78x -> 0.78x | **2.60x -> 2.90x** | **2.51x -> 5.72x** |

Batch-call counts (and therefore the leaf-count histogram) stayed
bit-identical across every cell, confirming the graph itself did not move.

### Width 16: the plateau is broken

Stage 13's headline finding was that width 16 barely moved wall time
against width 8 (17.9 vs 18.4 s in that stage's session) despite offering
double the permit pool -- "more permits don't help" against a fan-out an
order of magnitude past even the widest pool. That plateau is gone here:
width 16 now measures **9.67-10.04 s, a genuine 2.2-2.3x speedup over
width 8's 19.17-19.76 s**, where the unmodified baseline shows width 16
*costing* slightly more than width 8 (22.4 vs 21.7 s, the plateau/mild
regression Stage 13 documented). Effective concurrency more than doubles,
2.51x to 5.72x, exactly the kind of jump "the window did what it claims"
should produce. Occupancy rose from little more than half the pool (57%)
to over four-fifths (82%), and total successful forks rose too (+19-25%
depending on which pair of reps is compared). **This is not a null
result at width 16** -- the sliding window measurably converts previously
wasted, idle permit capacity into completed work.

### Width 8: wall time improved, but the diagnostic story is genuinely mixed -- reported as found, not smoothed over

Width 8 does **not** tell the same clean story, and this is worth stating
plainly rather than folding into the width-16 win. Wall time did improve,
consistently, across every paired rep (21.51-21.83 s to 19.17-19.76 s, a
real ~9-12%) -- but occupancy rising sharply (67% to 90%) came together
with the raw fork-success *count* **falling** (32,790-34,831 to
16,496-20,946), the opposite of what this stage's own prediction expected
("success ratio should rise sharply"). The fork-depth histogram explains
where the successes moved, not just that they moved: the unmodified
baseline's successes are overwhelmingly depth 6 (33,248 of ~34,880, 95%) --
`riskScoreComp`'s own batch forking its direct children, exactly Stage
13's finding. After the fix, depth 5 dominates instead (17,096 of ~20,948,
82%), with depth 6 reduced to a few hundred. Depth is the *forking
thread's own* ancestor-chain size, fixed per batch (captured once in
`doSuspended`, shared by every leaf of that batch regardless of when its
own fork attempt happens) -- so this is not a measurement artifact, it is
successes genuinely concentrating one level shallower in the tree than
before. The most likely explanation, not fully chased down: at width 8 the
window doesn't just help `riskScoreComp`'s own 382-wide internal fan-out --
it *also* lets `riskScoreComp`'s own **parent** batch (the depth-5 level,
plausibly the per-patient batches the leaf histogram calls "140 eval
leaves, unidentified exactly") successfully fork more of *its* eligible
leaves too, letting several whole `riskScoreComp` evaluations for
*different* patients run concurrently for the first time -- a kind of
overlap the collect-time-only decision never produced at all, since a
parent batch's own eligible leaves beyond its collect-time permit share
were just as permanently condemned as anything else. That shifts demand
for the same shared 7-permit pool from "one `riskScoreComp` instance
forking heavily" to "several `riskScoreComp` instances competing," which
would produce exactly this signature: more of the *pool's* capacity in use
(higher occupancy, wall time down) but fewer of any *one* level's attempts
succeeding outright (lower raw success count at the level that used to
dominate). This is offered as the most likely reading of the data in hand,
not a proven mechanism -- per this stage's own charter, **not chased
further**: wall time did move, in the right direction, at both widths
measured, which is the question this stage was actually asked to answer.

### Width 1 unchanged

- `Persist`: `allocated_bytes` (cold) **22,987,626,200 B identical**,
  before and after, both freshly rebuilt in this session (the
  `docs/benchmark-notes.md`-quoted `22,987,773,984 B` figure did not
  reproduce for *either* build in this session's toolchain state --
  session/toolchain drift on the target number itself, not a regression:
  what matters is before and after agree, bit for bit, on the same build).
- `Hospital`: **4,005.3 B/instance identical**, matching the documented
  baseline exactly.
- Tiered cold wall at width 1: 72.49 -> 71.93 s, flat within this session's
  own noise band; `ce_par` is `Nothing` throughout (width 1 never allocates
  `ParState`), so `prepEvalLeaf`'s `_ -> pure inline` branch is reached
  before `mWindow` is ever touched -- literally the pre-window code path.

### Tests

`stack test --flag incremental-computations:werror` -- 158 pre-existing
tests plus one new one (`test_slidingWindowForksMoreLeavesThanThePermitCountAtEvalWidth3`,
`TestCompEvalConcurrency.hs`) proving the window forks more leaves than the
raw permit count over a batch's lifetime, via distinct `Async`-spawned
`ThreadId`s observed through a real `threadDelay` inside the state-if's
`capEvaluationStarted` hook (safe IO, not inside a `CompM` body -- see this
doc's own caveat on `unsafePerformIO` in a "pure" comp body). That test is
stable across dozens of runs.

`test_hashCachingCapReferencedTwiceInOneBatchEvaluatesOnceAtEvalWidth8`
(pre-existing) is not: measuring it directly, in isolation from every other
change in this stage, both **before and after this stage's commit, on
the exact same optimised (`-O2`, no `--fast`) build `stack test` itself
produces** -- the previously undocumented baseline is **22/60 ≈ 37%**
failures (unmodified HEAD, this session), rising to **29/60 ≈ 48%** after
this stage's change (60 standalone runs of the built test binary, each
cell). Both figures are new measurements this stage took specifically
because a single `stack test` run flagged the failure the CLAUDE.md brief
warned about ("intermittently flaky ... do not chase it, but report if its
character changes") -- the `--fast` (unoptimised) build shows **0/50** for
both, confirming the race is real but requires call/allocation overhead
low enough for a fresh green thread's scheduling latency to actually
compete with an essentially-instant `pure` comp body's own claim-eval-
release cycle, which only `-O2` gets close enough to. This is a genuine,
if modest, character change (+11 points), most plausibly the one extra
`IORef` write-then-read the window's cross-call-site retry design needs on
the immediate collect-time attempt (see "the shape chosen" above) --
**not chased further**, per the brief's own instruction: the failure mode
is a test built on winning a hardware-scheduling race with a trivial body,
not a correctness bug in the dedup mechanism itself (whichever side wins
the race, the promise table still dedupes correctly; the only failure is
both sides finishing with no overlap window at all).

### Disposition

Kept. `Control.Computations.CompEngine.Impl`: new `PendingEvalLeaf`
newtype, `topUpWindow`/`popWindowFront` functions, `prepEvalLeaf` rewritten
to the hybrid immediate-attempt/queued-retry shape above, `mWindow`
threaded through `prepLeaf`/`doSuspended`'s general path (allocated only
when `ce_par` is `Just`, so width 1 pays nothing new). `ed_forkAttempts`'s
haddock updated: one logical leaf can now contribute more than one
attempt/failure pair (retried across several later leaves' own join
points) before it either succeeds or falls back to inline, unlike the
collect-time-only decision this replaced. `TestCompReqCombined.hs`'s
golden ordering trace was not touched.
