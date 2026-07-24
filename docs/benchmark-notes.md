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
