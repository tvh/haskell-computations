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
