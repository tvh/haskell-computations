# Changelog for `incremental-computations`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- `FlowConcurrency(..)` and a defaulted `compSrcConcurrency :: s ->
  FlowConcurrency` method on `CompSrc`, so a source can declare that the
  engine may run several of its `compSrcExecute` calls concurrently.
  Defaults to `FlowSerial`, so no existing instance's behaviour changes;
  `HashMapFlow`, `TimeSrc`, and `FileSrc` now declare `FlowConcurrent`.
- A second benchmark, the Hospital pipeline benchmark
  (`HOSPITAL_BENCH=1 stack bench`), with a graph that builds real applicative
  batches against sources with configurable latency — something the
  existing scale benchmark's graph cannot do. Env vars: `HOSPITAL_BENCH_SCALE`,
  `HOSPITAL_BENCH_SRC_LATENCY_US`. See the README's Benchmark section.
- `COMP_ENGINE_LOCK_STATS` env var: when set, instruments the engine's
  single state-lock with acquisition count and total hold time, printed at
  engine shutdown. Off by default; adds no overhead when unset.
- A third phase on the Hospital pipeline benchmark: a rerun-heavy live
  update that mutates many source keys in one batch (spread across patients
  and all five sources) and times the incremental engine's rerun path at a
  scale the existing single-key live phases (8 reruns each) cannot exercise
  — 400 keys by default, ~3,069 reruns. Runs on every plain
  `HOSPITAL_BENCH=1 stack bench` invocation; reports keys mutated, wall
  time, reruns, and µs/rerun. Env vars: `HOSPITAL_BENCH_RERUN_KEYS` (0
  disables the phase), `HOSPITAL_BENCH_RERUN_LOOPS` (repeats the round,
  mirroring the scale benchmark's `PERSIST_BENCH_LIVE_LOOPS`). Also adds
  `SystemSrc.sysInsertBatch`, an atomic multi-key insert — the naive
  "many separate `sysInsert` calls, then one `waitForFullSettle`" approach
  races the driver thread and can silently undercount reruns.
- `CompEvalConcurrency`, `mkCompEvalConcurrency`, `unCompEvalConcurrency`,
  `setCompEvalConcurrency`/`readCompEvalConcurrency` on `CompFlowRegistry`:
  the engine's one and only concurrency width knob, for how many nested cap
  evaluations (eval leaves of a `CompReqCombined` batch) may fork to a
  permit-bounded pool instead of running one at a time on the engine
  thread. A `FlowConcurrent` source (or sink) instance's own genuine
  overlap now comes from this same forking, not from a dispatch mechanism
  of its own: two eval leaves forked onto separate threads can each build
  their own nested batch against the same instance, and those nested
  batches run concurrently for real. A promise table (one IVar per
  in-flight cap) guarantees each cap is still evaluated at most once per
  occasion no matter how many leaves reference it. Default width 1
  allocates no fork pool at all and is byte-identical to the pre-existing
  engine; `newCompFlowRegistry`'s signature is unchanged, so this is purely
  additive. Read once, at engine start. See the README's "Concurrent
  computation evaluation" and "Concurrent flow execution" sections and
  `docs/benchmark-notes.md`'s Stages 13-15 for the design, the ordering
  contract that changes above width 1, and measured numbers (16.6x
  cold-eval speedup within one session on the tiered benchmark at width 64,
  against a ~64-wide ceiling).
- `compSinkConcurrency :: s -> FlowConcurrency` on `CompSink`, the sink-side
  sibling of `CompSrc`'s `compSrcConcurrency`. Defaults to `FlowSerial`, so
  no existing `CompSink` instance's behaviour changes; only consulted when
  eval concurrency is enabled, since that's the only way two threads could
  ever reach the same sink instance at once.
- A third benchmark, the tiered pipeline benchmark
  (`TIERED_BENCH=1 stack bench`), extending Hospital's graph with per-source
  latency tiers (a ~40x spread across five sources) and comps that read a
  source alongside their existing comp dependencies in the same applicative
  batch — the mixed shape needed to actually exercise eval concurrency,
  which Hospital's own graph mixes in only one comp. Env vars:
  `TIERED_BENCH_SCALE`, `TIERED_BENCH_LATENCY_MULT`, `TIERED_BENCH_JITTER`,
  `TIERED_BENCH_BUNDLING`, `TIERED_BENCH_EVAL_CONCURRENCY`,
  `TIERED_BENCH_RERUN_KEYS`,
  `TIERED_BENCH_RERUN_LOOPS`. See the README's Benchmark section.

### Changed

- **Ordering contract above eval width 1**: turning on
  `setCompEvalConcurrency` above its default of 1 gives up two guarantees
  the engine previously always held — the interleaving of
  `capEvaluationStarted` across a batch's eval leaves, and the relative
  order of sink writes issued by *different* caps (order within one cap's
  own body is still preserved). Dependency correctness, dedup (now complete
  at any width, `hashCaching` included), left-error bias, and
  join-before-batch-returns all still hold at any width. Width 1 is
  unaffected and remains byte-identical to the engine as it existed before
  this change, evaluation order included — this is a behaviour change only
  for callers who opt in. See the README's "Concurrent computation
  evaluation" section and `docs/benchmark-notes.md`'s Stage 15, "The
  ordering contract, which changed".

### Fixed

- `stack bench` (the plain scale benchmark, no env vars) errored out before
  printing its memory section, because the benchmark's baked-in
  `-with-rtsopts=-A64m` was missing `-T`, so `GHC.Stats.getRTSStats` threw.
  User-visible since it broke the exact command the README documents.

## 0.2.0.0 - 2026-07-25

This release is a fork of the original `incremental-computations` package
(`skogsbaer/incremental-computations`), maintained going forward at
`tvh/haskell-computations`. It packages the library for standalone
publication and makes the following user-facing changes:

### Changed

- The computation engine's internal state representation was rewritten to a
  columnar, per-definition layout (`Control.Computations.CompEngine.Utils.*`).
  This is an internal change with no effect on the public API, but it is the
  reason for the minor version bump: downstream code that reached past the
  public API into engine internals will need to update.
- The public API surface is now deliberately narrow: only 21 of the 47
  modules under `Control.Computations` are exposed. The rest (engine
  internals, the columnar state layer, and `Utils` modules that never appear
  in an exposed signature) are implementation details and may change without
  a major version bump going forward. See the `exposed-modules` comments in
  `package.yaml` for the module-by-module rationale.
- `SqliteSrc` moved out of the library and into the Hospital demo
  (`app/Control/Computations/Demos/FlowImpls/SqliteSrc.hs`); it was
  demo-specific and not part of the intended public surface.
- `Control.Computations.CompEngine.CompSrc.typedCompSrcId` and
  `Control.Computations.CompEngine.CompSink.typedCompSinkId` (build a typed
  id from a bare instance-name string, with nothing to check it against)
  are renamed to `unsafeMkTypedCompSrcId`/`unsafeMkTypedCompSinkId` to mark
  them as the unchecked escape hatch: a typo, or an instance name that has
  drifted from what the actual source/sink instance reports, still
  typechecks and only fails at runtime, as a registry-lookup miss. Prefer
  the new `typedCompSrcIdOf`/`typedCompSinkIdOf` (see Added below) whenever
  a live instance is in scope.
- `TypedCompSrcId`/`TypedCompSinkId` are now exported abstractly (plus the
  `unTypedCompSrcId`/`unTypedCompSinkId` accessor for the safe
  typed-to-untyped direction); their raw constructors are no longer part of
  the public API, closing a second unchecked way to fabricate a typed id.

### Removed

- All test code (HTF-based inline tests, test-only modules) moved out of the
  library into `test/`. The library no longer depends on HTF or QuickCheck's
  test-runner machinery at all; the exposed `QuickCheck` dependency that
  remains is for a small number of `Arbitrary` instances shipped alongside
  their types (see `package.yaml`'s `library.dependencies` comment).
- `Control.Computations.CompEngine.CompSrc.compSrcId` and
  `Control.Computations.CompEngine.CompSink.compSinkId` are no longer part
  of the public API. Both were exactly `unTypedCompSrcId . typedCompSrcIdOf`
  (resp. sink) and therefore redundant now that `typedCompSrcIdOf`/
  `typedCompSinkIdOf` exist; use that composition, or keep the typed id,
  instead.

### Added

- `Control.Computations.CompEngine.CompSrc.typedCompSrcIdOf` and
  `Control.Computations.CompEngine.CompSink.typedCompSinkIdOf` derive a
  source's/sink's typed id directly from a live instance, so it can't drift
  from what the instance itself reports via `compSrcInstanceId`/
  `compSinkInstanceId` -- the safe alternative to writing the instance's
  name out twice (once to build the id, once in the instance's own config)
  with only a runtime failure if the two copies disagree.
- A dedicated `benchmarks:` stanza (`incremental-computations-bench`) so the
  scale benchmark can be run with `stack bench` without pulling its
  dependencies into the library or the test suite. See
  `docs/benchmark-notes.md` and the README's Benchmark section.
- PVP version bounds on all library dependencies.

### Packaging

- `stack.yaml` no longer carries any `extra-deps`: every dependency,
  including `large-hashable`, resolves from the `lts-24.51` snapshot. The
  previous git-commit pin on `large-hashable` (which would have blocked a
  Hackage upload) is gone.

## 0.1.0.0 - YYYY-MM-DD
