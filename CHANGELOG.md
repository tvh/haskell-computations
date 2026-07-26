# Changelog for `incremental-computations`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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
