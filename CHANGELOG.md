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

### Removed

- All test code (HTF-based inline tests, test-only modules) moved out of the
  library into `test/`. The library no longer depends on HTF or QuickCheck's
  test-runner machinery at all; the exposed `QuickCheck` dependency that
  remains is for a small number of `Arbitrary` instances shipped alongside
  their types (see `package.yaml`'s `library.dependencies` comment).

### Added

- A dedicated `benchmarks:` stanza (`incremental-computations-bench`) so the
  scale benchmark can be run with `stack bench` without pulling its
  dependencies into the library or the test suite. See
  `docs/benchmark-notes.md` and the README's Benchmark section.
- PVP version bounds on all library dependencies.

### Known issue

- `large-hashable` is pinned to a git commit in `stack.yaml`'s `extra-deps`
  rather than a Hackage release. This blocks an actual `cabal upload` of this
  package until `large-hashable` (or a replacement) is available from
  Hackage.

## 0.1.0.0 - YYYY-MM-DD
