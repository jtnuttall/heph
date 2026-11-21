# Changelog for `heph-aligned-storable`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Changed

- Require GHC >= 9.8 (dropped 8.10 and 9.6); tested with GHC 9.8–9.14.
- Raised dependency upper bounds for current Hackage and GHC 9.14 boot libraries.

### Removed

- The test suite moved to `heph-aligned-storable-testlib`.

## 0.1.0.0 - 2026-02-02

### Added

- Initial release. Generic derivation of `AlignedStorable` for GPU memory layouts.
- Provided instances for `Std140`, `Std430`, and `Scalar` layout rules.
- Added support for SPIR-V primitives, vectors, and matrices.
- Included `AlignedArray` for opt-in `memcpy` of fixed-size arrays.
- Added marshaling helpers (`withPacked`, `allocaPacked`) with guaranteed zero-initialized padding.
- Tested via property-based tests, unit tests verified against GLSL compiler output, and inspection tests.
