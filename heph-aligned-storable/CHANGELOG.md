# Changelog for `heph-aligned-storable`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- `Foreign.GPU.Storable.Aligned.TH`: the generators behind the built-in instances,
  exposed so that other packages can bind their own math libraries.
- `Foreign.GPU.Storable.Aligned.Internal`: the class, layouts and helpers the
  generated code refers to. No PVP guarantees.

### Changed

- **Breaking** Require GHC >= 9.10 (dropped 8.10–9.8); tested with GHC 9.10–9.14. Library
  now uses GHC2024.
- Raised dependency upper bounds to allow current Hackage and GHC 9.14 boot libraries.
- Instances are generated with Template Haskell instead of CPP.

### Removed

- **BREAKING**: Moved instances for `linear` to `heph-aligned-storable-linear`.
- Test suite moved to `heph-aligned-storable-testlib`.
- Dropped the dependency on `unliftio` in favor of `unliftio-core`.

## 0.1.0.0 - 2026-02-02

### Added

- Initial release. Generic derivation of `AlignedStorable` for GPU memory layouts.
- Provided instances for `Std140`, `Std430`, and `Scalar` layout rules.
- Added support for SPIR-V primitives, vectors, and matrices.
- Included `AlignedArray` for opt-in `memcpy` of fixed-size arrays.
- Added marshaling helpers (`withPacked`, `allocaPacked`) with guaranteed zero-initialized padding.
- Tested via property-based tests, unit tests verified against GLSL compiler output, and inspection tests.
