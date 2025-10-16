# Changelog for `heph-aligned-storable`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2025-10-14

### Added

- Initial release. Generic derivation of `AlignedStorable` for GPU memory layouts.
- Provided instances for `Std140`, `Std430`, and `Scalar` layout rules.
- Added support for SPIR-V primitives, vectors, and matrices.
- Included `AlignedArray` for opt-in `memcpy` of fixed-size arrays.
- Added marshaling helpers (`withPacked`, `allocaPacked`) with guaranteed zero-initialized padding.
- Tested via property-based tests, unit tests verified against GLSL compiler output, and inspection tests.
