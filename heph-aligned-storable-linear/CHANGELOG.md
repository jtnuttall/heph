# Changelog for `heph-aligned-storable-linear`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- Initial release. `AlignedStorable` instances, under `Std140`, `Std430` and
  `Scalar`, for linear's `V2`, `V3` and `V4` of `Bool`, `Half`, `Float`,
  `Double`, `Int8` to `Int64` and `Word8` to `Word64`, and for `M22` to `M44`
  of `Half`, `Float` and `Double`. They moved here from `heph-aligned-storable`
  0.1.0.0.
- Test suite running the `heph-aligned-storable-testlib` conformance suites and
  inspection tests on the linear types.
