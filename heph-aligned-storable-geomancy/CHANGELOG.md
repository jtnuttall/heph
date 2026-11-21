# Changelog for `heph-aligned-storable-geomancy`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.1.0.0 - Unreleased

### Added

- Initial release. `AlignedStorable` instances, under `Std140`, `Std430` and
  `Scalar`, for geomancy 0.4: the 18 vector types (`Vec*`, `DVec*`, `IVec*`,
  `UVec*`, `I64Vec*`, `U64Vec*`), `Mat4`, `DMat4`, `Quaternion`, `DQuaternion`,
  `Transform`, `DTransform`, the `Packed` 3-vectors and `Point v` for every
  vector type.
- Test suite running the `heph-aligned-storable-testlib` conformance suites,
  plus glslang-verified structs for the geomancy-only types.
