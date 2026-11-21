# Changelog for `heph-aligned-storable-testlib`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Added

- Initial package. The `heph-aligned-storable` test suite moved here: the
  round-trip harness, the structs under test with their glslang-verified
  oracles, and the inspection tests.
- Shared suites parameterized over a vector library through a tag type and
  shape-group classes (`FloatVecs`, `FloatMats`, `DoubleVecs`, ...), with
  oracles that write vectors and matrices one component at a time.
- One `Oracle l a` newtype for all oracles; `sutTests` builds the round-trip
  properties and packed-size unit tests for a struct.
- Byte-image check in `ptrTripping`: the SUT and the oracle must write the same
  bytes into zeroed buffers, so writes into padding are caught.
- `inspectAlignedStorable`, a Template Haskell generator for the inspection
  tests of a type.
- Structs `NestedPrim`, `ArrayStruct`, `DeepNest`, `TailVec3`, `IntVecStruct`,
  `DUniforms`, `StructWithArrayF` and `StructWithArrayV`, verified with
  glslang 16.6.0.
