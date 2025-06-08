# Changelog for `heph-sparse-set`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2025-06-08

### Added

- Initial release of `heph-sparse-set`, a fast, mutable sparse set data structure.
- Provided `MutableSparseSet` implementations for `Unboxed`, `Storable`, and boxed types.
- Introduced amortized O(1) operations for insertions, deletions, and lookups.
- Implemented efficient iteration (`mapM_`, `ifoldM`) and intersection (`ifoldIntersectionM`).
- Included comprehensive test suite with unit tests, property-based tests, and `NoThunks` checks to ensure correctness and prevent space leaks.
- Designed with a flexible `PrimMonad` interface for use in `IO` and `ST` computations.

### Changed

- Explicitly marked as **NOT thread-safe**, prioritizing single-threaded performance.
