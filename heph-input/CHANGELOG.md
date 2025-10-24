# Changelog for `heph-input`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 - 2025-10-22

Initial release of heph-input.

### Added

- Type-safe action mapping system using GADTs and phantom types
- Support for Button, Axis1D, and Axis2D action types
- Pattern synonyms for common input sources (Key, GamepadButton, MouseButton, etc.)
- Template Haskell code generation for Actionlike instances via `makeAction`
- Double-buffered input state for efficient delta queries
- Input aggregation for multiple sources bound to single actions
- Sensitivity and deadzone configuration for analog inputs
- Bounded array implementation for O(1) input lookups
- Comprehensive test suite with property-based tests and memory leak detection
