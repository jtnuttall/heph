# heph

heph is the umbrella project for Hephaestus, a rendering system built in Haskell. 
It houses a collection of high-performance, low-level libraries designed for systems 
programming.

This repository is a monorepo containing the core, reusable components of the engine. 
Each package is designed to be general-purpose and useful on its own.

## Packages

- heph-aligned-storable: A library to generically derive Storable instances suitable
  for CPU-GPU transfer. It provides compile-time guarantees for spec-compliant memory
  layouts (std140, std430, scalar).
- heph-input: A type-safe, high-performance input action mapping system for games.
  Maps low-level input events to semantic actions with compile-time type safety.
- heph-sparse-set: A mutable sparse set implementation, ideal for performance-critical
  applications like Entity Component System (ECS) architectures.

