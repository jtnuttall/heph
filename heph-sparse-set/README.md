# heph-sparse-set

A highly-performant, mutable sparse set implementation for Haskell.

This library is part of the forthcoming Hephaestus rendering system, but is sufficiently general purpose for any application that needs its particular behavior.

This library provides a mutable sparse set data structure designed for high-throughput systems where predictable, constant-time operations are critical. It is particularly well-suited for the performance demands of applications like game development (especially in ECS architectures), simulations, and other scenarios managing collections of integer-keyed data.

The design guarantees O(1) amortized complexity for core operations by leveraging direct array indexing and cache-friendly memory layouts.

## Features

- **Algorithmic Guarantees**: Provides amortized O(1) complexity for all core operations: `insert`, `delete`, and `lookup`.
- **Cache-Locality & Efficiency**: `Unboxed` and `Storable` implementations are built to ensure contiguous memory layout and minimal pointer indirection.
- **Monad-Agnostic Interface**: Operates within any `PrimMonad` context, enabling its use in both `IO` and pure `ST` computations without performance degradation.
- **Efficient Iteration**: Offers iterators (`mapM_`, `ifoldM`) that operate directly on the underlying contiguous storage, avoiding the allocation of intermediate lists.
- **Tested for Correctness**: The implementation is validated by a comprehensive test suite.

## Design & Implementation

At its core, `heph-sparse-set` is implemented with three internal vectors to provide its performance characteristics:

1.  A **dense** vector that stores the component data itself in a tightly packed array. Iteration happens over this array.
2.  An **indices** vector, also dense, that stores the entity ID for each element in the dense vector.
3.  A **sparse** vector that maps entity IDs directly to their location in the dense arrays. A lookup is a simple O(1) check in this vector.

When an element is removed, the last element from the dense arrays is swapped into the deleted element's slot (a "swap-and-pop"). This maintains the packed nature of the dense storage and ensures the O(1) time complexity for removals.

## Performance Characteristics

The full benchmark suite can be found in the `bench` directory and run with `stack bench`.

Benchmarks show that `heph-sparse-set` holds a significant performance advantage for its target workloads.

The following results compare `heph-sparse-set` against `Data.IntMap.Strict` for **100,000** elements. The speedup factor is an approximation, rounded to account for statistical noise in the measurements.

| Operation                | `heph-sparse-set` | `Data.IntMap.Strict` | Advantage            | Speedup Factor |
| :----------------------- | :---------------- | :------------------- | :------------------- | :------------- |
| **Get (Existing)**       | **134 µs**        | 4,351 µs             | `heph-sparse-set`    | ~32x           |
| **Contains (Existing)**  | **113 µs**        | 4,380 µs             | `heph-sparse-set`    | ~39x           |
| **Update (Dense)**       | **356 µs**        | 21,120 µs            | `heph-sparse-set`    | ~59x           |
| **Insert (Dense)**       | **2.39 ms**       | 18.76 ms             | `heph-sparse-set`    | ~8x            |
| **Insert (Sparse, Asc)** | 82.41 ms          | **32.95 ms**         | `Data.IntMap.Strict` | ~2.5x          |
| **Remove (Dense)**       | **1.29 ms**       | 2.91 ms              | `heph-sparse-set`    | ~2x            |
| **Intersection (50%)**   | **191.0 μs**      | 2.054 ms             | `heph-sparse-set`    | ~11x           |
| **Mixed Workload**       | **1.06 ms**       | 13.73 ms             | `heph-sparse-set`    | ~13x           |

### Analysis

- **Lookups**: The core strength of `heph-sparse-set` is true O(1) lookups, which is orders of magnitude faster than the O(log n) tree traversal of an `IntMap`.
- **Dense Operations**: For densely packed entity IDs, `heph-sparse-set` maintains a strong advantage across all operations due to its cache-friendly memory layout.
- **Trade-Offs**: The table explicitly demonstrates the primary trade-off. `Data.IntMap.Strict` is significantly faster when inserting **sparse, sequentially ascending keys**.

## Usage

Here's a basic usage example with Unboxed sets in `IO`.

```haskell
import Data.SparseSet.Unboxed.Mutable qualified as SS

-- An entity in our system is just an Int
type Entity = Int
type Position = (Int, Int)

main :: IO ()
main = do
  -- Create a new sparse set
  positions <- SS.new @Position

  -- Insert some components
  SS.insert positions 10 (5, 5)   -- Entity 10 has position (5, 5)
  SS.insert positions 42 (1, 2)   -- Entity 42 has position (1, 2)
  SS.insert positions 3  (9, -4)

  -- Look up a component
  maybePos <- SS.lookup positions 42
  putStrLn $ "Position of entity 42: " <> show maybePos -- Just (1,2)

  -- Overwrite an existing component
  SS.insert positions 10 (6, 6)

  -- Delete a component
  deletedVal <- SS.delete positions 3
  putStrLn $ "Deleted component for entity 3: " <> show deletedVal -- Just (9,-4)

  -- Check for existence
  has42 <- SS.contains positions 42 -- True
  putStrLn $ "Set contains 42: " <> show has42
  has3  <- SS.contains positions 3  -- False
  putStrLn $ "Set contains 3: " <> show has3

  -- Efficiently iterate over all (entity, component) pairs
  putStrLn "Current members:"
  SS.imapM_ (\(entity, pos) -> print (entity, pos)) positions
  -- Expected output (order may vary):
  -- (10,(6,6))
  -- (42,(1,2))
```

## Considerations

### Memory Consumption

In general use, the memory consumption of this implementation will grow relative to the maximum key in the set. It is advisable to manage memory consumption deliberately:

1. Use the `compact` method periodically to reduce memory consumption. This is an expensive operation, so a reasonable heuristic would need to be established for your specific system.
2. Use a generational entity ID, so that entity ID growth can be constrained to a reasonable degree.

### Concurrency

This library is intentionally not thread-safe. For concurrent use, thread safety must be provided externally.

### Backwards Compatibility

I took some pains to ensure that this library is reasonably backwards-compatible. However, I'm intentionally designing Hephaestus to use GHC2021, so while this library doesn't use GHC2021, it does depend on modern language extensions that constrain its compatibility.

**Please note that you may experience performance degradation on GHC < 9.2**

- **Minimum supported GHC**: 8.10.4
- **Minimum supported Stackage snapshot**: lts-18.0

## Roadmap

- Compacted immutable sparse sets with read-only semantics for storage
- Atomic operations or wrappers
