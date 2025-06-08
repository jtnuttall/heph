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

The full benchmark suite can be found in the `bench` directory and run with `cabal bench` or `stack bench`.

The following results compare `heph-sparse-set` against `Data.IntMap.Strict` for **100,000** elements, using the default GHC garbage collector on a quiet system. The results are highly stable (R² > 0.99) and demonstrate the significant performance advantage of `heph-sparse-set` for its target workloads.

| Operation                | `heph-sparse-set` | `Data.IntMap.Strict` | Advantage            | Speedup Factor |
| :----------------------- | :---------------- | :------------------- | :------------------- | :------------- |
| **Get (Existing)**       | **138 μs**        | 3,749 μs             | `heph-sparse-set`    | **~27x**       |
| **Contains (Existing)**  | **111 μs**        | 3,788 μs             | `heph-sparse-set`    | **~34x**       |
| **Update (Dense)**       | **168 μs**        | 11,290 μs            | `heph-sparse-set`    | **~67x**       |
| **Insert (Dense, Asc)**  | **1.86 ms**       | 5.44 ms              | `heph-sparse-set`    | **~2.9x**      |
| **Remove (Dense)**       | **1.02 ms**       | 1.81 ms              | `heph-sparse-set`    | **~1.8x**      |
| **Intersection (50%)**   | **157 μs**        | 1,177 μs             | `heph-sparse-set`    | **~7.5x**      |
| **Mixed Workload**       | **868 μs**        | 10,670 μs            | `heph-sparse-set`    | **~12x**       |
| **Insert (Sparse, Asc)** | 77.23 ms          | **7.67 ms**          | `Data.IntMap.Strict` | **~10x**       |

### Analysis

- **Lookups and Updates**: The core strength of `heph-sparse-set` is its true O(1) complexity for lookups, updates, and containment checks. The **~27x to ~67x speedup** is a direct result of simple array indexing versus the O(log n) tree traversal required by `IntMap`.

- **Dense Workloads**: For densely packed entity IDs, `heph-sparse-set` is significantly faster across all operations. The **~2.9x speedup** for dense insertions highlights the efficiency of amortized O(1) appends to contiguous vectors over the allocations and rebalancing of a tree structure.

- **Iteration and Cache Performance**: The library's advantage in iteration-heavy tasks like `Intersection` and the `Mixed Workload` showcases the benefit of its cache-friendly memory layout. Iterating over the internal dense arrays is significantly faster than the pointer chasing required to traverse the nodes of an `IntMap`.

- **The Sparse Insertion Trade-off**: The table clearly shows the primary trade-off. `Data.IntMap.Strict` is the superior choice for workloads dominated by **sparse, ascending key insertions**, where it performs up to **10x faster**. This is because each such insert in `heph-sparse-set` can trigger a costly reallocation of the internal sparse array. Interestingly, insertions in _descending_ sparse order are much faster in `heph-sparse-set` (`~14.6 ms`) because the sparse array is allocated once to its maximum required size and then filled, avoiding repeated reallocations.

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
