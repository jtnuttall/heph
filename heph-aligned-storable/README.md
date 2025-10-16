# heph-aligned-storable

<!--toc:start-->

- [heph-aligned-storable](#heph-aligned-storable)
  - [What it gives you](#what-it-gives-you)
  - [Quick Start](#quick-start)
  - [Design Philosophy & Correct Usage](#design-philosophy-correct-usage)
    - [Why does this exist?](#why-does-this-exist)
    - [Why not `derive-storable`?](#why-not-derive-storable)
    - [The `Storable` Contract & Memory Initialization](#the-storable-contract-memory-initialization)
      - [Guaranteed Safe Operations](#guaranteed-safe-operations)
      - [Unsafe Operations (Require Manual Initialization)](#unsafe-operations-require-manual-initialization)
    - [Opt-in `memcpy` with `AlignedArray`](#opt-in-memcpy-with-alignedarray)
  - [Common Pitfalls & GLSL Mapping](#common-pitfalls-glsl-mapping)
    - [Matrix Dimensions: `Mnm` vs. `matNxM`](#matrix-dimensions-mnm-vs-matnxm)
    - [The Meaning of `layout(row_major)`](#the-meaning-of-layoutrowmajor)
  - [An Opinionated, "Batteries Included" Design](#an-opinionated-batteries-included-design)
  <!--toc:end-->

A Haskell library to generically derive `Storable` instances suitable for
CPU-GPU transfer, with a focus on correctness, performance, and type-safety.

[![CI](https://github.com/jtnuttall/heph/actions/workflows/haskell.yml/badge.svg)](https://github.com/jtnuttall/heph/actions/workflows/haskell.yml)
<!-- [![Hackage](https://img.shields.io/hackage/v/heph-aligned-storable.svg)](https://hackage.haskell.org/package/heph-aligned-storable) -->
<!-- Uncomment after publishing to Hackage -->

## What it gives you

- Upload Haskell data types to the GPU with correct, spec-compliant padding.
- Use a single `memcpy` for entire arrays of complex structs.
- Leverage type-level witnesses (`Std140`, `Std430`, `Scalar`) to prevent layout
  errors at compile time.
- Achieve the performance of a hand-written `Storable` instance. The generic
  machinery is fully eliminated at compile time.

## Quick Start

Define your type, derive `Generic`, and give it an `AlignedStorable` instance
for your target layout. The library handles the rest.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Marshal.Aligned
import GHC.Generics (Generic)
import Linear (V3, M44)

-- 1. Your data type
data Uniforms = Uniforms
  { modelViewProjection :: M44 Float
  , cameraPosition      :: V3 Float
  , time                :: Float
  } deriving (Generic, Show, Eq)

-- 2. An instance for your target layout
instance AlignedStorable Std140 Uniforms

-- 3. Upload it
main :: IO ()
main = do
  -- Demo values, none of these are actually valid in a real program :)
  let myUniforms =
        Uniforms
          { modelViewProjection =
              V4 (V4 1 2 3 4) (V4 5 6 7 8) (V4 9 10 11 12) (V4 13 14 15 16)
          , cameraPosition = V3 1 2 3
          , time = 12.12
          }
  -- 'withPacked' gives you a pointer to a single, correctly-padded struct.
  withPacked @Std140 myUniforms $ \ptr -> do
    -- ptr is ready to be used with vkCmdPushConstants or a mapped buffer copy.
    -- copyToGpuBuffer ptr ...
    pure ()
```

## Design Philosophy & Correct Usage

### Why does this exist?

Efficiently and correctly aligning values to GPU standards is a tricky, solved
problem in other languages. C++ has `glm`, Rust has `glam`. Haskell has the
necessary bits and pieces for robust linear algebra, but ensuring the memory
layout is both correct for the GPU and efficient to transfer is a largely
unexplored problem space. This library is designed to be Haskell's answer to
that problem.

It is built for high-performance GPU programming and adheres to a design
philosophy that prioritizes performance and explicit control, similar to `glm`.

### Why not `derive-storable`?

`derive-storable` is an excellent library, but it has a fundamentally different
purpose: creating `Storable` instances that are appropriate for FFI with C. This
does not work for the GPU. For example:

- The layout rules of the system C compiler will typically tightly pack structs,
  making generic instances improper for the padding rules of `std140` and
  `std430`.
- The `scalar` block layout rules also deviate from many system C compilers. For
  example, a `bool` in C is often 1 byte, but a `scalar`-layout `bool` must be 4
  bytes.

This library is built for high-performance GPU programming and adheres to a
design philosophy that prioritizes performance and explicit control, similar to
`glm`.

### The `Storable` Contract & Memory Initialization

This library is built for high-performance graphics and adheres to a strict
contract: the `poke` implementation for derived types **only writes member data.
It does not touch padding bytes.**

The consequence is that padding bytes within your struct can contain
uninitialized "garbage" data if you are not careful. For predictable results and
easier debugging with tools like RenderDoc, these padding bytes should be zero.

The Haskell ecosystem provides several ways to work with memory, with differing
initialization guarantees.

#### Guaranteed Safe Operations

These methods are the recommended "golden path" as they ensure that the memory
you work with is zero-initialized _before_ you poke your data into it,
preventing garbage in padding bytes.

1. **Use This Library's Helpers (Strongly Recommended):** The functions in
   `Foreign.GPU.Marshal.Aligned` (like `withPacked`, `newPacked`, etc.) are the
   simplest and safest way to work with pointers to single structs. They have
   been designed to explicitly zero-out the memory region before poking your
   data.

   ```haskell
   -- The memory pointed to by `ptr` is zeroed before `myUniforms` is poked.
   -- This is the library's recommended safe path.
   withPacked @Std140 myUniforms $ \ptr -> do
     -- Ready for immediate and safe use with the GPU.
     ...
   ```

2. **Use `Data.Vector.Storable`:** The `vector` library's safe API guarantees
   zero-initialization.

   - High-level creation functions like `SV.fromList` and `SV.replicate` are
     safe.
   - Low-level mutable creation via `Data.Vector.Storable.Mutable.new` is also
     safe.

3. **Use `calloc` for Manual Allocation:** If you really need to manage memory
   yourself, `Foreign.Marshal.Alloc.calloc` and `callocBytes` are standard
   library functions that provide a guaranteed zero-initialized block of memory
   from the heap.

#### Unsafe Operations (Require Manual Initialization)

The following common functions **do not** guarantee zero-initialization. If you
use them, you are responsible for manually zeroing the memory (e.g., with
`fillBytes`) before using this library's `poke`.

- `Foreign.Marshal.Alloc.malloc`
- `Foreign.Marshal.Alloc.alloca` (and by extension, the standard `with` function
  from `Foreign.Storable`)
- `Data.Vector.Storable.Mutable.unsafeNew`

Unless you have a specific reason to manage initialization yourself, you should
strongly prefer the guaranteed safe operations listed above.

### Opt-in `memcpy` with `AlignedArray`

For structs containing arrays of elements, the default `AlignedStorable`
instance will `poke` each element of the array individually. This provides
correct, if not maximally performant, behavior.

If your array can be treated as a single, contiguous block of bytes, you can
opt-in to a `memcpy`-based optimization by wrapping your sized vector in the
`AlignedArray` newtype. This is a deliberate design choice that makes the
high-performance `memcpy` path explicit to the user.

```haskell
data MyStruct (layout :: MemoryLayout) = MyStruct
  { meta :: Float
  , pixels :: AlignedArray layout 64 (V4 Float) -- This will be memcpy'd
  } deriving Generic

instance AlignedStorable Std140 (MyStruct Std140)
```

## Common Pitfalls & GLSL Mapping

When matching Haskell types to GLSL/SPIR-V, there are a few common sources of
error to be aware of.

### Matrix Dimensions: `Mnm` vs. `matNxM`

The `linear` library uses the naming convention `Mnm` for a matrix with **`n`
rows** and **`m` columns**. GLSL uses `matNxM` for a matrix with **`N` columns**
of **`M`-element vectors**.

This means the dimensions are swapped in the name. For example:

- A `linear` **`M32 Float`** (3 rows, 2 columns) maps to a GLSL **`mat2x3`**.
- A `linear` **`M24 Double`** (2 rows, 4 columns) maps to a GLSL **`dmat4x2`**.

This library correctly calculates the memory layout according to the GLSL
specification, but you must ensure you use the corresponding type in your shader
code.

### The Meaning of `layout(row_major)`

In GLSL, adding `layout(row_major)` to a uniform block **does not change GLSL's
column-major semantics for matrix math**. A matrix is still treated as an array
of column vectors. The `row_major` qualifier only affects how that matrix is
**laid out in memory**. Specifically, it means that the elements of each _row_
are contiguous in memory.

This library correctly implements the `row_major` memory layout rules. You do
not need to transpose your matrices on the CPU before uploading. `M44 Float` in
Haskell will correctly map to a `mat4` in a `row_major` buffer, with
multiplication `(matrix * vector)` working as expected in the shader.

## An Opinionated, "Batteries Included" Design

This library is opinionated in its choice of dependencies to provide a complete,
working solution out of the box. The core libraries chosen are de facto
standards in the high-performance Haskell ecosystem.

- **Linear Algebra:** Types for vectors and matrices are provided by
  **`linear`**. This is a robust, widely-used library whose performance
  characteristics are well understood.

- **Fixed-Size Arrays:** Fixed-size arrays within structs are represented by
  **`vector-sized`**. This allows for compile-time checking of array bounds and
  provides the necessary `KnownNat` constraints for generic layout calculation.

Be aware that these choices bring in a notable set of transitive dependencies
(e.g., from `lens`, `distributive`, `adjunctions`). This is the trade-off for
the type-safety and functionality that these libraries provide.
