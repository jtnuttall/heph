# heph-aligned-storable

Generically derive `Storable` instances for GPU memory layouts (`std140`, `std430`, `scalar`).

[![CI](https://github.com/jtnuttall/heph/actions/workflows/haskell.yml/badge.svg)](https://github.com/jtnuttall/heph/actions/workflows/haskell.yml)

<!-- [![Hackage](https://img.shields.io/hackage/v/heph-aligned-storable.svg)](https://hackage.haskell.org/package/heph-aligned-storable) -->

## Quick Start

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Marshal.Aligned
import GHC.Generics (Generic)
import Linear (M44, V3, V4(..))

data Uniforms = Uniforms
  { modelViewProjection :: M44 Float
  , cameraPosition      :: V3 Float
  , time                :: Float
  } deriving (Generic, Show, Eq)

instance AlignedStorable Std140 Uniforms

main :: IO ()
main = do
  let uniforms = Uniforms
        { modelViewProjection = V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 0 0 0 1)
        , cameraPosition = V3 0 0 5
        , time = 0
        }
  withPacked @Std140 uniforms $ \ptr -> do
    -- ptr is ready for vkCmdPushConstants, memcpy to mapped buffer, etc.
    pure ()
```

## Features

- Correct, spec-compliant padding for `Std140`, `Std430`, and `Scalar` layouts
- Single `memcpy` for arrays via `AlignedArray`
- Type-level layout witnesses prevent mismatched layouts at compile time
- Zero runtime overhead—generic machinery fully eliminated by GHC

## The Contract

**`alignedPoke` writes member data only. Padding bytes are untouched.**

Use the helpers in `Foreign.GPU.Marshal.Aligned` (`withPacked`, `allocaPacked`, etc.) for guaranteed zero-initialized padding. If you allocate memory yourself, use `calloc` or zero the buffer before poking.

## Arrays

By default, arrays are poked element-by-element. For a single `memcpy`, wrap in `AlignedArray`:

```haskell
data MyStruct (layout :: MemoryLayout) = MyStruct
  { meta   :: Float
  , pixels :: AlignedArray layout 64 (V4 Float)  -- memcpy'd as a block
  } deriving Generic

instance AlignedStorable Std140 (MyStruct Std140)
```

## Gotchas

### Matrix naming conventions

`linear` uses `Mnm` for n rows × m columns. GLSL uses `matNxM` for N columns of M-vectors.

- `M32 Float` (3 rows, 2 cols) → `mat2x3`
- `M24 Double` (2 rows, 4 cols) → `dmat4x2`

### `row_major` doesn't mean what you think

GLSL's `layout(row_major)` affects memory layout, not matrix semantics. Matrices are still column-major for arithmetic. This library implements the memory layout correctly—you don't need to transpose before upload.

### `vec3` and `mat3` are cursed

Driver handling of the round-up rules for these types has historically been inconsistent. Consider padding to `vec4`/`mat4` and pretending the 3-element variants don't exist.

## Why not `derive-storable`?

`derive-storable` produces FFI-compatible layouts (C struct ABI), not GPU layouts. GPU alignment rules differ:

- `std140` rounds struct alignment to 16 bytes
- `scalar` layout requires 4-byte booleans, not 1-byte
