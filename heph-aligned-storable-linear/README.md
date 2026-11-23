# heph-aligned-storable-linear

`AlignedStorable` instances for the vectors and matrices of
[linear](https://hackage.haskell.org/package/linear), so that structs of linear
types get GPU layouts (`std140`, `std430`, `scalar`) from
[`heph-aligned-storable`](../heph-aligned-storable).

## Quick Start

**IMPORTANT**: Be sure to use `layout(row_major)` if you are going to use `linear`
with GLSL. This library preserves `linear`'s row major semantics.

GLSL:

```glsl
layout(std140, row_major, binding = 0) uniform myuniforms {
  mat4 modelViewProjection;
  vec3 cameraPosition;
  float time;
};
```

Haskell:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.Aligned.Linear ()
import Foreign.GPU.Marshal.Aligned
import GHC.Generics (Generic)
import Linear (M44, V3(..), V4(..))

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

## Gotchas

### Matrix naming conventions

`linear` uses `Mnm` for n rows × m columns. GLSL uses `matNxM` for N columns of M-vectors.

- `M32 Float` (3 rows, 2 cols) → `mat2x3`
- `M24 Double` (2 rows, 4 cols) → `dmat4x2`

### `row_major`

GLSL's `layout(row_major)` affects memory layout, not matrix semantics. Matrices
are still column-major for arithmetic. This library implements the memory layout
correctly. You don't need to transpose before upload.

### `vec3` and `mat3` are cursed

Driver handling of the round-up rules for these types has historically been inconsistent. Consider padding to `vec4`/`mat4` and pretending the 3-element variants don't exist.

## Tests

The test suite runs the shared conformance suites and inspection tests of
[`heph-aligned-storable-testlib`](../heph-aligned-storable-testlib) on the
linear types, with `V2` to `V4` and every `Mnm` as the vector and matrix types.
