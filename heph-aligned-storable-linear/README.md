# heph-aligned-storable-linear

`AlignedStorable` instances for the vectors and matrices of
[linear](https://hackage.haskell.org/package/linear), so that structs of linear
types get GPU layouts (`std140`, `std430`, `scalar`) from
[`heph-aligned-storable`](../heph-aligned-storable).

## Quick Start

**IMPORTANT**: Be sure to use `layout(row_major)` if you are using `linear` with this library.

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

## Covered types

Every type below has an instance under `Std140`, `Std430` and `Scalar`.

| linear | GLSL |
|---|---|
| `V2 Bool`, `V3 Bool`, `V4 Bool` | `bvec2`, `bvec3`, `bvec4` |
| `V2 Half` to `V4 Half` | `f16vec2` to `f16vec4` |
| `V2 Float` to `V4 Float` | `vec2` to `vec4` |
| `V2 Double` to `V4 Double` | `dvec2` to `dvec4` |
| `V2 Int8` to `V4 Int8`, `V2 Word8` to `V4 Word8` | `i8vec2` to `i8vec4`, `u8vec2` to `u8vec4` |
| `V2 Int16` to `V4 Int16`, `V2 Word16` to `V4 Word16` | `i16vec2` to `i16vec4`, `u16vec2` to `u16vec4` |
| `V2 Int32` to `V4 Int32`, `V2 Word32` to `V4 Word32` | `ivec2` to `ivec4`, `uvec2` to `uvec4` |
| `V2 Int64` to `V4 Int64`, `V2 Word64` to `V4 Word64` | `i64vec2` to `i64vec4`, `u64vec2` to `u64vec4` |
| `M22 Half` to `M44 Half`, all nine shapes | `f16mat2` to `f16mat4`, `f16matNxM` |
| `M22 Float` to `M44 Float` | `mat2` to `mat4`, `matNxM` |
| `M22 Double` to `M44 Double` | `dmat2` to `dmat4`, `dmatNxM` |

The shaders need the extension that goes with each type, for example
`GL_EXT_shader_explicit_arithmetic_types_int64` for the 64-bit integer vectors.
Matrix naming below says which `Mnm` is which `matNxM`.

## Gotchas

### Matrix naming conventions

`linear` uses `Mnm` for n rows × m columns. GLSL uses `matNxM` for N columns of M-vectors.

- `M32 Float` (3 rows, 2 cols) → `mat2x3`
- `M24 Double` (2 rows, 4 cols) → `dmat4x2`

### `row_major`

GLSL's `layout(row_major)` affects memory layout, not matrix semantics. Matrices are still column-major for arithmetic. This library implements the memory layout correctly. You don't need to transpose before upload.

### `vec3` and `mat3` are cursed

Driver handling of the round-up rules for these types has historically been inconsistent. Consider padding to `vec4`/`mat4` and pretending the 3-element variants don't exist.

## Tests

The test suite runs the shared conformance suites and inspection tests of
[`heph-aligned-storable-testlib`](../heph-aligned-storable-testlib) on the
linear types, with `V2` to `V4` and every `Mnm` as the vector and matrix types.
