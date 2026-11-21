# heph-aligned-storable-geomancy

`AlignedStorable` instances for the types of
[geomancy](https://hackage.haskell.org/package/geomancy), so that structs of
geomancy vectors and matrices get GPU layouts (`std140`, `std430`, `scalar`)
from [`heph-aligned-storable`](../heph-aligned-storable).

## Usage

GLSL:

```glsl
layout(std140, binding = 0) uniform Uniforms {
  mat4 model;
  vec3 cameraPosition;
  float time;
};
```

Haskell:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import Foreign.GPU.Marshal.Aligned
import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.Aligned.Geomancy ()
import GHC.Generics (Generic)
import Geomancy.Transform (Transform)
import qualified Geomancy.Transform as Transform
import Geomancy.Vec3 (Vec3, vec3)

data Uniforms = Uniforms
  { model :: Transform
  , cameraPosition :: Vec3
  , time :: Float
  }
  deriving (Generic)

instance AlignedStorable Std140 Uniforms

main :: IO ()
main = do
  let uniforms =
        Uniforms
          { model = Transform.translate 1 2 3
          , cameraPosition = vec3 0 0 5
          , time = 0
          }
  withPacked @Std140 uniforms $ \ptr -> do
    -- ptr is ready for vkCmdPushConstants, memcpy to mapped buffer, etc.
    pure ()
```

`time` sits at offset 76, in the four bytes after `cameraPosition`.

## Covered types

Every type below has an instance under `Std140`, `Std430` and `Scalar`.

| geomancy | GLSL |
|---|---|
| `Vec2`, `Vec3`, `Vec4` | `vec2`, `vec3`, `vec4` |
| `DVec2`, `DVec3`, `DVec4` | `dvec2`, `dvec3`, `dvec4` |
| `IVec2`, `IVec3`, `IVec4` | `ivec2`, `ivec3`, `ivec4` |
| `UVec2`, `UVec3`, `UVec4` | `uvec2`, `uvec3`, `uvec4` |
| `I64Vec2`, `I64Vec3`, `I64Vec4` | `i64vec2`, `i64vec3`, `i64vec4` |
| `U64Vec2`, `U64Vec3`, `U64Vec4` | `u64vec2`, `u64vec3`, `u64vec4` |
| `Mat4`, `Transform` | `mat4` |
| `DMat4`, `DTransform` | `dmat4` |
| `Quaternion` | `vec4` |
| `DQuaternion` | `dvec4` |
| `Packed` of each 3-vector module (`Geomancy.Vec3.Packed`, ...) | the 3-vector it wraps |
| `Point v`, for each vector type `v` above | the same as `v` |

The 64-bit integer vectors need `GL_EXT_shader_explicit_arithmetic_types_int64`
in the shader. A `Quaternion`'s components are in the order of `quaternion`'s
arguments, scalar part first: a `Quaternion w x y z` lands in GLSL as
`vec4(w, x, y, z)`, so the scalar part is `.x`, not `.w` as most shader code
assumes. Likewise `DQuaternion`.

## Matrices: no `row_major`

Upload `Mat4`, `DMat4`, `Transform` and `DTransform` with GLSL's default
(column-major) matrix layout. Do not add `layout(row_major)`. geomancy's README
says:

> Transforms produced, composed, and applied to mimic the GLSL order (col-major):
>
> - `vec4 vPosOut = P * V * M * vPosIn;`
> - `vPosOut = (p <> v <> m) !* vPosIn`
>
> This way you don't have to transpose your transforms or fiddle with layout
> annotations.

The 16 floats of a `Mat4` are written in memory order, and that order already
is the column-major order GLSL reads by default. For example,
`Transform.translate x y z` stores `x`, `y`, `z` as floats 12 to 14, which GLSL
reads as the fourth column, where a translation belongs. With `row_major`, GLSL
would read the transpose.

`mat4` and `dmat4` have no padding in any layout (each column is exactly 16 or
32 bytes), so these instances copy the 64 or 128 bytes as they are. The other
GLSL matrix shapes have no geomancy counterpart.

## `Vec3` and `Packed`

geomancy's `Storable` instances pad the 3-vectors: `Vec3`, `IVec3` and `UVec3`
are 16 bytes, `DVec3`, `I64Vec3` and `U64Vec3` are 32 bytes, and the ones for
`Vec3` and `DVec3` also write `1.0` as a fourth component. The `Packed`
newtypes are exactly 12 (or 24) bytes. These
instances never use the `Storable` instance of a vector: they write the three
components one at a time. So `Vec3` and `Vec3.Packed` are both a GLSL `vec3`,
with the same size and alignment, and neither writes into the bytes that
follow, such as the `float` packed after a `vec3` in `std140`.

## Allocation

`Vec4`, `DVec4`, `Mat4` and `DMat4` are backed by a `ByteArray#`, so building
one allocates. Reading a struct that contains them (`alignedPeek`, `peek`)
allocates their arrays. Writing one (`alignedPoke`, `poke`) copies from the
array without allocating.

## Tests

The test suite runs the shared conformance suites of
[`heph-aligned-storable-testlib`](../heph-aligned-storable-testlib) on the
geomancy types, plus structs of its own for `Transform`, `DTransform`,
`Quaternion`, `DQuaternion`, `Point`, `Packed`, the 64-bit vectors, and a `mat4`
at a 4-byte offset under `scalar`. Their GLSL twins are in
[`glsl/Geomancy.glsl`](glsl/Geomancy.glsl), checked against glslang.
