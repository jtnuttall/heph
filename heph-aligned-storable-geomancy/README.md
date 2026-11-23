# heph-aligned-storable-geomancy

`AlignedStorable` instances for [geomancy](https://hackage.haskell.org/package/geomancy).
Provides GPU layouts via [`heph-aligned-storable`](https://hackage.haskell.org/package/heph-aligned-storable).

These layouts can be used either alongside or in lieu of geomancy's
own  `gl-block` instances.

`heph-aligned-storable` is designed to be especially useful for
closed-domain, type-level GPU manipulation and/or eDSL implementations.

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

## Matrices: no `row_major`

Upload `Mat4`, `DMat4`, `Transform` and `DTransform` with GLSL's default
matrix layout.

Do not add `layout(row_major)`, in contrast to `linear`, which requires
`row_major`. geomancy's README says:

> Transforms produced, composed, and applied to mimic the GLSL order (col-major):
>
> - `vec4 vPosOut = P * V * M * vPosIn;`
> - `vPosOut = (p <> v <> m) !* vPosIn`
>
> This way you don't have to transpose your transforms or fiddle with layout
> annotations.

## Tests

See [`heph-aligned-storable-testlib`](https://hackage.haskell.org/package/heph-aligned-storable-testlib)
geomancy types, plus structs of its own for `Transform`, `DTransform`,
`Quaternion`, `DQuaternion`, `Point`, `Packed`, the 64-bit vectors, and a `mat4`
at a 4-byte offset under `scalar`. Their GLSL twins are in
[`glsl/Geomancy.glsl`](glsl/Geomancy.glsl), checked against glslang.
