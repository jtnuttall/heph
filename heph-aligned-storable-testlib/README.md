# heph-aligned-storable-testlib

Conformance tests for [`heph-aligned-storable`](https://hackage.haskell.org/package/heph-aligned-storable) and
the packages that give it instances for a vector library.

Every record in this library has a reference GLSL `struct` in
[`glsl/Conformance.glsl`](glsl/Conformance.glsl). Hand-written oracles
derived from the spirv disassembly provided by glslang then
test the behavior.

The oracles shouldn't use a vector's or matrix's own `Storable` instance, and should
be written manually so that they can be used to check any vector library.

## Example

```haskell
import Foreign.GPU.Storable.Aligned.Test

data L

instance (Component a) => GlslVec (V3 a) where
  type VecElem (V3 a) = a
  type VecDim (V3 a) = 3
  toComponents (V3 x y z) = [x, y, z]
  fromComponents [x, y, z] = V3 x y z
  fromComponents xs = componentArity 3 xs
-- likewise V2 and V4, and GlslMat for the matrices (storage vectors in
-- memory order)

instance FloatVecs L where
  type Vec2 L = V2 Float
  type Vec3 L = V3 Float
  type Vec4 L = V4 Float

test_conformance :: TestTree
test_conformance = testGroup "linear" (floatVecSuites (Proxy @L))
```

## Adding a struct

1. Add the struct to `glsl/Conformance.glsl` and to its `BLOCKS` list.
2. Compile it and read the offsets and strides:

   ```sh
   glslangValidator --target-env vulkan1.3 -S vert -V glsl/Conformance.glsl -o Conformance.spv
   spirv-dis Conformance.spv
   ```

3. Write the Haskell twin and one `Storable (Oracle l T)` instance per layout
   with those numbers, and a suite with `sutTests`.
4. Add the struct to the verified list in the GLSL file's header.
