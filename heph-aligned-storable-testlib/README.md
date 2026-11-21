# heph-aligned-storable-testlib

Conformance tests for [`heph-aligned-storable`](../heph-aligned-storable) and
the packages that give it instances for a vector library.

Every struct under test has a GLSL twin in
[`glsl/Conformance.glsl`](glsl/Conformance.glsl) and, per layout (`std140`,
`std430`, `scalar`), a hand-written oracle whose offsets, sizes and alignments
are copied from glslang's output for that twin. Each test pokes and peeks the
struct through its derived `AlignedStorable` instance and through the oracle,
in every combination, inside a buffer guarded by canaries, and compares the
bytes both write.

The oracles never use a vector's or matrix's own `Storable` instance. They
write one component at a time, so the same oracles check any vector library.

## What runs where

`heph-aligned-storable` cannot depend on this package from its own test suite
(cabal rejects the cycle), so this package's test suite tests it:

- `primSuites`: structs of scalars, nested structs and arrays of them, and the
  marshal tests. No vector library needed.
- Inspection tests for `NestedPrim` and `Stress DeepNest`.
- Until `heph-aligned-storable-linear` exists, the shared suites instantiated
  for `linear`.

## Testing an instance package

Pick a tag type, describe your vectors and matrices, and say which shape groups
you provide:

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

| Suite | Needs |
|---|---|
| `floatVecSuites`, `tailVec3Suite`, `marshalVecSuite` | `FloatVecs` |
| `uniformsSuite` | `FloatVecs`, `FloatMat4` |
| `dUniformsSuite` | `DoubleVecs`, `DoubleMat4` |
| `intVecStructSuite` | `Int32Vecs` |
| `complexPaddingSuite`, `d2Suite` | `FloatVecs`, `FloatMats` |
| `doubleTroubleSuite` | `DoubleVecs`, `DoubleMats` |
| `testNonSquareSuite` | `HalfMats`, `DoubleMats` |
| `kitchenSinkSuite` | `DoubleVecs`, `FloatMats`, `HalfMats`, `DoubleMats` |
| `complexVectorStructSuite` | `SmallIntVecs`, `DoubleVecs` |
| `insanitySuite` | `FloatVecs`, `SmallIntVecs`, `DoubleVecs`, `BoolVecs` |
| `allSuites` | all of the above |

A suite whose groups you do not implement does not compile; nothing is skipped
silently. `sutTests`, `at` and `Oracle` are exported, so a package can add
structs of its own.

Inspection tests are spliced into a module compiled with the inspection plugin;
see `Foreign.GPU.Storable.Aligned.Test.Inspection`.

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
