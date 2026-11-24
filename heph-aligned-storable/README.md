# heph-aligned-storable

Generically derive `Storable` instances for GPU memory layouts (`std140`, `std430`, `scalar`).

[![CI](https://github.com/jtnuttall/heph/actions/workflows/ci.yml/badge.svg)](https://github.com/jtnuttall/heph/actions/workflows/ci.yml)

<!-- [![Hackage](https://img.shields.io/hackage/v/heph-aligned-storable.svg)](https://hackage.haskell.org/package/heph-aligned-storable) -->

This package has instances for the GLSL scalar types and for arrays. Instances for the
vector and matrix types of a math library live in a package of their own, so that this
one depends on none of them:

- [`heph-aligned-storable-linear`](../heph-aligned-storable-linear) for `linear`
- [`heph-aligned-storable-geomancy`](../heph-aligned-storable-geomancy) for `geomancy`

Those packages show a full quick start. The `Foreign.GPU.Storable.Aligned.TH` module
exports the generators they use, so a package can bind another library the same way.

The test suite lives in [`heph-aligned-storable-testlib`](../heph-aligned-storable-testlib),
which also runs it on each of the instance packages.

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
  { meta    :: Float
  , weights :: AlignedArray layout 64 Float  -- memcpy'd as a block
  } deriving Generic

instance AlignedStorable Std140 (MyStruct Std140)
```

## Gotchas

### `vec3` and `mat3` are cursed

Driver handling of the round-up rules for these types has historically been inconsistent. Consider padding to `vec4`/`mat4` and pretending the 3-element variants don't exist.

## Why not `derive-storable`?

`derive-storable` produces FFI-compatible layouts (C struct ABI), not GPU layouts. GPU alignment rules differ:

- `std140` rounds struct alignment to 16 bytes
- `scalar` layout requires 4-byte booleans, not 1-byte
