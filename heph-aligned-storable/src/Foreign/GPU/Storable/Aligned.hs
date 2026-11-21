{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Generically derive Storable instances for GPU memory layouts
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Derive @Storable@ instances that respect GPU memory layout rules (@std140@,
-- @std430@, @scalar@). Works with any product type that has a 'Generic' instance.
--
-- @
-- data Uniforms = Uniforms
--   { viewProj  :: M44 Float
--   , cameraPos :: V3 Float
--   } deriving Generic
--
-- instance AlignedStorable Std140 Uniforms
-- @
--
-- Then use "Foreign.GPU.Marshal.Aligned" to get properly-padded pointers:
--
-- @
-- withPacked \@Std140 uniforms $ \\ptr -> uploadToGPU ptr
-- @
--
-- __NB:__ @vec3@ and @mat3@ types have historically had driver bugs
-- around alignment. Prefer @vec4@/@mat4@ when possible.
module Foreign.GPU.Storable.Aligned (
  -- * The AlignedStorable class
  AlignedStorable (..),

  -- * Memory layout
  MemoryLayout (..),
  SMemoryLayout (..),
  KnownMemoryLayout (..),
  memoryLayoutVal,

  -- * Storable Wrappers
  Strided (..),
  StridedVector,
  Packed (..),
  AlignedArray (..),
  mkAlignedArray,
  withAlignedArray,
  AlignedPtr (..),

  -- * Memory layout rules
  MemoryLayoutRules (..),

  -- * Generics
  GAlignedStorable (..),
)
where

import Data.Int
import Data.Proxy
import Data.Word
import Linear (M22, M23, M24, M32, M33, M34, M42, M43, M44, V2 (..), V3 (..), V4 (..))
import Numeric.Half (Half)

import Foreign.GPU.Storable.Aligned.Internal

--------------------------------------------------------------------------------
-- Base instances for primitive shader types
--
-- CPP is the simple way to stamp out the instances. I'll probably switch to TH
-- at some point.
--------------------------------------------------------------------------------

-- schema: (MemoryLayout, Type, size, alignment)
#define FOR_EACH_STD140_PRIMITIVE(X) \
  X (Std140, Bool, 4, 4) \
  X (Std140, Half, 2, 2) \
  X (Std140, Float, 4, 4) \
  X (Std140, Double, 8, 8) \
  X (Std140, Int8, 4, 4) \
  X (Std140, Int16, 4, 4) \
  X (Std140, Int32, 4, 4) \
  X (Std140, Int64, 8, 8) \
  X (Std140, Word8, 4, 4) \
  X (Std140, Word16, 4, 4) \
  X (Std140, Word32, 4, 4) \
  X (Std140, Word64, 8, 8)

-- schema: (MemoryLayout, Type, size, alignment)
#define FOR_EACH_STD430_PRIMITIVE(X) \
  X (Std430, Bool, 4, 4) \
  X (Std430, Half, 2, 2) \
  X (Std430, Float, 4, 4) \
  X (Std430, Double, 8, 8) \
  X (Std430, Int8, 1, 1) \
  X (Std430, Int16, 2, 2) \
  X (Std430, Int32, 4, 4) \
  X (Std430, Int64, 8, 8) \
  X (Std430, Word8, 1, 1) \
  X (Std430, Word16, 2, 2) \
  X (Std430, Word32, 4, 4) \
  X (Std430, Word64, 8, 8)

-- schema: (MemoryLayout, Type, size, alignment)
#define FOR_EACH_SCALAR_PRIMITIVE(X) \
  X (Scalar, Bool, 4, 4) \
  X (Scalar, Half, 2, 2) \
  X (Scalar, Float, 4, 4) \
  X (Scalar, Double, 8, 8) \
  X (Scalar, Int8, 1, 1) \
  X (Scalar, Int16, 2, 2) \
  X (Scalar, Int32, 4, 4) \
  X (Scalar, Int64, 8, 8) \
  X (Scalar, Word8, 1, 1) \
  X (Scalar, Word16, 2, 2) \
  X (Scalar, Word32, 4, 4) \
  X (Scalar, Word64, 8, 8)

-- schema: (Type)
#define FOR_EACH_MATRIX_PRIMITIVE(X) \
  X (Half) \
  X (Float) \
  X (Double)

#define PRIMITIVE_INSTANCE(LAYOUT, T, S, A) \
instance AlignedStorable LAYOUT T where { \
  packedAlignedSizeOf _ _ = S; \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf _ _ = S; \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment _ _ = A; \
  {-# INLINE alignedAlignment #-}; \
  alignedPeek = defaultAlignedPeek; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = defaultAlignedPoke; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(LAYOUT, T, S, A) PRIMITIVE_INSTANCE(LAYOUT, T, S, A)
FOR_EACH_STD140_PRIMITIVE (X)
FOR_EACH_STD430_PRIMITIVE (X)
FOR_EACH_SCALAR_PRIMITIVE (X)
#undef X

--------------------------------------------------------------------------------
-- Base instances for vectors and matrices
--------------------------------------------------------------------------------

#define VEC2_STD_INSTANCE(LAYOUT, T) \
instance AlignedStorable LAYOUT (V2 T) where { \
  packedAlignedSizeOf _ _ = 2 * alignedSizeOf (Proxy :: Proxy LAYOUT) (Proxy :: Proxy T); \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf _ _ = 2 * alignedSizeOf (Proxy :: Proxy LAYOUT) (Proxy :: Proxy T); \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment _ _ = 2 * alignedAlignment (Proxy :: Proxy LAYOUT) (Proxy :: Proxy T); \
  {-# INLINE alignedAlignment #-}; \
  alignedPeek = alignedPeekV2; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeV2; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(LAYOUT, T, S, A) VEC2_STD_INSTANCE(LAYOUT, T)
FOR_EACH_STD140_PRIMITIVE (X)
FOR_EACH_STD430_PRIMITIVE (X)
#undef X

#define VEC2_SCALAR_INSTANCE(T) \
instance AlignedStorable Scalar (V2 T) where { \
  packedAlignedSizeOf _ _ = 2 * alignedSizeOf (Proxy :: Proxy Scalar) (Proxy :: Proxy T); \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf _ _ = 2 * alignedSizeOf (Proxy :: Proxy Scalar) (Proxy :: Proxy T); \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment _ _ = alignedAlignment (Proxy :: Proxy Scalar) (Proxy :: Proxy T); \
  alignedPeek = defaultAlignedPeek; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = defaultAlignedPoke; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(LAYOUT, T, S, A) VEC2_SCALAR_INSTANCE(T)
FOR_EACH_SCALAR_PRIMITIVE (X)
#undef X

#define MAT2_STD_INSTANCE(LAYOUT, T) \
instance AlignedStorable LAYOUT (M22 T) where { \
  packedAlignedSizeOf l a = 2 * packedAlignedSizeOfMat l a; \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf = packedAlignedSizeOf; \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment l _ = alignBlock l (alignedAlignment l (Proxy :: Proxy (V2 T))); \
  {-# INLINE alignedAlignment #-}; \
  alignedPeek = alignedPeekM2; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeM2; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(T) MAT2_STD_INSTANCE(Std140, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define X(T) MAT2_STD_INSTANCE(Std430, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

-- For Scalar layout, matrices are trivially the same as nested vectors
#define X(T) VEC2_SCALAR_INSTANCE((V2 T))
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define MAT23_STD_INSTANCE(LAYOUT, T) \
instance AlignedStorable LAYOUT (M23 T) where { \
  packedAlignedSizeOf l a = 2 * packedAlignedSizeOfMat l a; \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf = packedAlignedSizeOf; \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment l _ = alignBlock l (alignedAlignment l (Proxy :: Proxy (V3 T))); \
  {-# INLINE alignedAlignment #-}; \
  alignedPeek = alignedPeekM2; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeM2; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(T) MAT23_STD_INSTANCE(Std140, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define X(T) MAT23_STD_INSTANCE(Std430, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

-- For Scalar layout, matrices are trivially the same as nested vectors
#define X(T) VEC2_SCALAR_INSTANCE((V3 T))
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define MAT24_STD_INSTANCE(LAYOUT, T) \
instance AlignedStorable LAYOUT (M24 T) where { \
  packedAlignedSizeOf l a = 2 * packedAlignedSizeOfMat l a; \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf = packedAlignedSizeOf ; \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment l _ = alignBlock l (alignedAlignment l (Proxy :: Proxy (V4 T))); \
  {-# INLINE alignedAlignment #-}; \
  alignedPeek = alignedPeekM2; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeM2; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(T) MAT24_STD_INSTANCE(Std140, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define X(T) MAT24_STD_INSTANCE(Std430, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

-- For Scalar layout, matrices are trivially the same as nested vectors
#define X(T) VEC2_SCALAR_INSTANCE((V4 T))
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define VEC3_STD_INSTANCE(LAYOUT, A) \
instance AlignedStorable LAYOUT (V3 A) where { \
  packedAlignedSizeOf l _ = 3 * alignedSizeOf l (Proxy :: Proxy A); \
  {-# INLINE packedAlignedSizeOf #-} ; \
  alignedSizeOf l _ = 3 * alignedSizeOf l (Proxy :: Proxy A); \
  alignedAlignment l _ = 4 * alignedAlignment l (Proxy :: Proxy A); \
  {-# INLINE alignedAlignment #-} ; \
  alignedPeek = alignedPeekV3; \
  {-# INLINE alignedPeek #-} ; \
  alignedPoke = alignedPokeV3; \
    {-# INLINE alignedPoke #-} ; \
  }; \

#define X(LAYOUT, T, S, A) VEC3_STD_INSTANCE(LAYOUT, T)
FOR_EACH_STD140_PRIMITIVE (X)
FOR_EACH_STD430_PRIMITIVE (X)
#undef X

#define VEC3_SCALAR_INSTANCE(A) \
instance AlignedStorable Scalar (V3 A) where { \
  packedAlignedSizeOf _ _ = 3 * alignedSizeOf (Proxy :: Proxy Scalar) (Proxy :: Proxy A); \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf _ _ = 3 * alignedSizeOf (Proxy :: Proxy Scalar) (Proxy :: Proxy A); \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment _ _ = alignedAlignment (Proxy :: Proxy Scalar) (Proxy :: Proxy A); \
  {-# INLINE alignedAlignment #-}; \
  alignedPeek = defaultAlignedPeek; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = defaultAlignedPoke; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(LAYOUT, T, S, A) VEC3_SCALAR_INSTANCE(T)
FOR_EACH_SCALAR_PRIMITIVE (X)
#undef X

#define MAT3_STD_INSTANCE(LAYOUT, A) \
instance AlignedStorable LAYOUT (M33 A) where { \
  packedAlignedSizeOf l a = 3 * packedAlignedSizeOfMat l a; \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf = packedAlignedSizeOf; \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment l _ = alignBlock l (alignedAlignment l (Proxy :: Proxy (V3 A))); \
  {-# INLINE alignedAlignment #-} ; \
  alignedPeek = alignedPeekM3; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeM3; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(T) MAT3_STD_INSTANCE(Std140, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define X(T) MAT3_STD_INSTANCE(Std430, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

-- For Scalar layout, matrices are trivially the same as nested vectors
#define X(T) VEC3_SCALAR_INSTANCE((V3 T))
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define MAT32_STD_INSTANCE(LAYOUT, A) \
instance AlignedStorable LAYOUT (M32 A) where { \
  packedAlignedSizeOf l a = 3 * packedAlignedSizeOfMat l a ; \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf = packedAlignedSizeOf; \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment l _ = alignBlock l (alignedAlignment l (Proxy :: Proxy (V2 A))); \
  {-# INLINE alignedAlignment #-} ; \
  alignedPeek = alignedPeekM3; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeM3; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(T) MAT32_STD_INSTANCE(Std140, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define X(T) MAT32_STD_INSTANCE(Std430, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

-- For Scalar layout, matrices are trivially the same as nested vectors
#define X(T) VEC3_SCALAR_INSTANCE((V2 T))
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define MAT34_STD_INSTANCE(LAYOUT, A) \
instance AlignedStorable LAYOUT (M34 A) where { \
  packedAlignedSizeOf l a = 3 * packedAlignedSizeOfMat l a; \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf = packedAlignedSizeOf ; \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment l _ = alignBlock l (alignedAlignment l (Proxy :: Proxy (V4 A))); \
  {-# INLINE alignedAlignment #-} ; \
  alignedPeek = alignedPeekM3; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeM3; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(T) MAT34_STD_INSTANCE(Std140, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define X(T) MAT34_STD_INSTANCE(Std430, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

-- For Scalar layout, matrices are trivially the same as nested vectors
#define X(T) VEC3_SCALAR_INSTANCE((V4 T))
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define VEC4_STD_INSTANCE(LAYOUT, A) \
instance AlignedStorable LAYOUT (V4 A) where { \
  packedAlignedSizeOf l _ = 4 * alignedSizeOf l (Proxy :: Proxy A); \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf l _ = 4 * alignedSizeOf l (Proxy :: Proxy A); \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment l _ = 4 * alignedAlignment l (Proxy :: Proxy A); \
  {-# INLINE alignedAlignment #-} ; \
  alignedPeek = alignedPeekV4; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeV4; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(LAYOUT, T, S, A) VEC4_STD_INSTANCE(LAYOUT, T)
FOR_EACH_STD140_PRIMITIVE (X)
FOR_EACH_STD430_PRIMITIVE (X)
#undef X

#define VEC4_SCALAR_INSTANCE(A) \
instance AlignedStorable Scalar (V4 A) where { \
  packedAlignedSizeOf _ _ = 4 * alignedSizeOf (Proxy :: Proxy Scalar) (Proxy :: Proxy A); \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf _ _ = 4 * alignedSizeOf (Proxy :: Proxy Scalar) (Proxy :: Proxy A); \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment _ _ = alignedAlignment (Proxy :: Proxy Scalar) (Proxy :: Proxy A); \
  {-# INLINE alignedAlignment #-}; \
  alignedPeek = defaultAlignedPeek; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = defaultAlignedPoke; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(LAYOUT, T, S, A) VEC4_SCALAR_INSTANCE(T)
FOR_EACH_SCALAR_PRIMITIVE (X)
#undef X

#define MAT4_STD_INSTANCE(LAYOUT, A) \
instance AlignedStorable LAYOUT (M44 A) where { \
  packedAlignedSizeOf l a = 4 * packedAlignedSizeOfMat l a; \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf = packedAlignedSizeOf; \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment l _ = alignBlock l (alignedAlignment l (Proxy :: Proxy (V4 A))); \
  {-# INLINE alignedAlignment #-}; \
  alignedPeek = alignedPeekM4; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeM4; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(T) MAT4_STD_INSTANCE(Std140, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define X(T) MAT4_STD_INSTANCE(Std430, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

-- For Scalar layout, matrices are trivially the same as nested vectors
#define X(T) VEC4_SCALAR_INSTANCE((V4 T))
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define MAT42_STD_INSTANCE(LAYOUT, A) \
instance AlignedStorable LAYOUT (M42 A) where { \
  packedAlignedSizeOf l a = 4 * packedAlignedSizeOfMat l a; \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf = packedAlignedSizeOf; \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment l _ = alignBlock l (alignedAlignment l (Proxy :: Proxy (V2 A))); \
  {-# INLINE alignedAlignment #-}; \
  alignedPeek = alignedPeekM4; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeM4; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(T) MAT42_STD_INSTANCE(Std140, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define X(T) MAT42_STD_INSTANCE(Std430, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

-- For Scalar layout, matrices are trivially the same as nested vectors
#define X(T) VEC4_SCALAR_INSTANCE((V2 T))
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define MAT43_STD_INSTANCE(LAYOUT, A) \
instance AlignedStorable LAYOUT (M43 A) where { \
  packedAlignedSizeOf l a = 4 * packedAlignedSizeOfMat l a; \
  {-# INLINE packedAlignedSizeOf #-}; \
  alignedSizeOf = packedAlignedSizeOf; \
  {-# INLINE alignedSizeOf #-}; \
  alignedAlignment l _ = alignBlock l (alignedAlignment l (Proxy :: Proxy (V3 A))); \
  {-# INLINE alignedAlignment #-}; \
  alignedPeek = alignedPeekM4; \
  {-# INLINE alignedPeek #-}; \
  alignedPoke = alignedPokeM4; \
  {-# INLINE alignedPoke #-}; \
  };

#define X(T) MAT43_STD_INSTANCE(Std140, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

#define X(T) MAT43_STD_INSTANCE(Std430, T)
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X

-- For Scalar layout, matrices are trivially the same as nested vectors
#define X(T) VEC4_SCALAR_INSTANCE((V3 T))
FOR_EACH_MATRIX_PRIMITIVE (X)
#undef X
