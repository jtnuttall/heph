{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Description : Shape groups: the vector and matrix types a library provides
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Every shared struct under test takes a tag type @lib@, chosen by the package
-- that runs the tests, and names its vector and matrix fields through the
-- associated types of these classes (@'Vec3' lib@, @'Mat4' lib@, ...). A package
-- opts into a group by writing an instance for its tag; a suite whose groups are
-- missing does not compile, so nothing is skipped silently.
--
-- Names follow GLSL: @MatCxR@ is GLSL @matCxR@ (C columns, R rows), described
-- to 'GlslMat' in memory order.
--
-- > data L
-- > instance FloatVecs L where
-- >   type Vec2 L = V2 Float
-- >   type Vec3 L = V3 Float
-- >   type Vec4 L = V4 Float
module Foreign.GPU.Storable.Aligned.Test.Vocab (
  -- * Constraint synonyms
  Aligned3,
  VecOf,
  MatOf,

  -- * Shape groups
  FloatVecs (..),
  FloatMat4 (..),
  FloatMats (..),
  DoubleVecs (..),
  DoubleMat4 (..),
  DoubleMats (..),
  HalfMats (..),
  SmallIntVecs (..),
  BoolVecs (..),
  Int32Vecs (..),
)
where

import Data.Int
import Data.Kind (Type)
import Data.Typeable (Typeable)
import Data.Word
import Foreign (Storable)
import Numeric.Half (Half)

import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.Aligned.Test.Glsl

-- | An 'AlignedStorable' instance for each of the three layouts.
type Aligned3 a = (AlignedStorable Std140 a, AlignedStorable Std430 a, AlignedStorable Scalar a)

-- | @v@ is a GLSL vector of @n@ components of type @e@.
type VecOf v e n = (GlslVec v, VecElem v ~ e, VecDim v ~ n, Aligned3 v)

-- | @m@ is a GLSL matrix stored as @r@ vectors of @c@ components of type @e@.
type MatOf m e r c = (GlslMat m, MatElem m ~ e, MatVecs m ~ r, MatVecDim m ~ c, Aligned3 m)

-- | @vec2@, @vec3@ and @vec4@. @'Vec3' lib@ and @'Vec4' lib@ also need a
-- 'Storable' instance (any; it only backs a Haskell-side
-- @Data.Vector.Storable.Sized.Vector@).
class
  ( Typeable lib
  , VecOf (Vec2 lib) Float 2
  , VecOf (Vec3 lib) Float 3
  , Storable (Vec3 lib)
  , VecOf (Vec4 lib) Float 4
  , Storable (Vec4 lib)
  ) =>
  FloatVecs lib
  where
  type Vec2 lib :: Type
  type Vec3 lib :: Type
  type Vec4 lib :: Type

-- | @mat4@.
class (Typeable lib, MatOf (Mat4 lib) Float 4 4) => FloatMat4 lib where
  type Mat4 lib :: Type

-- | @mat2@, @mat3@ and @mat3x4@ (four vectors of three components in memory
-- under @row_major@).
class
  ( FloatMat4 lib
  , MatOf (Mat2 lib) Float 2 2
  , MatOf (Mat3 lib) Float 3 3
  , MatOf (Mat3x4 lib) Float 4 3
  ) =>
  FloatMats lib
  where
  type Mat2 lib :: Type
  type Mat3 lib :: Type
  type Mat3x4 lib :: Type

-- | @dvec2@, @dvec3@ and @dvec4@. @'DVec2' lib@ also needs a 'Storable'
-- instance (any; it only backs a Haskell-side sized vector).
class
  ( Typeable lib
  , VecOf (DVec2 lib) Double 2
  , Storable (DVec2 lib)
  , VecOf (DVec3 lib) Double 3
  , VecOf (DVec4 lib) Double 4
  ) =>
  DoubleVecs lib
  where
  type DVec2 lib :: Type
  type DVec3 lib :: Type
  type DVec4 lib :: Type

-- | @dmat4@.
class (Typeable lib, MatOf (DMat4 lib) Double 4 4) => DoubleMat4 lib where
  type DMat4 lib :: Type

-- | @dmat3@, @dmat3x4@, @dmat4x3@ and @dmat2x4@, with @row_major@ storage
-- shapes 3×3, 4×3, 3×4 and 4×2.
class
  ( DoubleMat4 lib
  , MatOf (DMat3 lib) Double 3 3
  , MatOf (DMat3x4 lib) Double 4 3
  , MatOf (DMat4x3 lib) Double 3 4
  , MatOf (DMat2x4 lib) Double 4 2
  ) =>
  DoubleMats lib
  where
  type DMat3 lib :: Type
  type DMat3x4 lib :: Type
  type DMat4x3 lib :: Type
  type DMat2x4 lib :: Type

-- | @f16mat2x3@, @f16mat3x2@, @f16mat4@ and @f16mat4x2@, with @row_major@
-- storage shapes 3×2, 2×3, 4×4 and 2×4.
class
  ( Typeable lib
  , MatOf (F16Mat2x3 lib) Half 3 2
  , MatOf (F16Mat3x2 lib) Half 2 3
  , MatOf (F16Mat4 lib) Half 4 4
  , MatOf (F16Mat4x2 lib) Half 2 4
  ) =>
  HalfMats lib
  where
  type F16Mat2x3 lib :: Type
  type F16Mat3x2 lib :: Type
  type F16Mat4 lib :: Type
  type F16Mat4x2 lib :: Type

-- | @u16vec3@. Also needs a 'Storable' instance (any; it only backs a
-- Haskell-side sized vector).
class (Typeable lib, VecOf (U16Vec3 lib) Word16 3, Storable (U16Vec3 lib)) => SmallIntVecs lib where
  type U16Vec3 lib :: Type

-- | @bvec3@.
class (Typeable lib, VecOf (BVec3 lib) Bool 3) => BoolVecs lib where
  type BVec3 lib :: Type

-- | @ivec2@ to @ivec4@ and @uvec2@ to @uvec4@. @'IVec3' lib@ also needs a
-- 'Storable' instance (any; it only backs a Haskell-side sized vector).
class
  ( Typeable lib
  , VecOf (IVec2 lib) Int32 2
  , VecOf (IVec3 lib) Int32 3
  , Storable (IVec3 lib)
  , VecOf (IVec4 lib) Int32 4
  , VecOf (UVec2 lib) Word32 2
  , VecOf (UVec3 lib) Word32 3
  , VecOf (UVec4 lib) Word32 4
  ) =>
  Int32Vecs lib
  where
  type IVec2 lib :: Type
  type IVec3 lib :: Type
  type IVec4 lib :: Type
  type UVec2 lib :: Type
  type UVec3 lib :: Type
  type UVec4 lib :: Type
