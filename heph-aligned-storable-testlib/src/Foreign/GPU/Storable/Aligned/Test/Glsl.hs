{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Description : GLSL scalars, vectors and matrices as seen by the oracles
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- The oracles never use a vector's or matrix's own 'Storable' instance. They
-- write one component at a time, at the offsets GLSL gives each component, so
-- the same oracle checks any vector library: a downstream package only says how
-- to take its vectors and matrices apart ('GlslVec', 'GlslMat').
module Foreign.GPU.Storable.Aligned.Test.Glsl (
  -- * Components
  Component (..),

  -- * Vectors
  GlslVec (..),
  componentArity,
  genVec,
  peekVecAt,
  pokeVecAt,

  -- * Matrices
  GlslMat (..),
  genMat,
  matVecStride,
  peekMatAt,
  pokeMatAt,

  -- * Arrays
  peekArrayAt,
  pokeArrayAt,
)
where

import Control.Monad (replicateM, zipWithM_)
import Data.Int
import Data.Kind (Type)
import Data.Proxy
import Data.Typeable (Typeable)
import Data.Vector.Generic qualified as GV
import Data.Vector.Generic.Sized qualified as SGV
import Data.Word
import Foreign (Ptr, Storable, peekByteOff, pokeByteOff, sizeOf)
import Foreign.GPU.Storable.Aligned (MemoryLayout (..))
import GHC.TypeLits
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Numeric.Half (Half)

import Foreign.GPU.Storable.Aligned.Test.Harness

--------------------------------------------------------------------------------
-- Components
--------------------------------------------------------------------------------

-- | A GLSL scalar type that can be a vector or matrix component.
class (Storable e, Eq e, Show e, Typeable e) => Component e where
  -- | Generates any value of the type.
  genComponent :: Gen e

  -- | The distance in bytes between consecutive components of a vector of @e@
  -- under the given layout.
  componentStride :: MemoryLayout -> Proxy e -> Int
  componentStride _ _ = sizeOf (undefined :: e)

instance Component Bool where
  genComponent = Gen.bool

instance Component Half where
  genComponent = genFloat

instance Component Float where
  genComponent = genFloat

instance Component Double where
  genComponent = genFloat

instance Component Int32 where
  genComponent = genInt

instance Component Int64 where
  genComponent = genInt

instance Component Word32 where
  genComponent = genInt

instance Component Word64 where
  genComponent = genInt

-- The library gives 8- and 16-bit integers 4 bytes under Std140, so these
-- strides do too. glslang gives them their natural size in every layout.

instance Component Int8 where
  genComponent = genInt
  componentStride Std140 _ = 4
  componentStride _ _ = 1

instance Component Int16 where
  genComponent = genInt
  componentStride Std140 _ = 4
  componentStride _ _ = 2

instance Component Word8 where
  genComponent = genInt
  componentStride Std140 _ = 4
  componentStride _ _ = 1

instance Component Word16 where
  genComponent = genInt
  componentStride Std140 _ = 4
  componentStride _ _ = 2

--------------------------------------------------------------------------------
-- Vectors
--------------------------------------------------------------------------------

-- | A GLSL vector (@vecN@, @dvecN@, @ivecN@, @u16vec3@, @bvec3@, ...) of
-- 'VecDim' components of type 'VecElem'.
class (Component (VecElem v), KnownNat (VecDim v), Eq v, Show v, Typeable v) => GlslVec v where
  type VecElem v :: Type
  type VecDim v :: Nat

  -- | The components in @x, y, z, w@ order.
  toComponents :: v -> [VecElem v]

  -- | Inverse of 'toComponents'. May assume exactly 'VecDim' components; see
  -- 'componentArity'.
  fromComponents :: [VecElem v] -> v

-- | The error for a component list of the wrong length:
--
-- > fromComponents [x, y, z] = V3 x y z
-- > fromComponents xs = componentArity 3 xs
componentArity :: Int -> [a] -> b
componentArity n xs =
  error $ "fromComponents: expected " <> show n <> " components, got " <> show (length xs)

-- | Generates every component with 'genComponent'.
genVec :: forall v. (GlslVec v) => Gen v
genVec = fromComponents <$> replicateM (natInt @(VecDim v)) genComponent

-- | Read a vector at a byte offset, one component every 'componentStride' bytes.
peekVecAt :: forall v b. (GlslVec v) => MemoryLayout -> Ptr b -> Int -> IO v
peekVecAt l ptr off =
  fromComponents
    <$> traverse
      (\i -> peekByteOff ptr (off + i * componentStride l (Proxy @(VecElem v))))
      [0 .. natInt @(VecDim v) - 1]

-- | Write a vector at a byte offset, one component every 'componentStride' bytes.
pokeVecAt :: forall v b. (GlslVec v) => MemoryLayout -> Ptr b -> Int -> v -> IO ()
pokeVecAt l ptr off =
  zipWithM_
    (\i -> pokeByteOff ptr (off + i * componentStride l (Proxy @(VecElem v))))
    [0 ..]
    . toComponents

--------------------------------------------------------------------------------
-- Matrices
--------------------------------------------------------------------------------

-- | A GLSL matrix, described by the vectors it stores contiguously, in memory
-- order: 'MatVecs' vectors of 'MatVecDim' components each.
--
-- With @layout(row_major)@, a GLSL @matCxR@ stores R rows of C components, which
-- is linear's @M_RC@ (@V_R (V_C a)@). A library that stores columns describes
-- its columns instead and needs no @row_major@.
class
  (Component (MatElem m), KnownNat (MatVecs m), KnownNat (MatVecDim m), Eq m, Show m, Typeable m) =>
  GlslMat m
  where
  type MatElem m :: Type
  type MatVecs m :: Nat
  type MatVecDim m :: Nat

  -- | The storage vectors in memory order, each in component order.
  toVectors :: m -> [[MatElem m]]

  -- | Inverse of 'toVectors'. May assume the exact shape.
  fromVectors :: [[MatElem m]] -> m

-- | Generates every component with 'genComponent'.
genMat :: forall m. (GlslMat m) => Gen m
genMat =
  fromVectors <$> replicateM (natInt @(MatVecs m)) (replicateM (natInt @(MatVecDim m)) genComponent)

-- | The distance in bytes between consecutive storage vectors of a matrix,
-- given the layout, the number of components per vector and the component
-- size in bytes.
matVecStride :: MemoryLayout -> Int -> Int -> Int
matVecStride Std140 2 2 = 16
matVecStride Std430 2 2 = 4
matVecStride Std140 2 4 = 16
matVecStride Std430 2 4 = 8
matVecStride Std140 2 8 = 16
matVecStride Std430 2 8 = 16
matVecStride Std140 3 2 = 16
matVecStride Std430 3 2 = 8
matVecStride Std140 3 4 = 16
matVecStride Std430 3 4 = 16
matVecStride Std140 3 8 = 32
matVecStride Std430 3 8 = 32
matVecStride Std140 4 2 = 16
matVecStride Std430 4 2 = 8
matVecStride Std140 4 4 = 16
matVecStride Std430 4 4 = 16
matVecStride Std140 4 8 = 32
matVecStride Std430 4 8 = 32
matVecStride Scalar n e = n * e
matVecStride l n e =
  error $
    "matVecStride: no entry for "
      <> show l
      <> " with "
      <> show n
      <> " components of "
      <> show e
      <> " bytes"

-- | Read a matrix at a byte offset: storage vector @i@ at
-- @off + i * 'matVecStride'@, its components packed.
peekMatAt :: forall m b. (GlslMat m) => MemoryLayout -> Ptr b -> Int -> IO m
peekMatAt l ptr off =
  fromVectors
    <$> traverse
      (\i -> traverse (peekByteOff ptr . (off + i * vs +) . (* es)) [0 .. dim - 1])
      [0 .. natInt @(MatVecs m) - 1]
 where
  (dim, es, vs) = matShape @m l

-- | Write a matrix at a byte offset: storage vector @i@ at
-- @off + i * 'matVecStride'@, its components packed.
pokeMatAt :: forall m b. (GlslMat m) => MemoryLayout -> Ptr b -> Int -> m -> IO ()
pokeMatAt l ptr off =
  zipWithM_
    (\i -> zipWithM_ (pokeByteOff ptr . (off + i * vs +) . (* es)) [0 ..])
    [0 ..]
    . toVectors
 where
  (_, es, vs) = matShape @m l

matShape :: forall m. (GlslMat m) => MemoryLayout -> (Int, Int, Int)
matShape l =
  let dim = natInt @(MatVecDim m)
      es = componentStride l (Proxy @(MatElem m))
   in (dim, es, matVecStride l dim es)

--------------------------------------------------------------------------------
-- Arrays
--------------------------------------------------------------------------------

-- | Read a sized array at a byte offset, element @i@ at @off + i * stride@.
peekArrayAt
  :: forall n v a b
   . (KnownNat n, GV.Vector v a)
  => Int
  -> (Ptr b -> Int -> IO a)
  -> Ptr b
  -> Int
  -> IO (SGV.Vector v n a)
peekArrayAt stride peekElem ptr off =
  SGV.generateM \i -> peekElem ptr (off + fromIntegral i * stride)

-- | Write a sized array at a byte offset, element @i@ at @off + i * stride@.
pokeArrayAt
  :: forall n v a b
   . (KnownNat n, GV.Vector v a)
  => Int
  -> (Ptr b -> Int -> a -> IO ())
  -> Ptr b
  -> Int
  -> SGV.Vector v n a
  -> IO ()
pokeArrayAt stride pokeElem ptr off =
  SGV.imapM_ \i -> pokeElem ptr (off + fromIntegral i * stride)

natInt :: forall n. (KnownNat n) => Int
natInt = fromIntegral (natVal (Proxy @n))
