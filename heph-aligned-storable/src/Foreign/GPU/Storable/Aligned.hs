{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Generically derive Storable instances suitable for CPU-GPU synchronization
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- This module contains everything you need to derive storable instances sufficient for
-- CPU-GPU transfer.
--
-- == Example
--
-- > data MyUniforms = MyUniforms
-- >   { cameraViewProj :: M44 Float
-- >   , cameraPos :: V3 Float
-- >   } deriving (Generic)
-- >
-- > instance AlignedStorable Std140 MyUniforms
-- >
-- > -- later
-- >
-- > withPacked (myUniforms) $ \ptr -> uploadBuffer buf ptr
--
-- NB: It's often best to pretend that `vec3` and `mat3` just don't exist in
-- your shaders. This library should implement their layout rules correctly, but
-- historically there has been inconsistency in driver handling of the round-up
-- rules.
module Foreign.GPU.Storable.Aligned (
  -- * The AlignedStorable class
  MemoryLayout (..),
  KnownMemoryLayout (..),
  AlignedStorable (..),

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

import Control.Monad
import Data.Bifunctor (first)
import Data.Bits
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Hashable (Hashable)
import Data.Int
import Data.Kind
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Typeable (Typeable)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Sized as SGV
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Mutable as SMV
import qualified Data.Vector.Storable.Sized as SSV
import Data.Word
import Foreign (Ptr, castPtr, copyBytes, plusPtr)
import Foreign.Storable
import GHC.Generics
import GHC.TypeLits
import Linear (M22, M23, M24, M32, M33, M34, M42, M43, M44, V2 (..), V3 (..), V4 (..))
import Numeric.Half (Half)

--------------------------------------------------------------------------------
-- Layout Rules
--------------------------------------------------------------------------------

-- | A type-level tag representing a memory layout standard.
data MemoryLayout
  = -- | `std140` is a standardized memory layout for SPIR-V interface blocks.
    -- It has strict padding and alignment rules, ensuring layout consistency across platforms.
    --
    -- For details, refer to 7.6.2.2 "Standard Uniform Block Layout" of the
    -- [OpenGL 4.6 Specification](https://registry.khronos.org/OpenGL/specs/gl/glspec46.core.pdf).
    Std140
  | -- | `std430` is a standardized memory layout, typically used for Shader Storage Buffer
    -- Objects (SSBOs). It has more relaxed alignment rules for arrays and structs
    -- than `std140`, which can result in more compact memory usage.
    --
    -- For details, please refer to the section 7.6.2.2 "Standard Uniform Block Layout" of the
    -- [OpenGL 4.6 Specification](https://registry.khronos.org/OpenGL/specs/gl/glspec46.core.pdf).
    Std430
  | -- | `scalar` block layout is a memory layout with the most relaxed alignment rules,
    -- closely matching the alignment of scalar and vector types in C.
    --
    -- For details, please refer to the
    -- [extension specification](https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GL_EXT_scalar_block_layout.txt)
    Scalar
  deriving (Show, Generic, Typeable, Enum, Bounded, Eq, Ord)

instance Hashable MemoryLayout

-- | This class gives the value associated with a type-level 'MemoryLayout'.
--
-- This is primarily useful for authors of higher-level libraries (e.g.,
-- shader eDSLs, descriptor set builders) who need to inspect the layout
-- at runtime to make decisions. It allows you to write functions that are
-- polymorphic over the layout but can still access a concrete representation
-- of which layout is being used.
--
-- A very rough sketch of this use case:
--
-- > emitLayoutQualifier :: forall layout. KnownMemoryLayout layout => String
-- > emitLayoutQualifier = case memoryLayoutVal (Proxy @layout) of
-- >   Std140 -> "layout(std140)"
-- >   Std430 -> "layout(std430)"
-- >   Scalar -> "layout(scalar)"
class KnownMemoryLayout (layout :: MemoryLayout) where
  memoryLayoutVal :: Proxy layout -> MemoryLayout

instance KnownMemoryLayout Std140 where
  memoryLayoutVal _ = Std140
  {-# INLINE memoryLayoutVal #-}

instance KnownMemoryLayout Std430 where
  memoryLayoutVal _ = Std430
  {-# INLINE memoryLayoutVal #-}

instance KnownMemoryLayout Scalar where
  memoryLayoutVal _ = Scalar
  {-# INLINE memoryLayoutVal #-}

-- | Defines the precise calculation rules for a given 'MemoryLayout'.
--
-- This typeclass serves as the low-level specification that underpins the
-- entire 'AlignedStorable' deriving mechanism. For authors of higher-level
-- libraries, you will not need to _implement_ instances of this class.
-- Instead, understanding these rules is key to trusting the output of functions
-- like 'alignedSizeOf' and 'layoutStride', which you may use to build your own
-- abstractions (like type-safe descriptor sets).
--
-- These rules directly correspond to the memory layout requirements found in
-- the OpenGL and Vulkan specifications.
class MemoryLayoutRules (layout :: MemoryLayout) where
  -- | The alignment rule for top-level structs, matrix rows, and array members.
  --
  -- This determines the final alignment of a composite type. For 'Std140', this
  -- means rounding the base alignment of the members up to a multiple of 16.
  -- For other layouts, it may be a no-op.
  alignBlock :: Proxy layout -> Int -> Int

  -- | The final size rule for a top-level struct.
  --
  -- This rule ensures the total size of a struct is a multiple of its final
  -- (potentially rounded) alignment. This is what adds the padding at the
  -- _end_ of a struct.
  roundStructSize :: Proxy layout -> Int -> Int -> Int

  -- | The stride rule for an element within an array.
  --
  -- This is a critical function for library authors. It determines the byte-spacing
  -- between consecutive elements in an array, which may be larger than the
  -- struct's padded size. For 'Std140', this stride is always rounded up to a
  -- multiple of 16.
  --
  -- When building descriptor sets or calculating buffer sizes for arrays of
  -- uniforms, the value returned by this function is the "true" size of each
  -- element from the GPU's perspective. The 'Storable' instance for the
  -- 'Strided' newtype uses this function to determine its 'sizeOf'.
  layoutStride :: Proxy layout -> Int -> Int -> Int

std140Align :: Int
std140Align = 16

instance MemoryLayoutRules Std140 where
  alignBlock _ = roundUpTo std140Align
  {-# INLINE alignBlock #-}
  roundStructSize _ = flip roundUpTo
  {-# INLINE roundStructSize #-}
  layoutStride _ size align = roundUpTo std140Align (roundUpTo align size)
  {-# INLINE layoutStride #-}

instance MemoryLayoutRules Std430 where
  alignBlock _ = id
  {-# INLINE alignBlock #-}
  roundStructSize _ = flip roundUpTo
  {-# INLINE roundStructSize #-}
  layoutStride _ = flip roundUpTo
  {-# INLINE layoutStride #-}

instance MemoryLayoutRules Scalar where
  alignBlock _ = id
  {-# INLINE alignBlock #-}
  roundStructSize _ size _ = size
  {-# INLINE roundStructSize #-}
  layoutStride _ = flip roundUpTo
  {-# INLINE layoutStride #-}

--------------------------------------------------------------------------------
-- Layout-aware Storable
--------------------------------------------------------------------------------

-- | A newtype wrapper that instructs the 'Storable' instance to include
-- full stride padding, making it suitable for use in arrays.
--
-- For example, when creating a @SV.Vector (Strided Std140 MyType)@, each element
-- will be spaced correctly in memory, allowing the entire vector to be copied
-- to the GPU in a single `memcpy`.
newtype Strided (layout :: MemoryLayout) a = Strided {unStrided :: a}
  deriving (Generic, Typeable, Show, Eq, Ord)

-- | A newtype wrapper that instructs the 'Storable' instance to use the
-- packed size of a type, without the final stride padding that 'Strided' adds.
-- This is useful for single, non-array uniform buffer objects.
newtype Packed (layout :: MemoryLayout) a = Packed {unPacked :: a}
  deriving (Generic, Typeable, Show, Eq, Ord)

-- | A convenience type for Data.Vector.Storable.Vector (Strided layout a)
type StridedVector (layout :: MemoryLayout) a = SV.Vector (Strided layout a)

-- | A newtype wrapper over fixed-size storable vectors that signals that the
-- entire array should be treated as a single, contiguous block of memory.
--
-- The 'AlignedStorable' instance for this type uses 'copyBytes' for its
-- 'alignedPoke' and 'alignedPeek' implementations. This provides a significant
-- performance boost for large arrays.
--
-- To use this type, you must parameterize your datatype by its layout.
newtype AlignedArray (layout :: MemoryLayout) (n :: Nat) a = AlignedArray
  {unAlignedArray :: SSV.Vector n (Strided layout a)}
  deriving (Generic, Typeable, Show, Eq, Ord)

-- | Construct an 'AlignedArray' from a sized vector. The resulting array
-- uses 'copyBytes' instead of individual pokes.
--
-- > data MyStruct layout = MyStruct
-- >   { metadata :: Float
-- >   , pixels :: AlignedArray layout 64 (V4 Float)
-- >   } deriving Generic
-- >
-- > instance AlignedStorable Std140 (MyStruct Std140)
mkAlignedArray
  :: (AlignedStorable layout a, Storable a) => SSV.Vector n a -> AlignedArray layout n a
mkAlignedArray = AlignedArray . SSV.map Strided
{-# INLINE CONLIKE mkAlignedArray #-}

-- | Helper function to unwrap an 'AlignedArray' and manipulate its contents.
--
-- __NOTE__: It is not safe to manipulate the underling Ptr or perform an unsafe
-- thaw or freeze during this operation.
withAlignedArray
  :: AlignedArray layout n a
  -> (SSV.Vector n (Strided layout a) -> SSV.Vector n (Strided layout a))
  -> AlignedArray layout n a
withAlignedArray (AlignedArray v) f = AlignedArray (f v)
{-# INLINE withAlignedArray #-}

-- | A newtype wrapper for a 'Ptr' that is tagged with a 'MemoryLayout'.
-- This is used internally by the generic machinery to pass the layout
-- context during peeking and poking operations.
newtype AlignedPtr (layout :: MemoryLayout) a = AlignedPtr {unAlignedPtr :: Ptr a}
  deriving (Generic, Typeable, Show)

type role AlignedPtr nominal nominal

-- | A class for types that have calculable layouts according to GPU requirements.
--
-- Instances can be derived generically for any data type that is an instance of
-- 'GHC.Generics.Generic'. The performance of a derived instance is equivalent
-- to a carefully hand-written one. All abstraction overhead from the generic
-- deriving mechanism is eliminated by GHC's optimizer. This is verified via
-- inspection testing.
--
-- === The `AlignedStorable` Contract
--
-- Instances of this class adhere to a specific contract designed for performance.
--
-- 1.  __`alignedPoke` only writes member data.__ To avoid performance penalties,
--     `alignedPoke` operations write *only* the bytes corresponding to the fields
--     of your type. They __will not__ initialize any padding bytes that may exist
--     within the type, _either between members or within the stride_.
-- 2.  __`alignedPeek` and `alignedPoke` are inverse operations__ for the data's members.
--     That is, `alignedPeek ptr` after `alignedPoke ptr x` will reconstruct `x`.
-- 3.  __Layouts are accurate.__ `packedAlignedSizeOf`, `alignedSizeOf`, and
--     `alignedAlignment` correctly represent the type's memory layout as
--     required by the given layout standard.
class (MemoryLayoutRules layout) => AlignedStorable (layout :: MemoryLayout) a where
  -- | The size of the type 'a' after its contents are laid out, but *before*
  -- any final padding is applied to the container struct itself. This is
  -- the offset after the last member.
  packedAlignedSizeOf :: Proxy layout -> Proxy a -> Int
  default packedAlignedSizeOf
    :: (GAlignedStorable layout (Rep a))
    => Proxy layout
    -> Proxy a
    -> Int
  packedAlignedSizeOf l _ = fst (galignedSize l (Proxy @(Rep a)) 0)
  {-# INLINE packedAlignedSizeOf #-}

  -- | The size of the type 'a' including final padding/rounding according to the
  -- layout rules for a struct. For `Std140`, this means the size is rounded
  -- up to a multiple of 16.
  alignedSizeOf :: Proxy layout -> Proxy a -> Int
  default alignedSizeOf
    :: (GAlignedStorable layout (Rep a))
    => Proxy layout
    -> Proxy a
    -> Int
  alignedSizeOf l _ =
    let (!s, !a) = galignedSize l (Proxy @(Rep a)) 0
        !structAlign = alignBlock l a
     in roundStructSize l s structAlign
  {-# INLINE alignedSizeOf #-}

  -- | The base alignment requirement for the type 'a'.
  alignedAlignment :: Proxy layout -> Proxy a -> Int
  default alignedAlignment
    :: (GAlignedStorable layout (Rep a))
    => Proxy layout
    -> Proxy a
    -> Int
  alignedAlignment l _ =
    let (_, !a) = galignedSize l (Proxy @(Rep a)) 0
     in alignBlock l a
  {-# INLINE alignedAlignment #-}

  -- | Read a value from the given pointer, respecting the layout rules.
  alignedPeek :: AlignedPtr layout a -> IO a
  default alignedPeek
    :: (Generic a, GAlignedStorable layout (Rep a))
    => AlignedPtr layout a
    -> IO a
  alignedPeek (AlignedPtr ptr) = to . fst <$> galignedPeek @layout (AlignedPtr (castPtr ptr)) 0
  {-# INLINE alignedPeek #-}

  -- | Write a value to the given pointer, respecting the layout rules.
  alignedPoke :: AlignedPtr layout a -> a -> IO ()
  default alignedPoke
    :: (Generic a, GAlignedStorable layout (Rep a))
    => AlignedPtr layout a
    -> a
    -> IO ()
  alignedPoke (AlignedPtr ptr) val = void $ galignedPoke @layout (AlignedPtr (castPtr ptr)) (from val) 0
  {-# INLINE alignedPoke #-}

-- | \$sizeOf @(Strided layout a)$ calculates the full stride of the type,
-- including final padding, making it suitable for array allocations.
--
-- This storable instance lies a bit about it size so that you can create a
-- \$SV.Vector (Strided layout a)$ and then do a straight 'copyBytes' on the
-- underlying 'Ptr'.
--
-- __This means that 'poke' does not zero out the remaining space in the buffer.__
-- It can't do so safely because it doesn't know whether you have allocated a singleton
-- buffer or an array.
instance (MemoryLayoutRules layout, AlignedStorable layout a) => Storable (Strided layout a) where
  sizeOf _ =
    layoutStride
      (Proxy @layout)
      (alignedSizeOf (Proxy @layout) (Proxy @a))
      (alignedAlignment (Proxy @layout) (Proxy @a))
  {-# INLINE sizeOf #-}
  alignment _ = alignedAlignment (Proxy @layout) (Proxy @a)
  {-# INLINE alignment #-}
  peek ptr = Strided <$> alignedPeek @layout (AlignedPtr (castPtr ptr))
  {-# INLINE peek #-}
  poke ptr a = alignedPoke @layout (AlignedPtr (castPtr ptr)) (coerce a :: a)
  {-# INLINE poke #-}

instance (AlignedStorable layout a) => Storable (Packed layout a) where
  sizeOf _ = alignedSizeOf (Proxy @layout) (Proxy @a)
  {-# INLINE sizeOf #-}
  alignment _ = alignedAlignment (Proxy @layout) (Proxy @a)
  {-# INLINE alignment #-}
  peek ptr = Packed <$> alignedPeek @layout (AlignedPtr (castPtr ptr))
  {-# INLINE peek #-}
  poke ptr a = alignedPoke @layout (AlignedPtr (castPtr ptr)) (coerce a :: a)
  {-# INLINE poke #-}

--------------------------------------------------------------------------------
-- Generic Deriving Machinery
--------------------------------------------------------------------------------

-- | Internal class for generic 'AlignedStorable' derivation. Operates on
-- 'GHC.Generics.Rep' to calculate layouts and implement peek/poke.
--
-- You don't need to interact with this unless you're writing custom instances
-- or debugging the generic machinery.
--
-- > data MyType = MyType { field1 :: Float, field2 :: V3 Float }
-- >   deriving (Generic)
-- >
-- > instance AlignedStorable Std140 MyType
class GAlignedStorable (layout :: MemoryLayout) (rep :: Type -> Type) where
  galignedSize :: Proxy layout -> Proxy rep -> Int -> (Int, Int)
  galignedPoke :: AlignedPtr layout a -> rep a -> Int -> IO Int
  galignedPeek :: AlignedPtr layout a -> Int -> IO (rep a, Int)

instance
  ( TypeError
      (Text "Cannot derive AlignedStorable for empty data types as there is no shader equivalent.")
  )
  => GAlignedStorable layout V1
  where
  galignedSize = error "unreachable: empty data type"
  galignedPoke = error "unreachable: empty data type"
  galignedPeek = error "unreachable: empty data type"

instance
  ( TypeError
      (Text "Cannot derive AlignedStorable for nullary constructors as there is no shader equivalent.")
  )
  => GAlignedStorable layout U1
  where
  galignedSize = error "unreachable: nullary constructor"
  galignedPoke = error "unreachable: nullary constructor"
  galignedPeek = error "unreachable: nullary constructor"

instance
  ( TypeError
      ( Text
          "Cannot derive AlignedStorable for sum types as there is no unambiguous shader equivalent."
      )
  )
  => GAlignedStorable layout (a :+: b)
  where
  galignedSize = error "unreachable: sum type"
  galignedPoke = error "unreachable: sum type"
  galignedPeek = error "unreachable: sum type"

instance (AlignedStorable layout c) => GAlignedStorable layout (K1 i c) where
  galignedSize l _ off =
    let !a = alignedAlignment l (Proxy @c)
        !s = alignedSizeOf l (Proxy @c)
        !off' = roundUpTo a off
     in (off' + s, a)
  {-# INLINE galignedSize #-}
  galignedPoke (AlignedPtr ptr) (K1 !v) !off = do
    let !a = alignedAlignment (Proxy @layout) (Proxy @c)
        !s = alignedSizeOf (Proxy @layout) (Proxy @c)
        !off' = roundUpTo a off
    alignedPoke @layout (AlignedPtr (ptr `plusPtr` off')) v
    pure (off' + s)
  {-# INLINE galignedPoke #-}
  galignedPeek (AlignedPtr ptr) !off = do
    let !a = alignedAlignment (Proxy @layout) (Proxy @c)
        !s = alignedSizeOf (Proxy @layout) (Proxy @c)
        !off' = roundUpTo a off
    !v <- alignedPeek @layout (AlignedPtr (ptr `plusPtr` off'))
    pure (K1 v, off' + s)
  {-# INLINE galignedPeek #-}

instance (GAlignedStorable layout a, GAlignedStorable layout b) => GAlignedStorable layout (a :*: b) where
  galignedSize l _ off =
    let (!nextOffA, !alignA) = galignedSize l (Proxy @a) off
        (!nextOffB, !alignB) = galignedSize l (Proxy @b) nextOffA
     in (nextOffB, max alignA alignB)
  {-# INLINE galignedSize #-}
  galignedPoke ptr (valA :*: valB) !off = do
    !nextOffA <- galignedPoke ptr valA off
    galignedPoke ptr valB nextOffA
  {-# INLINE galignedPoke #-}
  galignedPeek ptr off = do
    (!valA, !nextOffA) <- galignedPeek ptr off
    (!valB, !nextOffB) <- galignedPeek ptr nextOffA
    pure (valA :*: valB, nextOffB)
  {-# INLINE galignedPeek #-}

instance (GAlignedStorable layout f) => GAlignedStorable layout (M1 i c f) where
  galignedSize l _ = galignedSize l (Proxy @f)
  {-# INLINE galignedSize #-}
  galignedPoke ptr = galignedPoke ptr . unM1
  {-# INLINE galignedPoke #-}
  galignedPeek ptr off = first M1 <$> galignedPeek ptr off
  {-# INLINE galignedPeek #-}

--------------------------------------------------------------------------------
-- Sized arrays
--------------------------------------------------------------------------------

instance
  {-# OVERLAPPABLE #-}
  (KnownNat n, AlignedStorable layout a, GV.Vector v a)
  => AlignedStorable layout (SGV.Vector v n a)
  where
  packedAlignedSizeOf _ _ = packedAlignedSizeOfArray (Proxy @layout) (Proxy @n) (Proxy @a)
  {-# INLINE packedAlignedSizeOf #-}
  alignedSizeOf = packedAlignedSizeOf
  {-# INLINE alignedSizeOf #-}
  alignedAlignment l _ = arrayAlignedAlignment l (Proxy @a)
  {-# INLINE alignedAlignment #-}
  alignedPeek = garrayAlignedPeek
  {-# INLINE alignedPeek #-}
  alignedPoke = garrayAlignedPoke
  {-# INLINE alignedPoke #-}

instance (KnownNat n, AlignedStorable Scalar a, GV.Vector v a) => AlignedStorable Scalar (SGV.Vector v n a) where
  packedAlignedSizeOf _ _ = scalarPackedAlignedSizeOfArray (Proxy @n) (Proxy @a)
  {-# INLINE packedAlignedSizeOf #-}
  alignedSizeOf = packedAlignedSizeOf
  {-# INLINE alignedSizeOf #-}
  alignedAlignment l _ = arrayAlignedAlignment l (Proxy @a)
  {-# INLINE alignedAlignment #-}
  alignedPeek = garrayAlignedPeek
  {-# INLINE alignedPeek #-}
  alignedPoke = garrayAlignedPoke
  {-# INLINE alignedPoke #-}

instance
  {-# OVERLAPPABLE #-}
  (KnownNat n, AlignedStorable layout a)
  => AlignedStorable layout (AlignedArray layout n a)
  where
  packedAlignedSizeOf _ _ = packedAlignedSizeOfArray (Proxy @layout) (Proxy @n) (Proxy @a)
  {-# INLINE packedAlignedSizeOf #-}
  alignedSizeOf = packedAlignedSizeOf
  {-# INLINE alignedSizeOf #-}
  alignedAlignment l _ = arrayAlignedAlignment l (Proxy @a)
  {-# INLINE alignedAlignment #-}
  alignedPeek (AlignedPtr src) = do
    let s = fromIntegral $ natVal (Proxy @n)
    v <- SMV.new s
    SMV.unsafeWith v \dest ->
      copyBytes (castPtr dest) src (alignedSizeOf (Proxy @layout) (Proxy @(AlignedArray layout n a)))
    AlignedArray . fromJust . SSV.toSized @n <$> SV.unsafeFreeze v
  {-# INLINE alignedPeek #-}
  alignedPoke (AlignedPtr dest) (AlignedArray v) = SV.unsafeWith (SSV.fromSized v) \src ->
    copyBytes dest (castPtr src) (alignedSizeOf (Proxy @layout) (Proxy @(AlignedArray layout n a)))
  {-# INLINE alignedPoke #-}

instance
  (KnownNat n, AlignedStorable Scalar a)
  => AlignedStorable Scalar (AlignedArray Scalar n a)
  where
  packedAlignedSizeOf _ _ = scalarPackedAlignedSizeOfArray (Proxy @n) (Proxy @a)
  {-# INLINE packedAlignedSizeOf #-}
  alignedSizeOf = packedAlignedSizeOf
  {-# INLINE alignedSizeOf #-}
  alignedAlignment l _ = arrayAlignedAlignment l (Proxy @a)
  {-# INLINE alignedAlignment #-}
  alignedPeek (AlignedPtr src) = do
    let s = fromIntegral $ natVal (Proxy @n)
    v <- SMV.new s
    SMV.unsafeWith v \dest ->
      copyBytes (castPtr dest) src (alignedSizeOf (Proxy @Scalar) (Proxy @(AlignedArray Scalar n a)))
    AlignedArray . fromJust . SSV.toSized @n <$> SV.unsafeFreeze v
  {-# INLINE alignedPeek #-}
  alignedPoke (AlignedPtr dest) (AlignedArray v) = SV.unsafeWith (SSV.fromSized v) \src ->
    copyBytes dest (castPtr src) (alignedSizeOf (Proxy @Scalar) (Proxy @(AlignedArray Scalar n a)))
  {-# INLINE alignedPoke #-}

packedAlignedSizeOfArray
  :: forall layout n a
   . (KnownNat n, AlignedStorable layout a)
  => Proxy layout
  -> Proxy n
  -> Proxy a
  -> Int
packedAlignedSizeOfArray _ _ _ = fromIntegral (natVal (Proxy @n)) * sizeOf @(Strided layout a) undefined
{-# INLINE packedAlignedSizeOfArray #-}

scalarPackedAlignedSizeOfArray
  :: forall n a
   . (KnownNat n, AlignedStorable Scalar a)
  => Proxy n
  -> Proxy a
  -> Int
scalarPackedAlignedSizeOfArray _ _ =
  let n = fromIntegral $ natVal (Proxy @n)
      stride = sizeOf @(Strided Scalar a) undefined
      lastElementSize = packedAlignedSizeOf (Proxy @Scalar) (Proxy @a)
   in if n > 0
        then ((n - 1) * stride) + lastElementSize
        else 0
{-# INLINE scalarPackedAlignedSizeOfArray #-}

arrayAlignedAlignment
  :: forall layout a. (AlignedStorable layout a) => Proxy layout -> Proxy a -> Int
arrayAlignedAlignment l a = alignBlock l $ alignedAlignment l a
{-# INLINE arrayAlignedAlignment #-}

garrayAlignedPeek
  :: forall layout n a v
   . (KnownNat n, GV.Vector v a, AlignedStorable layout a)
  => AlignedPtr layout (SGV.Vector v n a)
  -> IO (SGV.Vector v n a)
garrayAlignedPeek (AlignedPtr ptr) =
  let !stride = sizeOf @(Strided layout a) undefined
   in SGV.generateM \i ->
        alignedPeek @layout (AlignedPtr (ptr `plusPtr` (fromIntegral i * stride)))
{-# INLINE garrayAlignedPeek #-}

garrayAlignedPoke
  :: forall layout n a v
   . (KnownNat n, GV.Vector v a, AlignedStorable layout a)
  => AlignedPtr layout (SGV.Vector v n a)
  -> SGV.Vector v n a
  -> IO ()
garrayAlignedPoke (AlignedPtr ptr) =
  let !stride = sizeOf @(Strided layout a) undefined
   in fusedimapMSized_ \i -> alignedPoke @layout (AlignedPtr (ptr `plusPtr` (i * stride)))
{-# INLINE garrayAlignedPoke #-}

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

-- | Bit-twiddling hack. Assumes multiple is a power of 2.
-- The assumption holds for all standard GPU layouts.
-- GHC's optimizer seems to choke if this function becomes too complex (e.g.,
-- branching).
roundUpTo :: (Bits a, Num a) => a -> a -> a
roundUpTo multiple val = (val + multiple - 1) .&. complement (multiple - 1)
{-# INLINE roundUpTo #-}

-- | Exploits the knowledge of the array length to create a straight, bounded loop over the
-- underlying vector.
fusedimapMSized_
  :: forall n v a f
   . (Monad f, KnownNat n, GV.Vector v a)
  => (Int -> a -> f ())
  -> SGV.Vector v n a
  -> f ()
fusedimapMSized_ f v =
  traverse_
    (\i -> f i (SGV.unsafeIndex v i))
    [0 .. fromIntegral (natVal (Proxy @n)) - 1]
{-# INLINE fusedimapMSized_ #-}

--------------------------------------------------------------------------------
-- Internal helpers used across macros
--------------------------------------------------------------------------------

defaultAlignedPeek :: (Storable a) => AlignedPtr layout a -> IO a
defaultAlignedPeek (AlignedPtr ptr) = peek ptr
{-# INLINE defaultAlignedPeek #-}

defaultAlignedPoke :: (Storable a) => AlignedPtr layout a -> a -> IO ()
defaultAlignedPoke (AlignedPtr ptr) = poke ptr
{-# INLINE defaultAlignedPoke #-}

packedAlignedSizeOfMat
  :: forall layout a f g
   . (AlignedStorable layout (g a))
  => Proxy layout
  -> Proxy (f (g a))
  -> Int
packedAlignedSizeOfMat l _ =
  let rowSize = packedAlignedSizeOf l (Proxy @(g a))
      rowAlign = alignedAlignment l (Proxy @(g a))
   in layoutStride (Proxy @layout) rowSize rowAlign
{-# INLINE packedAlignedSizeOfMat #-}

-- | Unsafe, for internal use only
castAlignedPtr :: forall layout a b. AlignedPtr layout a -> AlignedPtr layout b
castAlignedPtr (AlignedPtr ptr) = AlignedPtr (castPtr ptr)

-- | Unsafe, for internal use only
alignedPeekByteOff
  :: forall layout a. (AlignedStorable layout a) => AlignedPtr layout a -> Int -> IO a
alignedPeekByteOff (AlignedPtr ptr) off = alignedPeek (AlignedPtr @layout (ptr `plusPtr` off))
{-# INLINE alignedPeekByteOff #-}

-- | Unsafe, for internal use only
alignedPokeByteOff
  :: forall layout a. (AlignedStorable layout a) => AlignedPtr layout a -> Int -> a -> IO ()
alignedPokeByteOff (AlignedPtr ptr) off = alignedPoke (AlignedPtr @layout (ptr `plusPtr` off))
{-# INLINE alignedPokeByteOff #-}

alignedPeekV2
  :: forall layout a
   . (AlignedStorable layout a, Storable a)
  => AlignedPtr layout (V2 a)
  -> IO (V2 a)
alignedPeekV2 (AlignedPtr ptr) =
  let !stride = alignedAlignment (Proxy @layout) (Proxy @a)
   in V2 <$> peekByteOff ptr 0 <*> peekByteOff ptr stride
{-# INLINE alignedPeekV2 #-}

alignedPokeV2
  :: forall layout a
   . (AlignedStorable layout a, Storable a)
  => AlignedPtr layout (V2 a)
  -> V2 a
  -> IO ()
alignedPokeV2 (AlignedPtr ptr) (V2 x y) = do
  let !stride = alignedAlignment (Proxy @layout) (Proxy @a)
  pokeByteOff ptr 0 x
  pokeByteOff ptr stride y
{-# INLINE alignedPokeV2 #-}

alignedPeekM2
  :: forall layout a v
   . (AlignedStorable layout (v a))
  => AlignedPtr layout (V2 (v a))
  -> IO (V2 (v a))
alignedPeekM2 aptr =
  let ptr = castAlignedPtr aptr
      !rowSize = packedAlignedSizeOf (Proxy @layout) (Proxy @(v a))
      !rowAlign = alignedAlignment (Proxy @layout) (Proxy @(v a))
      !stride = layoutStride (Proxy @layout) rowSize rowAlign
   in V2
        <$> alignedPeekByteOff ptr 0
        <*> alignedPeekByteOff ptr stride
{-# INLINE alignedPeekM2 #-}

alignedPokeM2
  :: forall layout a v
   . (AlignedStorable layout (v a))
  => AlignedPtr layout (V2 (v a))
  -> V2 (v a)
  -> IO ()
alignedPokeM2 aptr (V2 x y) = do
  let ptr = castAlignedPtr aptr
      !rowSize = packedAlignedSizeOf (Proxy @layout) (Proxy @(v a))
      !rowAlign = alignedAlignment (Proxy @layout) (Proxy @(v a))
      !stride = layoutStride (Proxy @layout) rowSize rowAlign
  alignedPokeByteOff ptr 0 x
  alignedPokeByteOff ptr stride y
{-# INLINE alignedPokeM2 #-}

alignedPeekV3
  :: forall layout a. (AlignedStorable layout a, Storable a) => AlignedPtr layout (V3 a) -> IO (V3 a)
alignedPeekV3 (AlignedPtr ptr) =
  let a = alignedAlignment (Proxy @layout) (Proxy @a)
   in V3 <$> peekByteOff ptr 0 <*> peekByteOff ptr a <*> peekByteOff ptr (a * 2)
{-# INLINE alignedPeekV3 #-}

alignedPokeV3
  :: forall layout a. (AlignedStorable layout a, Storable a) => AlignedPtr layout (V3 a) -> V3 a -> IO ()
alignedPokeV3 (AlignedPtr ptr) (V3 x y z) = do
  let a = alignedAlignment (Proxy @layout) (Proxy @a)
  pokeByteOff ptr 0 x
  pokeByteOff ptr a y
  pokeByteOff ptr (a * 2) z
{-# INLINE alignedPokeV3 #-}

alignedPeekM3
  :: forall layout a v
   . (AlignedStorable layout (v a))
  => AlignedPtr layout (V3 (v a))
  -> IO (V3 (v a))
alignedPeekM3 aptr =
  let ptr = castAlignedPtr aptr
      !rowSize = packedAlignedSizeOf (Proxy @layout) (Proxy @(v a))
      !rowAlign = alignedAlignment (Proxy @layout) (Proxy @(v a))
      !stride = layoutStride (Proxy @layout) rowSize rowAlign
   in V3
        <$> alignedPeekByteOff ptr 0
        <*> alignedPeekByteOff ptr stride
        <*> alignedPeekByteOff ptr (stride * 2)
{-# INLINE alignedPeekM3 #-}

alignedPokeM3
  :: forall layout a v
   . (AlignedStorable layout (v a))
  => AlignedPtr layout (V3 (v a))
  -> V3 (v a)
  -> IO ()
alignedPokeM3 aptr (V3 x y z) = do
  let ptr = castAlignedPtr aptr
      !rowSize = packedAlignedSizeOf (Proxy @layout) (Proxy @(v a))
      !rowAlign = alignedAlignment (Proxy @layout) (Proxy @(v a))
      !stride = layoutStride (Proxy @layout) rowSize rowAlign
  alignedPokeByteOff ptr 0 x
  alignedPokeByteOff ptr stride y
  alignedPokeByteOff ptr (stride * 2) z
{-# INLINE alignedPokeM3 #-}

alignedPokeV4
  :: forall layout a. (AlignedStorable layout a, Storable a) => AlignedPtr layout (V4 a) -> V4 a -> IO ()
alignedPokeV4 (AlignedPtr ptr) (V4 x y z w) = do
  let a = alignedAlignment (Proxy @layout) (Proxy @a)
  pokeByteOff ptr 0 x
  pokeByteOff ptr a y
  pokeByteOff ptr (a * 2) z
  pokeByteOff ptr (a * 3) w
{-# INLINE alignedPokeV4 #-}

alignedPeekV4
  :: forall layout a. (AlignedStorable layout a, Storable a) => AlignedPtr layout (V4 a) -> IO (V4 a)
alignedPeekV4 (AlignedPtr ptr) =
  let a = alignedAlignment (Proxy @layout) (Proxy @a)
   in V4
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr a
        <*> peekByteOff ptr (a * 2)
        <*> peekByteOff ptr (a * 3)
{-# INLINE alignedPeekV4 #-}

alignedPeekM4
  :: forall layout a v
   . (AlignedStorable layout (v a))
  => AlignedPtr layout (V4 (v a))
  -> IO (V4 (v a))
alignedPeekM4 aptr =
  let ptr = castAlignedPtr aptr
      !rowSize = packedAlignedSizeOf (Proxy @layout) (Proxy @(v a))
      !rowAlign = alignedAlignment (Proxy @layout) (Proxy @(v a))
      !stride = layoutStride (Proxy @layout) rowSize rowAlign
   in V4
        <$> alignedPeekByteOff ptr 0
        <*> alignedPeekByteOff ptr stride
        <*> alignedPeekByteOff ptr (stride * 2)
        <*> alignedPeekByteOff ptr (stride * 3)
{-# INLINE alignedPeekM4 #-}

alignedPokeM4
  :: forall layout a v
   . (AlignedStorable layout (v a))
  => AlignedPtr layout (V4 (v a))
  -> V4 (v a)
  -> IO ()
alignedPokeM4 aptr (V4 x y z w) = do
  let ptr = castAlignedPtr aptr
      !rowSize = packedAlignedSizeOf (Proxy @layout) (Proxy @(v a))
      !rowAlign = alignedAlignment (Proxy @layout) (Proxy @(v a))
      !stride = layoutStride (Proxy @layout) rowSize rowAlign
  alignedPokeByteOff ptr 0 x
  alignedPokeByteOff ptr stride y
  alignedPokeByteOff ptr (stride * 2) z
  alignedPokeByteOff ptr (stride * 3) w
{-# INLINE alignedPokeM4 #-}

--------------------------------------------------------------------------------
-- Base instances for primitive shader types
--
-- I decided to use CPP macros because they are idiomatic, homoiconic, purely
-- functional, and safe. :)
--
-- Joking aside, CPP seems like the best solution for this portion of the code.
-- For the library to hold its contract:
--
-- 1. We need coherent instances.
-- 2. We can't overlap on `Storable`, as `AlignedStorable` is orthogonal, and
--    has a fundamentally different contract.
-- 3. Template Haskell would require 3 modules and orphan instances for a job
--    that's basically just copy+paste, which is a nonstarter for this grug.
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
