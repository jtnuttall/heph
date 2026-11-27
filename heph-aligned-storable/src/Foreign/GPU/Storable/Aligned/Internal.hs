{-# LANGUAGE DefaultSignatures #-}

-- |
-- Description : Internals of "Foreign.GPU.Storable.Aligned"
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : internal
--
-- Provides the core types and machinery for generically deriving GPU memory layouts.
--
-- This is an internal module with no stability guarantees.
module Foreign.GPU.Storable.Aligned.Internal (
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

  -- * Helpers used by generated instances
  defaultAlignedPeek,
  defaultAlignedPoke,
  castAlignedPtr,
  alignedPeekByteOff,
  alignedPokeByteOff,
  rowStride,
  peekStrided2,
  pokeStrided2,
  peekStrided3,
  pokeStrided3,
  peekStrided4,
  pokeStrided4,
  peekCoerce,
  pokeCoerce,
)
where

import Control.Monad
import Data.Bifunctor (first)
import Data.Bits
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Hashable (Hashable)
import Data.Kind
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Vector.Generic qualified as GV
import Data.Vector.Generic.Sized qualified as SGV
import Data.Vector.Storable qualified as SV
import Data.Vector.Storable.Mutable qualified as SMV
import Data.Vector.Storable.Sized qualified as SSV
import Foreign (Ptr, castPtr, copyBytes, plusPtr)
import Foreign.Storable
import GHC.Generics
import GHC.TypeLits

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
  deriving (Bounded, Enum, Eq, Generic, Ord, Show)

instance Hashable MemoryLayout

-- | Singled 'MemoryLayout', primarily useful for authors of higher-level libraries
-- (e.g., shader eDSLs, descriptor set builders).
data SMemoryLayout (layout :: MemoryLayout) where
  SStd140 :: SMemoryLayout Std140
  SStd430 :: SMemoryLayout Std430
  SScalar :: SMemoryLayout Scalar

-- | This class gives the 'SMemoryLayout' associated with a 'MemoryLayout'.
--
-- A very rough sketch of this use case:
--
-- > emitLayoutQualifier :: forall layout. KnownMemoryLayout layout => String
-- > emitLayoutQualifier = case memoryLayoutVal (Proxy @layout) of
-- >   Std140 -> "layout(std140)"
-- >   Std430 -> "layout(std430)"
-- >   Scalar -> "layout(scalar)"
class KnownMemoryLayout (layout :: MemoryLayout) where
  memoryLayoutSing :: SMemoryLayout layout

instance KnownMemoryLayout Std140 where
  memoryLayoutSing = SStd140
  {-# INLINE memoryLayoutSing #-}

instance KnownMemoryLayout Std430 where
  memoryLayoutSing = SStd430
  {-# INLINE memoryLayoutSing #-}

instance KnownMemoryLayout Scalar where
  memoryLayoutSing = SScalar
  {-# INLINE memoryLayoutSing #-}

memoryLayoutVal :: forall layout. (KnownMemoryLayout layout) => Proxy layout -> MemoryLayout
memoryLayoutVal _ = case memoryLayoutSing @layout of
  SStd140 -> Std140
  SStd430 -> Std430
  SScalar -> Scalar
{-# INLINE memoryLayoutVal #-}

-- | Defines the precise calculation rules for a given 'MemoryLayout'.
--
-- These rules correspond to the memory layout requirements found in the OpenGL
-- and Vulkan specifications.
class MemoryLayoutRules (layout :: MemoryLayout) where
  -- | The alignment rule for top-level structs, matrix rows, and array members.
  alignBlock :: Proxy layout -> Int -> Int

  -- | The final size rule for a top-level struct.
  roundStructSize :: Proxy layout -> Int -> Int -> Int

  -- | The stride rule for an element within an array.
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

-- | Wrapper whose 'sizeOf' includes stride padding, for use in arrays.
-- @SV.Vector (Strided Std140 MyType)@ can be 'copyBytes'd to the GPU in one shot.
newtype Strided (layout :: MemoryLayout) a = Strided {unStrided :: a}
  deriving (Eq, Generic, Ord, Show)

-- | Wrapper without stride padding. Use for single values (push constants, lone UBOs).
newtype Packed (layout :: MemoryLayout) a = Packed {unPacked :: a}
  deriving (Eq, Generic, Ord, Show)

-- | A convenience type for Data.Vector.Storable.Vector (Strided layout a)
type StridedVector (layout :: MemoryLayout) a = SV.Vector (Strided layout a)

-- | Opt-in 'copyBytes' for fixed-size arrays. Parameterize your struct by layout:
--
-- @
-- data MyStruct layout = MyStruct { pixels :: AlignedArray layout 64 (V4 Float) }
-- @
newtype AlignedArray (layout :: MemoryLayout) (n :: Nat) a = AlignedArray
  {unAlignedArray :: SSV.Vector n (Strided layout a)}
  deriving (Eq, Generic, Ord, Show)

-- | Construct an 'AlignedArray' from a sized vector.
mkAlignedArray
  :: (AlignedStorable layout a, Storable a) => SSV.Vector n a -> AlignedArray layout n a
mkAlignedArray = AlignedArray . SSV.map Strided
{-# INLINE CONLIKE mkAlignedArray #-}

-- | Helper function to unwrap an 'AlignedArray' and manipulate its contents.
-- Don't do unsafe pointer tricks in the closure.
withAlignedArray
  :: AlignedArray layout n a
  -> (SSV.Vector n (Strided layout a) -> SSV.Vector n (Strided layout a))
  -> AlignedArray layout n a
withAlignedArray (AlignedArray v) f = AlignedArray (f v)
{-# INLINE withAlignedArray #-}

-- | A 'Ptr' that is tagged with its 'MemoryLayout'.
newtype AlignedPtr (layout :: MemoryLayout) a = AlignedPtr {unAlignedPtr :: Ptr a}
  deriving (Generic, Show)

type role AlignedPtr nominal nominal

-- | A class for types that have calculable layouts according to GPU requirements.
--
-- __NB__: `alignedPoke` only writes member data. Only poke into zeroed buffers
-- unless you really enjoy parsing through garbage in RenderDoc.
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

-- | @sizeOf @(Strided layout a)@ calculates the full stride of the type,
-- including final padding, making it suitable for array allocations.
--
-- Use zeroed buffers when using this newtype.
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
-- Generic deriving
--------------------------------------------------------------------------------

-- | Generically derive 'AlignedStorable'.
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
   in if n > 0 then
        ((n - 1) * stride) + lastElementSize
      else
        0
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
--
-- __NB__: As of writing, GHC's optimizer fails when optimizing GAlignedStorable
-- if this function branches.
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
-- Internal helpers used by generated instances
--------------------------------------------------------------------------------

-- | 'alignedPeek' through the type's 'Storable' instance. Only correct when the
-- 'Storable' bytes are exactly the GPU bytes.
defaultAlignedPeek :: (Storable a) => AlignedPtr layout a -> IO a
defaultAlignedPeek (AlignedPtr ptr) = peek ptr
{-# INLINE defaultAlignedPeek #-}

-- | 'alignedPoke' through the type's 'Storable' instance. Only correct when the
-- 'Storable' bytes are exactly the GPU bytes.
defaultAlignedPoke :: (Storable a) => AlignedPtr layout a -> a -> IO ()
defaultAlignedPoke (AlignedPtr ptr) = poke ptr
{-# INLINE defaultAlignedPoke #-}

-- | Unsafe, for internal use only
castAlignedPtr :: forall layout a b. AlignedPtr layout a -> AlignedPtr layout b
castAlignedPtr (AlignedPtr ptr) = AlignedPtr (castPtr ptr)
{-# INLINE castAlignedPtr #-}

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

-- | The distance in bytes between consecutive rows of a matrix (or columns, for
-- a library that stores columns) whose row type is @row@: the row's packed size
-- rounded up by 'layoutStride'.
rowStride :: forall layout row. (AlignedStorable layout row) => Proxy layout -> Proxy row -> Int
rowStride l r = layoutStride l (packedAlignedSizeOf l r) (alignedAlignment l r)
{-# INLINE rowStride #-}

-- | Read a value made of two components of type @e@, the second @stride@ bytes
-- after the first, and assemble it with the given constructor.
peekStrided2
  :: forall layout e v
   . (AlignedStorable layout e)
  => (e -> e -> v)
  -> Int
  -> AlignedPtr layout v
  -> IO v
peekStrided2 mk !stride aptr =
  let p = castAlignedPtr @layout @v @e aptr
   in mk
        <$> alignedPeekByteOff p 0
        <*> alignedPeekByteOff p stride
{-# INLINE peekStrided2 #-}

-- | Write a value made of two components of type @e@, the second @stride@
-- bytes after the first. The eliminator hands the components to a continuation,
-- e.g. @\\(V2 x y) k -> k x y@.
pokeStrided2
  :: forall layout e v
   . (AlignedStorable layout e)
  => (v -> (e -> e -> IO ()) -> IO ())
  -> Int
  -> AlignedPtr layout v
  -> v
  -> IO ()
pokeStrided2 elim !stride aptr v =
  let p = castAlignedPtr @layout @v @e aptr
   in elim v \x y -> do
        alignedPokeByteOff p 0 x
        alignedPokeByteOff p stride y
{-# INLINE pokeStrided2 #-}

-- | Three-component version of 'peekStrided2'.
peekStrided3
  :: forall layout e v
   . (AlignedStorable layout e)
  => (e -> e -> e -> v)
  -> Int
  -> AlignedPtr layout v
  -> IO v
peekStrided3 mk !stride aptr =
  let p = castAlignedPtr @layout @v @e aptr
   in mk
        <$> alignedPeekByteOff p 0
        <*> alignedPeekByteOff p stride
        <*> alignedPeekByteOff p (stride * 2)
{-# INLINE peekStrided3 #-}

-- | Three-component version of 'pokeStrided2'.
pokeStrided3
  :: forall layout e v
   . (AlignedStorable layout e)
  => (v -> (e -> e -> e -> IO ()) -> IO ())
  -> Int
  -> AlignedPtr layout v
  -> v
  -> IO ()
pokeStrided3 elim !stride aptr v =
  let p = castAlignedPtr @layout @v @e aptr
   in elim v \x y z -> do
        alignedPokeByteOff p 0 x
        alignedPokeByteOff p stride y
        alignedPokeByteOff p (stride * 2) z
{-# INLINE pokeStrided3 #-}

-- | Four-component version of 'peekStrided2'.
peekStrided4
  :: forall layout e v
   . (AlignedStorable layout e)
  => (e -> e -> e -> e -> v)
  -> Int
  -> AlignedPtr layout v
  -> IO v
peekStrided4 mk !stride aptr =
  let p = castAlignedPtr @layout @v @e aptr
   in mk
        <$> alignedPeekByteOff p 0
        <*> alignedPeekByteOff p stride
        <*> alignedPeekByteOff p (stride * 2)
        <*> alignedPeekByteOff p (stride * 3)
{-# INLINE peekStrided4 #-}

-- | Four-component version of 'pokeStrided2'.
pokeStrided4
  :: forall layout e v
   . (AlignedStorable layout e)
  => (v -> (e -> e -> e -> e -> IO ()) -> IO ())
  -> Int
  -> AlignedPtr layout v
  -> v
  -> IO ()
pokeStrided4 elim !stride aptr v =
  let p = castAlignedPtr @layout @v @e aptr
   in elim v \x y z w -> do
        alignedPokeByteOff p 0 x
        alignedPokeByteOff p stride y
        alignedPokeByteOff p (stride * 2) z
        alignedPokeByteOff p (stride * 3) w
{-# INLINE pokeStrided4 #-}

-- | 'alignedPeek' for a newtype @b@ over @a@, using @a@'s instance.
peekCoerce
  :: forall layout a b
   . (AlignedStorable layout a, Coercible a b)
  => Proxy a
  -> AlignedPtr layout b
  -> IO b
peekCoerce _ p = coerce (alignedPeek (castAlignedPtr @layout @b @a p) :: IO a)
{-# INLINE peekCoerce #-}

-- | 'alignedPoke' for a newtype @b@ over @a@, using @a@'s instance.
pokeCoerce
  :: forall layout a b
   . (AlignedStorable layout a, Coercible a b)
  => Proxy a
  -> AlignedPtr layout b
  -> b
  -> IO ()
pokeCoerce _ p b = alignedPoke (castAlignedPtr @layout @b @a p) (coerce b :: a)
{-# INLINE pokeCoerce #-}
