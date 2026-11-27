-- |
-- Description : Zero-initialized allocation for GPU-aligned types
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Allocation helpers with guaranteed zero-initialized padding.
-- Use these instead of 'alloca' or 'malloc' to avoid garbage in padding bytes.
module Foreign.GPU.Marshal.Aligned (
  -- * Utilities for 'Packed' values
  PackedPtr,
  withPacked,
  allocaPacked,

  -- * Utilities for 'Strided' values
  StridedPtr,
  withStrided,
  allocaStrided,

  -- * Utilities for runtime length arrays
  alignedCopyVector,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.IO.Unlift (MonadUnliftIO (withRunInIO))
import Data.Vector.Storable qualified as SV
import Data.Word (Word8)
import Foreign qualified
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (poke, sizeOf))
import Prelude (Int, undefined, ($), (*), (.), (>>))

import Foreign.GPU.Storable.Aligned.Internal (
  AlignedPtr (..),
  AlignedStorable,
  Packed (..),
  Strided (..),
 )

-- | Convenience type for 'Packed' 'AlignedPtr'
type PackedPtr layout a = AlignedPtr layout (Packed layout a)

-- | Temporarily allocates a zero-initialized block of memory and pokes a
-- 'Packed' value into it, providing a pointer to the result.
-- The storage is freed automatically. The pointer is only valid within the continuation.
withPacked
  :: forall layout a m b
   . (MonadUnliftIO m, AlignedStorable layout a)
  => a
  -> (PackedPtr layout a -> m b)
  -> m b
withPacked a f = withZeroed (Packed a) (f . AlignedPtr)
{-# INLINEABLE withPacked #-}

-- | Allocates temporary, zero-initialized storage for a 'Packed' value on the stack.
-- The pointer is only valid within the continuation.
allocaPacked
  :: forall layout a b m
   . (MonadUnliftIO m, AlignedStorable layout a)
  => (PackedPtr layout a -> m b)
  -> m b
allocaPacked f = allocaZeroed (f . AlignedPtr)
{-# INLINEABLE allocaPacked #-}

-- | Convenience type for 'Strided' 'AlignedPtr'
type StridedPtr layout a = AlignedPtr layout (Strided layout a)

-- | Temporarily allocates a zero-initialized block of memory and pokes a
-- 'Strided' value into it, providing a pointer to the result.
-- The storage is freed automatically. The pointer is only valid within the continuation.
withStrided
  :: (MonadUnliftIO m, AlignedStorable layout a) => a -> (StridedPtr layout a -> m b) -> m b
withStrided a f = withZeroed (Strided a) (f . AlignedPtr)
{-# INLINEABLE withStrided #-}

-- | Allocates temporary, zero-initialized storage for a 'Strided' value on the stack.
-- The pointer is only valid within the continuation.
allocaStrided :: (MonadUnliftIO m, AlignedStorable layout a) => (StridedPtr layout a -> m b) -> m b
allocaStrided f = allocaZeroed (f . AlignedPtr)
{-# INLINEABLE allocaStrided #-}

-- | Performs a straight 'copyBytes' on the underlying pointer of 'SV.Vector'
alignedCopyVector
  :: forall layout a m
   . (MonadIO m, AlignedStorable layout a)
  => AlignedPtr layout a
  -> SV.Vector (Strided layout a)
  -> m ()
alignedCopyVector (AlignedPtr dest) v = liftIO $ SV.unsafeWith v \src ->
  copyBytes
    dest
    (castPtr src)
    (sizeOf @(Strided layout a) undefined * SV.length v)
{-# INLINE alignedCopyVector #-}

--------------------------------------------------------------------------------
-- Base implementations
--------------------------------------------------------------------------------

zeroPtr :: forall a m. (MonadIO m, Storable a) => Ptr a -> m ()
zeroPtr ptr = fillBytes ptr 0 (sizeOf @a undefined)
{-# INLINE zeroPtr #-}

allocaZeroed :: (MonadUnliftIO m, Storable a) => (Ptr a -> m b) -> m b
allocaZeroed f = alloca \ptr -> zeroPtr ptr >> f ptr
{-# INLINE allocaZeroed #-}

withZeroed :: (MonadUnliftIO m, Storable a) => a -> (Ptr a -> m b) -> m b
withZeroed a f = allocaZeroed \ptr -> do
  liftIO $ poke ptr a
  f ptr
{-# INLINE withZeroed #-}

--------------------------------------------------------------------------------
-- Foreign in MonadIO/MonadUnliftIO
--------------------------------------------------------------------------------

alloca :: (MonadUnliftIO m, Storable a) => (Ptr a -> m b) -> m b
alloca f = withRunInIO (\u -> Foreign.alloca (u . f))
{-# INLINE alloca #-}

copyBytes :: (MonadIO m) => Ptr a -> Ptr a -> Int -> m ()
copyBytes dest src sz = liftIO (Foreign.copyBytes dest src sz)
{-# INLINE copyBytes #-}

fillBytes :: (MonadIO m) => Ptr a -> Word8 -> Int -> m ()
fillBytes ptr value sz = liftIO (Foreign.fillBytes ptr value sz)
{-# INLINE fillBytes #-}
