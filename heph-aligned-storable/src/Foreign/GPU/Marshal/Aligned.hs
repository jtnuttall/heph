{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Description : Utilities for marshaling values aligned to GPU requirements
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- This module provides convenience functions for allocating and manipulating
-- pointers to 'AlignedStorable' types, built on 'UnliftIO.Foreign'.
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

import qualified Data.Vector.Storable as SV
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)
import UnliftIO.Foreign

import Foreign.GPU.Storable.Aligned

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
-- Utilities
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
