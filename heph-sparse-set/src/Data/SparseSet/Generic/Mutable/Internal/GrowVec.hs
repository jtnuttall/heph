-- |
-- Description : A generic growable mutable vector with O(1) amortized append.
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
-- Portability : GHC
--
-- __WARNING:__ The functions in this module are generally unchecked and unsafe. Be careful to understand
-- and maintain invariants if using them. Misuse may result in undefined behavior.
--
-- Internal modules can change without warning between minor versions.
module Data.SparseSet.Generic.Mutable.Internal.GrowVec (
  GrowVec,
  withCapacity,
  new,
  length,
  capacity,
  snoc,
  readMaybe,
  unsafeRead,
  maximum,
  unsafeWrite,
  unsafeSwapRemove,
  cleared,
  compact,
  freeze,
  unsafeFreeze,
)
where

import Control.DeepSeq (NFData)
import Control.Monad.Primitive
import Data.Typeable (Typeable)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as VGM
import GHC.Generics (Generic)
import Prelude hiding (length, maximum)

data GrowVec v s a = GrowVec !Int !(v s a)
  deriving (Show, Generic, Typeable)

instance (NFData (v s a)) => NFData (GrowVec v s a)

-- | Create a new, empty vector with the given capacity
--
-- @since 0.1.0.0
withCapacity :: forall a v m. (PrimMonad m, VGM.MVector v a) => Int -> m (GrowVec v (PrimState m) a)
withCapacity c = GrowVec 0 <$> VGM.new (withMinCapacity c)
{-# INLINE withCapacity #-}

-- | Create a new, empty vector with a default capcity
--
-- @since 0.1.0.0
new :: forall a v m. (PrimMonad m, VGM.MVector v a) => m (GrowVec v (PrimState m) a)
new = GrowVec 0 <$> VGM.new 16
{-# INLINE new #-}

-- | O(1) The logical length of the vector.
length :: forall a v s. GrowVec v s a -> Int
length (GrowVec l _) = l
{-# INLINE length #-}

-- | O(1) The capacity of the vector.
capacity :: (VGM.MVector v a) => GrowVec v s a -> Int
capacity (GrowVec _ v) = VGM.length v
{-# INLINE capacity #-}

-- | Calculate the additional number of elements given the current length.
--
-- __INVARIANT__: length must be >= 2
--
-- @since 0.1.0.0
growthFactor :: Int -> Int
growthFactor l = (l `quot` 2) * 3
{-# INLINE growthFactor #-}

-- | O(1) amortized. Append to the vector, reallocating and copying if necessary.
--
-- Since this can't be done in-place, you must use the resulting vector in further computations.
--
-- @since 0.1.0.0
snoc
  :: (VGM.MVector v a, PrimMonad m) => GrowVec v (PrimState m) a -> a -> m (GrowVec v (PrimState m) a)
snoc gv a = do
  gv' <- grow gv
  unsafeWrite gv' (length gv) a
  pure gv'
 where
  grow (GrowVec l v)
    | capacity gv <= l = GrowVec (l + 1) <$> VGM.grow v (growthFactor l)
    | otherwise = pure $ GrowVec (l + 1) v
{-# INLINE snoc #-}

readMaybe
  :: forall a m v. (PrimMonad m, VGM.MVector v a) => GrowVec v (PrimState m) a -> Int -> m (Maybe a)
readMaybe (GrowVec l v) i
  | l < 0 || i >= l = pure Nothing
  | otherwise = Just <$> VGM.unsafeRead v i
{-# INLINE readMaybe #-}

-- | O(1) Read from a position in the vector. This position must be less than the length of the vector. This is not checked.
--
-- @since 0.1.0.0
unsafeRead
  :: forall a m v. (PrimMonad m, VGM.MVector v a) => GrowVec v (PrimState m) a -> Int -> m a
unsafeRead (GrowVec _ v) = VGM.unsafeRead v
{-# INLINE unsafeRead #-}

-- | O(n) The maximum value in the vector. Useful for compaction.
--
-- @since 0.1.0.0
maximum
  :: (PrimMonad m, VGM.MVector v a, Ord a, Bounded a) => GrowVec v (PrimState m) a -> m (Maybe a)
maximum (GrowVec l v)
  | l <= 0 = pure Nothing
  | otherwise = Just <$> VGM.foldl' max minBound (VGM.unsafeSlice 0 l v)
{-# INLINE maximum #-}

-- | O(1) Write to a position in the vector. This position must be less than the length of the vector. This is not checked.
--
-- @since 0.1.0.0
unsafeWrite
  :: forall a m v. (PrimMonad m, VGM.MVector v a) => GrowVec v (PrimState m) a -> Int -> a -> m ()
unsafeWrite (GrowVec _ v) = VGM.unsafeWrite v
{-# INLINE unsafeWrite #-}

-- | O(1) Swap-and-pop an element in the vector
--
-- @since 0.1.0.0
unsafeSwapRemove
  :: forall a m v
   . (PrimMonad m, VGM.MVector v a)
  => GrowVec v (PrimState m) a
  -> Int
  -> m (a, GrowVec v (PrimState m) a)
unsafeSwapRemove (GrowVec l v) i = do
  old <- VGM.read v i
  VGM.swap v i (l - 1)
  pure (old, GrowVec (l - 1) v)
{-# INLINE unsafeSwapRemove #-}

-- | O(1) Create a new, empty vector by setting logical length to 0. This does not change the
-- underlying vector in any way.
--
-- @since 0.1.0.0
cleared :: forall a s v. GrowVec v s a -> GrowVec v s a
cleared (GrowVec _ v) = GrowVec 0 v
{-# INLINE cleared #-}

-- | O(n) Shrink the vector so that its capacity matches its current length
--
-- @since 0.1.0.0
compact
  :: (VGM.MVector v a, PrimMonad m) => GrowVec v (PrimState m) a -> m (GrowVec v (PrimState m) a)
compact gv@(GrowVec l v)
  | capacity gv == l = pure gv
  | otherwise = do
      let l' = withMinCapacity l
      v' <- VGM.clone (VGM.unsafeSlice 0 l' v)
      pure (GrowVec l v')

freeze :: (PrimMonad m, VG.Vector v a) => GrowVec (VG.Mutable v) (PrimState m) a -> m (v a)
freeze (GrowVec l v) = VG.freeze (VGM.unsafeSlice 0 l v)
{-# INLINE freeze #-}

unsafeFreeze :: (PrimMonad m, VG.Vector v a) => GrowVec (VG.Mutable v) (PrimState m) a -> m (v a)
unsafeFreeze (GrowVec l v) = VG.unsafeFreeze (VGM.unsafeSlice 0 l v)
{-# INLINE unsafeFreeze #-}

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
withMinCapacity :: Int -> Int
withMinCapacity c = max c 4
{-# INLINE withMinCapacity #-}
