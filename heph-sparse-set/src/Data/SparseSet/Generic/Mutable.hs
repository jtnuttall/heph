-- |
-- Description : Fast, mutable sparse sets.
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
-- Portability : GHC
--
-- Generic mutable sparse sets, usable in any state transformer monad.
--
-- __This implementation is NOT thread-safe.__ Thread safety must be maintained by a whole-set
-- locking mechanism.
module Data.SparseSet.Generic.Mutable (
  MutableSparseSet,

  -- * Creation
  withCapacity,
  new,

  -- * Read
  length,
  contains,
  members,
  lookup,

  -- * Update
  insert,
  delete,
  clear,
  compact,

  -- * Iteration
  foldM,
  ifoldM,
  mapM_,
  imapM_,
  ifoldIntersectionM,
)
where

import Control.Monad hiding (foldM, foldM_, mapM_)
import Control.Monad.Primitive
import Data.Primitive
import Data.Typeable (Typeable)
import Data.Vector.Generic qualified as VG
import Data.Vector.Generic.Mutable qualified as MVG
import Data.Vector.Primitive.Mutable qualified as MVP
import GHC.Generics (Generic)
import Prelude hiding (length, lookup, mapM_)

import Data.SparseSet.Generic.Mutable.Internal.GrowVec (GrowVec)
import Data.SparseSet.Generic.Mutable.Internal.GrowVec qualified as GrowVec
import Data.SparseSet.Generic.Mutable.Internal.MutableSparseArray (MutableSparseArray)
import Data.SparseSet.Generic.Mutable.Internal.MutableSparseArray qualified as MSA

data MutableSparseSet v s a = MutableSparseSet
  { ssDense :: !(MutVar s (GrowVec v s a))
  , ssIndices :: !(MutVar s (GrowVec MVP.MVector s Int))
  , ssSparse :: !(MutVar s (MutableSparseArray s))
  }
  deriving (Generic, Typeable)

-- | Create a sparse set with a given dense and sparse capacity
--
-- It's a good idea to use this function if you have an estimate of your data requirements,
-- as it can prevent costly re-allocations as the set grows.
--
-- @since 0.1.0.0
withCapacity
  :: forall a v m
   . (PrimMonad m, MVG.MVector v a)
  => Int
  -- ^ Capacity for the dense set
  -> Int
  -- ^ Capacity for the sparse set
  -> m (MutableSparseSet v (PrimState m) a)
withCapacity dc sc =
  MutableSparseSet
    <$> (newMutVar =<< GrowVec.withCapacity dc)
    <*> (newMutVar =<< GrowVec.withCapacity dc)
    <*> (newMutVar =<< MSA.withCapacity sc)
{-# INLINE withCapacity #-}

-- | Create an empty sparse set with default capacities.
--
-- @since 0.1.0.0
new :: forall a v m. (PrimMonad m, MVG.MVector v a) => m (MutableSparseSet v (PrimState m) a)
new =
  MutableSparseSet
    <$> (newMutVar =<< GrowVec.new)
    <*> (newMutVar =<< GrowVec.new)
    <*> (newMutVar =<< MSA.new)
{-# INLINE new #-}

-- | O(1) Number of elements in the set (dense)
--
-- @since 0.1.0.0
length :: forall a v m. (PrimMonad m) => MutableSparseSet v (PrimState m) a -> m Int
length MutableSparseSet{..} = GrowVec.length <$> readMutVar ssDense
{-# INLINE length #-}

-- | O(1) Check whether an element is in the set
--
-- @since 0.1.0.0
contains :: forall a v m. (PrimMonad m) => MutableSparseSet v (PrimState m) a -> Int -> m Bool
contains MutableSparseSet{..} i = readMutVar ssSparse >>= (`MSA.contains` i)
{-# INLINE contains #-}

-- | O(n) The members of the set in an unspecified order.
--
-- @since 0.1.0.0
members
  :: forall w a v m. (VG.Vector v Int, PrimMonad m) => MutableSparseSet w (PrimState m) a -> m (v Int)
members MutableSparseSet{..} = fmap VG.convert . GrowVec.freeze =<< readMutVar ssIndices
{-# INLINE members #-}

-- | O(1) Look up an element in the set
--
-- @since 0.1.0.0
lookup
  :: forall a v m
   . (PrimMonad m, MVG.MVector v a)
  => MutableSparseSet v (PrimState m) a
  -> Int
  -> m (Maybe a)
lookup MutableSparseSet{..} i = do
  mSi <- (`MSA.lookup` i) =<< readMutVar ssSparse
  case mSi of
    Just si -> Just <$> (readMutVar ssDense >>= (`GrowVec.unsafeRead` si))
    Nothing -> pure Nothing
{-# INLINE lookup #-}

-- | O(1) amortized. Insert a value for a given key.
--
-- If the key is already in the set, its value is overwritten.
--
-- __INVARIANT__: Keys cannot be negative. An unchecked exception is
-- thrown if a negative key is added to the set.
--
-- @since 0.1.0.0
insert
  :: forall a v m
   . (PrimMonad m, MVG.MVector v a)
  => MutableSparseSet v (PrimState m) a
  -> Int
  -> a
  -> m ()
insert MutableSparseSet{..} i v
  | i < 0 = error "Key cannot be negative"
  | otherwise =
      readMutVar ssSparse >>= (`MSA.lookup` i) >>= \case
        Just di -> do
          dense <- readMutVar ssDense
          GrowVec.unsafeWrite dense di v
        Nothing -> do
          dense <- readMutVar ssDense
          writeMutVar ssDense =<< GrowVec.snoc dense v
          sparse <- readMutVar ssSparse >>= \arr -> MSA.unsafeInsert arr i (GrowVec.length dense)
          writeMutVar ssSparse sparse
          writeMutVar ssIndices =<< (`GrowVec.snoc` i) =<< readMutVar ssIndices
{-# INLINE insert #-}

-- | O(1) Delete an element from the set
--
-- @since 0.1.0.0
delete
  :: forall a v m
   . (PrimMonad m, MVG.MVector v a)
  => MutableSparseSet v (PrimState m) a
  -> Int
  -> m (Maybe a)
delete MutableSparseSet{..} i = do
  sparse <- readMutVar ssSparse
  MSA.delete sparse i >>= \case
    Just di -> do
      dense <- readMutVar ssDense
      indices <- readMutVar ssIndices
      (value, dense') <- GrowVec.unsafeSwapRemove dense di
      writeMutVar ssDense dense'
      (_, indices') <- GrowVec.unsafeSwapRemove indices di
      writeMutVar ssIndices indices'
      unless (di == GrowVec.length dense - 1) do
        swapped <- GrowVec.unsafeRead indices' di
        writeMutVar ssSparse =<< MSA.unsafeInsert sparse swapped di
      pure $ Just value
    Nothing -> pure Nothing
{-# INLINE delete #-}

-- | O(n) Clear all elements from the set
--
-- @since 0.1.0.0
clear :: forall a v m. (PrimMonad m) => MutableSparseSet v (PrimState m) a -> m ()
clear MutableSparseSet{..} = do
  indices <- readMutVar ssIndices
  sparse <- readMutVar ssSparse
  GrowVec.mapM_ (MSA.unsafeDelete sparse) indices

  atomicModifyMutVar' ssDense ((,()) . GrowVec.cleared)
  atomicModifyMutVar' ssIndices ((,()) . GrowVec.cleared)

-- | O(n) Shrink the capacity of the set to fit exactly the current number of elements.
--
-- @since 0.1.0.0
compact
  :: forall a v m. (PrimMonad m, MVG.MVector v a) => MutableSparseSet v (PrimState m) a -> m ()
compact MutableSparseSet{..} = do
  writeMutVar ssDense =<< GrowVec.compact =<< readMutVar ssDense
  indices <- readMutVar ssIndices
  indices' <- GrowVec.compact indices
  writeMutVar ssIndices indices'
  GrowVec.maximum indices >>= \case
    Nothing -> pure ()
    Just maxIndex -> do
      sparse <- readMutVar ssSparse
      writeMutVar ssSparse =<< MSA.unsafeCompactTo sparse (maxIndex + 1)

-- | O(n) Iterate over the values of the set with an accumulator.
--
-- @since 0.1.0.0
foldM
  :: (PrimMonad m, MVG.MVector v a)
  => (b -> a -> m b)
  -> b
  -> MutableSparseSet v (PrimState m) a
  -> m b
foldM f initAcc MutableSparseSet{..} = do
  denseGV <- readMutVar ssDense
  let !len = GrowVec.length denseGV
      go !idx !acc
        | idx >= len = pure acc
        | otherwise = do
            component <- GrowVec.unsafeRead denseGV idx
            newAcc <- f acc component
            go (idx + 1) newAcc

  go 0 initAcc
{-# INLINE foldM #-}

-- | O(n) Iterate over the keys and values of the set with an accumulator.
--
-- @since 0.1.0.0
ifoldM
  :: (PrimMonad m, MVG.MVector v a)
  => (b -> (Int, a) -> m b)
  -> b
  -> MutableSparseSet v (PrimState m) a
  -> m b
ifoldM f initAcc MutableSparseSet{..} = do
  denseGV <- readMutVar ssDense
  indicesGV <- readMutVar ssIndices
  let !len = GrowVec.length denseGV
      go !idx !acc
        | idx >= len = pure acc
        | otherwise = do
            entity <- GrowVec.unsafeRead indicesGV idx
            component <- GrowVec.unsafeRead denseGV idx
            newAcc <- f acc (entity, component)
            go (idx + 1) newAcc

  go 0 initAcc
{-# INLINE ifoldM #-}

-- | O(n) Iterate over the values of the set.
--
-- @since 0.1.0.0
mapM_
  :: (PrimMonad m, MVG.MVector v a)
  => (a -> m ())
  -> MutableSparseSet v (PrimState m) a
  -> m ()
mapM_ f MutableSparseSet{..} = do
  denseGV <- readMutVar ssDense
  let !len = GrowVec.length denseGV
      go !idx
        | idx >= len = pure ()
        | otherwise = do
            component <- GrowVec.unsafeRead denseGV idx
            f component
            go (idx + 1)
  go 0
{-# INLINE mapM_ #-}

-- | O(n) Iterate over the keys and values of the set.
--
-- @since 0.1.0.0
imapM_
  :: (PrimMonad m, MVG.MVector v a)
  => ((Int, a) -> m ())
  -> MutableSparseSet v (PrimState m) a
  -> m ()
imapM_ f MutableSparseSet{..} = do
  denseGV <- readMutVar ssDense
  indicesGV <- readMutVar ssIndices
  let !len = GrowVec.length denseGV
      go !idx
        | idx >= len = pure ()
        | otherwise = do
            entity <- GrowVec.unsafeRead indicesGV idx
            component <- GrowVec.unsafeRead denseGV idx
            f (entity, component)
            go (idx + 1)
  go 0
{-# INLINE imapM_ #-}

-- | O(min(n, m)) Iterate over the intersection of two sets with an accumulator.
--
-- The order of the arguments does not matter - the smaller of the two sets is
-- selected as the iteratee.
--
-- @since 0.1.0.0
ifoldIntersectionM
  :: (PrimMonad m, MVG.MVector v a, MVG.MVector v b)
  => (c -> Int -> a -> b -> m c)
  -- ^ Accumulator
  -> c
  -- ^ Initial value
  -> MutableSparseSet v (PrimState m) a
  -- ^ Set A
  -> MutableSparseSet v (PrimState m) b
  -- ^ Set B
  -> m c
ifoldIntersectionM f c a b = do
  la <- length a
  lb <- length b

  if la <= lb
    then ifoldM (goLookupB b) c a
    else ifoldM (goLookupA a) c b
 where
  goLookupB otherSetB acc (entity, componentA) =
    lookup otherSetB entity >>= \case
      Nothing -> pure acc
      Just componentB -> f acc entity componentA componentB

  goLookupA otherSetA acc (entity, componentB) =
    lookup otherSetA entity >>= \case
      Nothing -> pure acc
      Just componentA -> f acc entity componentA componentB
{-# INLINE ifoldIntersectionM #-}
