-- |
-- Description : Fast, mutable sparse sets.
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
-- Portability : GHC
--
-- __This implementation is NOT thread-safe.__ Thread safety must be maintained by a whole-set
-- locking mechanism.
module Data.SparseSet.Storable.Mutable (
  MutableSparseSet,
  IOMutableSparseSet,
  STMutableSparseSet,

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

import Control.Monad.Primitive
import Data.Typeable (Typeable)
import Data.Vector.Storable qualified as VS
import GHC.Generics (Generic)
import Prelude hiding (length, lookup, mapM_)

import Data.SparseSet.Generic.Mutable qualified as G

newtype MutableSparseSet s a = MSS (G.MutableSparseSet VS.MVector s a)
  deriving stock (Generic, Typeable)

type IOMutableSparseSet = MutableSparseSet RealWorld
type STMutableSparseSet s = MutableSparseSet s

-- | Create a sparse set with a given dense and sparse capacity
--
-- It's a good idea to use this function if you have an estimate of your data requirements,
-- as it can prevent costly re-allocations as the set grows.
--
-- @since 0.1.0.0
withCapacity
  :: (PrimMonad m, VS.Storable a)
  => Int
  -- ^ Capacity for the dense set
  -> Int
  -- ^ Capacity for the sparse set
  -> m (MutableSparseSet (PrimState m) a)
withCapacity dc sc = MSS <$> G.withCapacity dc sc

-- | Create an empty sparse set with default capacities
--
-- @since 0.1.0.0
new :: forall a m. (PrimMonad m, VS.Storable a) => m (MutableSparseSet (PrimState m) a)
new = MSS <$> G.new
{-# INLINE new #-}

-- | O(1) Number of elements in the set (dense)
--
-- @since 0.1.0.0
length :: forall a m. (PrimMonad m) => MutableSparseSet (PrimState m) a -> m Int
length (MSS g) = G.length g
{-# INLINE length #-}

-- | O(1) Check whether an element is in the set
--
-- @since 0.1.0.0
contains :: (PrimMonad m) => MutableSparseSet (PrimState m) a -> Int -> m Bool
contains (MSS g) = G.contains g
{-# INLINE contains #-}

-- | O(n) The members of the set in an unspecified order.
--
-- @since 0.1.0.0
members :: (PrimMonad m) => MutableSparseSet (PrimState m) a -> m (VS.Vector Int)
members (MSS g) = G.members g
{-# INLINE members #-}

-- | O(1) Look up an element in the set
--
-- @since 0.1.0.0
lookup :: (PrimMonad m, VS.Storable a) => MutableSparseSet (PrimState m) a -> Int -> m (Maybe a)
lookup (MSS g) = G.lookup g
{-# INLINE lookup #-}

-- | O(1) amortized. Insert a value for a given key.
--
-- If the key is already in the set, its value is overwritten.
--
-- __INVARIANT__: Keys cannot be negative. An unchecked exception is
-- thrown if a negative key is added to the set.
--
-- @since 0.1.0.0
insert :: (PrimMonad m, VS.Storable a) => MutableSparseSet (PrimState m) a -> Int -> a -> m ()
insert (MSS g) = G.insert g
{-# INLINE insert #-}

-- | O(1) Delete an element from the set
--
-- @since 0.1.0.0
delete :: (PrimMonad m, VS.Storable a) => MutableSparseSet (PrimState m) a -> Int -> m (Maybe a)
delete (MSS g) = G.delete g
{-# INLINE delete #-}

-- | O(1) Clear all elements from the set
--
-- @since 0.1.0.0
clear :: (PrimMonad m) => MutableSparseSet (PrimState m) a -> m ()
clear (MSS g) = G.clear g
{-# INLINE clear #-}

-- | O(n) Shrink the capacity of the set to fit exactly the current number of elements.
--
-- @since 0.1.0.0
compact :: (PrimMonad m, VS.Storable a) => MutableSparseSet (PrimState m) a -> m ()
compact (MSS g) = G.compact g
{-# INLINE compact #-}

-- | O(n) Fold over the values of the set.
--
-- @since 0.1.0.0
foldM
  :: (PrimMonad m, VS.Storable a)
  => (b -> a -> m b)
  -> b
  -> MutableSparseSet (PrimState m) a
  -> m b
foldM f initAcc (MSS g) = G.foldM f initAcc g
{-# INLINE foldM #-}

-- | O(n) Fold over the keys and values of the set.
--
-- @since 0.1.0.0
ifoldM
  :: (PrimMonad m, VS.Storable a)
  => (b -> (Int, a) -> m b)
  -> b
  -> MutableSparseSet (PrimState m) a
  -> m b
ifoldM f initAcc (MSS g) = G.ifoldM f initAcc g
{-# INLINE ifoldM #-}

-- | O(n) Iterate over the values of the set.
--
-- @since 0.1.0.0
mapM_
  :: (PrimMonad m, VS.Storable a)
  => (a -> m ()) -- Action to perform
  -> MutableSparseSet (PrimState m) a
  -> m ()
mapM_ f (MSS g) = G.mapM_ f g
{-# INLINE mapM_ #-}

-- | O(n) Iterate over the keys and values of the set.
--
-- @since 0.1.0.0
imapM_
  :: (PrimMonad m, VS.Storable a)
  => ((Int, a) -> m ()) -- Action to perform
  -> MutableSparseSet (PrimState m) a
  -> m ()
imapM_ f (MSS g) = G.imapM_ f g
{-# INLINE imapM_ #-}

-- | O(min(n, m)) Iterate over the intersection of two sets with an accumulator.
--
-- The order of the arguments does not matter - the smaller of the two sets is
-- selected as the iteratee.
--
-- @since 0.1.0.0
ifoldIntersectionM
  :: (PrimMonad m, VS.Storable a, VS.Storable b)
  => (c -> Int -> a -> b -> m c)
  -> c
  -> MutableSparseSet (PrimState m) a
  -> MutableSparseSet (PrimState m) b
  -> m c
ifoldIntersectionM acc c (MSS a) (MSS b) = G.ifoldIntersectionM acc c a b
{-# INLINE ifoldIntersectionM #-}
