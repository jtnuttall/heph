{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Description : Mutable sparse arrays, suitable for sparse set implementation.
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
module Data.SparseSet.Generic.Mutable.Internal.MutableSparseArray (
  MutableSparseArray,
  withCapacity,
  new,
  contains,
  lookup,
  unsafeInsert,
  delete,
  unsafeDelete,
  clear,
  unsafeCompactTo,
  freeze,
  unsafeFreeze,
)
where

import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Primitive
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import Data.Vector.Generic.Mutable qualified as VGM
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import GHC.Generics (Generic)
import Prelude hiding (lookup, maximum)

pattern ABSURD :: Int
pattern ABSURD = -1

-- | Mutable sparse integer array parameterized by its state token.
--
-- @since 0.1.0.0
newtype MutableSparseArray s = MutableSparseArray
  {getSparseArray :: VPM.MVector s Int}
  deriving newtype (NFData)
  deriving stock (Generic, Typeable)

-- | Create a new, empty array from a given capacity.
--
-- @since 0.1.0.0
withCapacity :: (PrimMonad m) => Int -> m (MutableSparseArray (PrimState m))
withCapacity rc = stToPrim do
  let c = max rc 4
  arr <- VPM.new c
  when (c > 0) $ fillArray 0 c arr
  pure $ MutableSparseArray arr
{-# INLINE withCapacity #-}

-- | Create a new, empty array.
--
-- @since 0.1.0.0
new :: (PrimMonad m) => m (MutableSparseArray (PrimState m))
new = withCapacity 32
{-# INLINE new #-}

contains :: (PrimMonad m) => MutableSparseArray (PrimState m) -> Int -> m Bool
contains arr i = isJust <$> lookup arr i
{-# INLINE contains #-}

lookup :: (PrimMonad m) => MutableSparseArray (PrimState m) -> Int -> m (Maybe Int)
#if MIN_VERSION_vector(0,13,0)
lookup (MutableSparseArray arr) i = (>>= msaReprToMaybe) <$> VPM.readMaybe arr i
#else
lookup (MutableSparseArray arr) i
  | i < 0 || i >= VPM.length arr = pure Nothing
  | otherwise = msaReprToMaybe <$> VPM.unsafeRead arr i
#endif
{-# INLINE lookup #-}

unsafeInsert
  :: (PrimMonad m)
  => MutableSparseArray (PrimState m)
  -> Int
  -> Int
  -> m (MutableSparseArray (PrimState m))
unsafeInsert (MutableSparseArray arr) i v
  | i < 0 = error $ "Negative index " <> show i
  | otherwise = do
      let len = VPM.length arr
          growBy = max (i + 1) ((len `quot` 2) * 3)
      mArr <-
        if i >= len
          then do
            r <- VPM.unsafeGrow arr growBy
            fillArray len growBy r
            pure r
          else pure arr

      VPM.unsafeWrite mArr i v
      pure $ MutableSparseArray mArr
{-# INLINE unsafeInsert #-}

delete :: (PrimMonad f) => MutableSparseArray (PrimState f) -> Int -> f (Maybe Int)
delete (MutableSparseArray arr) i
  | i < 0 || i >= VPM.length arr = pure Nothing
  | otherwise = msaReprToMaybe <$> VPM.unsafeExchange arr i ABSURD
{-# INLINE delete #-}

-- | Currently checks that the index is not negative, but this may change in the future
--
-- @since 0.1.0.0
unsafeDelete :: (PrimMonad m) => MutableSparseArray (PrimState m) -> Int -> m (Maybe Int)
unsafeDelete (MutableSparseArray arr) i
  | i < 0 = error $ "Negative index " <> show i
  | otherwise = msaReprToMaybe <$> VPM.unsafeExchange arr i ABSURD
{-# INLINE unsafeDelete #-}

clear :: (PrimMonad m) => MutableSparseArray (PrimState m) -> m ()
clear (MutableSparseArray arr) = VPM.set arr ABSURD
{-# INLINE clear #-}

unsafeCompactTo
  :: (PrimMonad f) => MutableSparseArray (PrimState f) -> Int -> f (MutableSparseArray (PrimState f))
unsafeCompactTo (MutableSparseArray arr) len
  | len < 0 = error "Cannot compact to negative capacity"
  | len >= VPM.length arr = pure $ MutableSparseArray arr
  | otherwise = MutableSparseArray <$> VPM.clone (VPM.slice 0 len arr)
{-# INLINE unsafeCompactTo #-}

freeze :: (PrimMonad m) => MutableSparseArray (PrimState m) -> m (VP.Vector Int)
freeze (MutableSparseArray arr) = VP.freeze arr
{-# INLINE freeze #-}

unsafeFreeze :: (PrimMonad m) => MutableSparseArray (PrimState m) -> m (VP.Vector Int)
unsafeFreeze (MutableSparseArray arr) = VP.unsafeFreeze arr
{-# INLINE unsafeFreeze #-}

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------
msaReprToMaybe :: Int -> Maybe Int
msaReprToMaybe v
  | v <= ABSURD = Nothing
  | otherwise = Just v
{-# INLINE msaReprToMaybe #-}

fillArray :: (PrimMonad m, VGM.MVector v Int) => Int -> Int -> v (PrimState m) Int -> m ()
fillArray len growBy arr = stToPrim $ VGM.basicSet (VGM.basicUnsafeSlice len growBy arr) ABSURD
{-# INLINE fillArray #-}
