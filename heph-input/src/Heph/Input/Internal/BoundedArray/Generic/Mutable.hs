module Heph.Input.Internal.BoundedArray.Generic.Mutable (
  MBoundedArray (..),
  length,
) where

import Control.Monad.Primitive
import Prelude hiding (length)

import Heph.Input.Internal.BoundedArray.Index

class (Enum i, Bounded i) => MBoundedArray a i e where
  new :: (PrimMonad m) => e -> m (a (PrimState m) i e)
  read :: (PrimMonad m) => a (PrimState m) i e -> i -> m e
  write :: (PrimMonad m) => a (PrimState m) i e -> i -> e -> m ()
  copy :: (PrimMonad m) => a (PrimState m) i e -> a (PrimState m) i e -> m ()
  set :: (PrimMonad m) => a (PrimState m) i e -> e -> m ()
  modify :: (PrimMonad m) => a (PrimState m) i e -> i -> (e -> e) -> m ()

length :: forall a s i e. (MBoundedArray a i e) => a s i e -> Int
length _ = size @i
{-# INLINE length #-}
