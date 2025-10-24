{-# LANGUAGE RoleAnnotations #-}

module Heph.Input.Internal.BoundedArray.Boxed.Mutable (
  MBoundedArray (..),
  IOBoundedArray,
  STBoundedArray,
  new,
  read,
  write,
  copy,
) where

import Control.Monad.Primitive
import Data.Primitive.SmallArray
import Prelude hiding (read)

import Heph.Input.Internal.BoundedArray.Generic.Mutable qualified as G
import Heph.Input.Internal.BoundedArray.Index

newtype MBoundedArray s i e = MBA (SmallMutableArray s e)

type IOBoundedArray = MBoundedArray RealWorld
type STBoundedArray s = MBoundedArray s

type role MBoundedArray nominal nominal nominal

instance (Enum i, Bounded i) => G.MBoundedArray MBoundedArray i e where
  new !e = do
    arr <- newSmallArray (size @i) e
    pure $ MBA arr
  {-# INLINE new #-}

  read (MBA arr) = readSmallArray arr . toIndex
  {-# INLINE read #-}

  write (MBA arr) = writeSmallArray arr . toIndex
  {-# INLINE write #-}

  copy (MBA dest) (MBA src) = copySmallMutableArray dest 0 src 0 (size @i)
  {-# INLINE copy #-}

new :: (Enum i, Bounded i, PrimMonad m) => e -> m (MBoundedArray (PrimState m) i e)
new = G.new
{-# INLINE new #-}

read :: (Enum i, Bounded i, PrimMonad m) => MBoundedArray (PrimState m) i e -> i -> m e
read = G.read
{-# INLINE read #-}

write :: (Enum i, Bounded i, PrimMonad m) => MBoundedArray (PrimState m) i e -> i -> e -> m ()
write = G.write
{-# INLINE write #-}

copy
  :: (Enum i, Bounded i, PrimMonad m)
  => MBoundedArray (PrimState m) i e
  -> MBoundedArray (PrimState m) i e
  -> m ()
copy = G.copy
{-# INLINE copy #-}
