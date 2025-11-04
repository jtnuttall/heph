{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Heph.Input.Internal.BoundedArray.Primitive.Mutable (
  MBoundedArray (..),
  IOBoundedArray,
  STBoundedArray,
  Primlike (..),
  PrimlikeEnum (..),
  new,
  read,
  write,
  copy,
  set,
) where

import Control.Monad.Primitive
import Data.Primitive.PrimArray
import Data.Primitive.Types
import Foreign (fromBool, toBool)
import Foreign.C.Types
import Prelude hiding (read)

import Control.DeepSeq (NFData)
import Heph.Input.Internal.BoundedArray.Generic.Mutable qualified as G
import Heph.Input.Internal.BoundedArray.Index

newtype MBoundedArray s i e = MBA (MutablePrimArray s (PrimRep e))
  deriving (NFData)

type IOBoundedArray = MBoundedArray RealWorld
type STBoundedArray s = MBoundedArray s

type role MBoundedArray nominal nominal nominal

class (Prim (PrimRep a)) => Primlike a where
  type PrimRep a
  toPrim :: a -> PrimRep a
  fromPrim :: PrimRep a -> a

instance Primlike Bool where
  type PrimRep Bool = CBool
  toPrim = fromBool
  {-# INLINE toPrim #-}
  fromPrim = toBool
  {-# INLINE fromPrim #-}

instance Primlike Float where
  type PrimRep Float = Float
  toPrim = id
  {-# INLINE toPrim #-}
  fromPrim = id
  {-# INLINE fromPrim #-}

newtype PrimlikeEnum a = PrimlikeEnum a

instance (Enum a) => Primlike (PrimlikeEnum a) where
  type PrimRep (PrimlikeEnum a) = Int
  toPrim (PrimlikeEnum a) = fromEnum a
  {-# INLINE toPrim #-}
  fromPrim = PrimlikeEnum . toEnum
  {-# INLINE fromPrim #-}

instance (Enum i, Bounded i, Primlike e) => G.MBoundedArray MBoundedArray i e where
  new !e = do
    let sz = size @i
    arr <- newAlignedPinnedPrimArray sz
    setPrimArray arr 0 sz (toPrim e)
    pure $ MBA arr
  {-# INLINE new #-}

  read (MBA arr) = fmap fromPrim . readPrimArray arr . toIndex
  {-# INLINE read #-}

  write (MBA arr) i = writePrimArray arr (toIndex i) . toPrim
  {-# INLINE write #-}

  copy (MBA dest) (MBA src) = copyMutablePrimArray dest 0 src 0 (size @i)
  {-# INLINE copy #-}

  set (MBA arr) = setPrimArray arr 0 (size @i) . toPrim

new :: (Enum i, Bounded i, Primlike e, PrimMonad m) => e -> m (MBoundedArray (PrimState m) i e)
new = G.new @MBoundedArray
{-# INLINE new #-}

read :: (Enum i, Bounded i, Primlike e, PrimMonad m) => MBoundedArray (PrimState m) i e -> i -> m e
read = G.read @MBoundedArray
{-# INLINE read #-}

write
  :: (Enum i, Bounded i, Primlike e, PrimMonad m) => MBoundedArray (PrimState m) i e -> i -> e -> m ()
write = G.write @MBoundedArray
{-# INLINE write #-}

copy
  :: (Enum i, Bounded i, Primlike e, PrimMonad m)
  => MBoundedArray (PrimState m) i e
  -> MBoundedArray (PrimState m) i e
  -> m ()
copy = G.copy
{-# INLINE copy #-}

set
  :: (Enum i, Bounded i, Primlike e, PrimMonad m) => MBoundedArray (PrimState m) i e -> e -> m ()
set = G.set
