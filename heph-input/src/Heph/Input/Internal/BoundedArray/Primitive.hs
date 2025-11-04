{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Heph.Input.Internal.BoundedArray.Primitive (
  BoundedArray (..),
  MBoundedArray,
  IOBoundedArray,
  STBoundedArray,
  replicate,
  index,
  runArray,
  unsafeFreeze,
) where

import Heph.Input.Internal.BoundedArray.Generic qualified as G
import Heph.Input.Internal.BoundedArray.Index
import Heph.Input.Internal.BoundedArray.Primitive.Mutable

import Control.DeepSeq
import Control.Monad.Primitive
import Control.Monad.ST.Strict
import Data.Primitive.PrimArray
import Prelude hiding (replicate)

newtype BoundedArray i e = BA (PrimArray (PrimRep e))
  deriving (NFData)

type instance G.Mutable BoundedArray = MBoundedArray

type role BoundedArray nominal nominal

instance (Enum i, Bounded i, Primlike e) => G.BoundedArray BoundedArray i e where
  replicate = BA . replicatePrimArray (size @i) . toPrim
  {-# INLINE replicate #-}

  index (BA arr) = fromPrim . indexPrimArray arr . toIndex
  {-# INLINE index #-}

  runArray act = BA $ runPrimArray do
    MBA arr <- act
    pure arr
  {-# INLINE runArray #-}

  unsafeFreeze (MBA arr) = BA <$> unsafeFreezePrimArray arr

replicate :: (Enum i, Bounded i, Primlike e) => e -> BoundedArray i e
replicate = G.replicate
{-# INLINE replicate #-}

index :: (Enum i, Bounded i, Primlike e) => BoundedArray i e -> i -> e
index = G.index
{-# INLINE index #-}

runArray
  :: (Enum i, Bounded i, Primlike e) => (forall s. ST s (MBoundedArray s i e)) -> BoundedArray i e
runArray = G.runArray
{-# INLINE runArray #-}

unsafeFreeze
  :: (G.BoundedArray a i e, PrimMonad m)
  => G.Mutable a (PrimState m) i e
  -> m (a i e)
unsafeFreeze = G.unsafeFreeze
