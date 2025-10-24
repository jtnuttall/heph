{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Heph.Input.Internal.BoundedArray.Boxed (
  BoundedArray,
  MBoundedArray,
  IOBoundedArray,
  STBoundedArray,
  replicate,
  index,
  map,
  runArray,
) where

import Heph.Input.Internal.BoundedArray.Boxed.Mutable
import Heph.Input.Internal.BoundedArray.Generic qualified as G
import Heph.Input.Internal.BoundedArray.Index

import Control.DeepSeq
import Control.Monad.ST.Strict
import Data.Primitive.SmallArray
import Prelude hiding (map, replicate)

newtype BoundedArray i e = BA (SmallArray e)
  deriving (NFData, Foldable)

type instance G.Mutable BoundedArray = MBoundedArray

type role BoundedArray nominal nominal

instance (Enum i, Bounded i) => G.BoundedArray BoundedArray i e where
  index :: BoundedArray i e -> i -> e
  index (BA arr) = indexSmallArray arr . toIndex
  {-# INLINE index #-}

  runArray :: (forall s. ST s (MBoundedArray s i e)) -> BoundedArray i e
  runArray act = BA $ runSmallArray do
    MBA arr <- act
    pure arr
  {-# INLINE runArray #-}

replicate :: (Enum i, Bounded i) => e -> BoundedArray i e
replicate = G.replicate
{-# INLINE replicate #-}

index :: (Enum i, Bounded i) => BoundedArray i e -> i -> e
index = G.index
{-# INLINE index #-}

map :: (e -> e1) -> BoundedArray i e -> BoundedArray i e1
map f (BA arr) = BA $ mapSmallArray' f arr
{-# INLINE map #-}

runArray :: (Enum i, Bounded i) => (forall s. ST s (MBoundedArray s i e)) -> BoundedArray i e
runArray = G.runArray @BoundedArray
{-# INLINE runArray #-}
