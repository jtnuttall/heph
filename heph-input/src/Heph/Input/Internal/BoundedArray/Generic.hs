{-# LANGUAGE TypeFamilyDependencies #-}

module Heph.Input.Internal.BoundedArray.Generic (
  Mutable,
  G.MBoundedArray,
  BoundedArray (..),
  length,
)
where

import Control.Monad.ST.Strict
import Data.Kind (Type)
import Prelude hiding (length)

import Heph.Input.Internal.BoundedArray.Generic.Mutable qualified as G
import Heph.Input.Internal.BoundedArray.Index

type family Mutable (arr :: Type -> Type -> Type) = (marr :: Type -> Type -> Type -> Type) | marr -> arr

class (G.MBoundedArray (Mutable a) i e) => BoundedArray a i e where
  replicate :: e -> a i e
  replicate e = runArray $ G.new e
  {-# INLINE replicate #-}
  index :: a i e -> i -> e
  runArray :: (forall s. ST s ((Mutable a) s i e)) -> a i e

length :: forall a i e. (BoundedArray a i e) => a i e -> Int
length _ = size @i
