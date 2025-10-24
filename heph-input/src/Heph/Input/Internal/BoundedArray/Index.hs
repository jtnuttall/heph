{-# LANGUAGE AllowAmbiguousTypes #-}

module Heph.Input.Internal.BoundedArray.Index (
  size,
  offset,
  toIndex,
) where

size :: forall i. (Enum i, Bounded i) => Int
size = fromEnum (maxBound @i) - fromEnum (minBound @i) + 1
{-# INLINE size #-}

offset :: forall i. (Enum i, Bounded i) => Int
offset = fromEnum (minBound @i)
{-# INLINE offset #-}

toIndex :: forall i. (Enum i, Bounded i) => i -> Int
toIndex i = fromEnum i - offset @i
{-# INLINE toIndex #-}
