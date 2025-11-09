{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Heph.Input.Types.Mouse (
  MouseButton (..),
  MouseAxis (..),
) where

import Heph.Input.Internal.BoundedArray.Primitive.Mutable

import Control.DeepSeq
import GHC.Generics

data MouseButton
  = MouseButtonLeft
  | MouseButtonMiddle
  | MouseButtonRight
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving (Primlike) via (PrimlikeEnum MouseButton)

instance NFData MouseButton

data MouseAxis
  = MouseX
  | MouseY
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving (Primlike) via (PrimlikeEnum MouseAxis)

instance NFData MouseAxis
