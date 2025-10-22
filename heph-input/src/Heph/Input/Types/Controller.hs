{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Heph.Input.Types.Controller (
  ControllerButton (..),
  ControllerAxis (..),
) where

import Heph.Input.Internal.BoundedArray.Primitive.Mutable

import Control.DeepSeq
import GHC.Generics

data ControllerButton
  = ControllerButtonA
  | ControllerButtonB
  | ControllerButtonX
  | ControllerButtonY
  | ControllerButtonBack
  | ControllerButtonGuide
  | ControllerButtonStart
  | ControllerButtonLeftStick
  | ControllerButtonRightStick
  | ControllerButtonLeftShoulder
  | ControllerButtonRightShoulder
  | ControllerButtonDpadUp
  | ControllerButtonDpadDown
  | ControllerButtonDpadLeft
  | ControllerButtonDpadRight
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving (Primlike) via (PrimlikeEnum ControllerButton)

instance NFData ControllerButton

data ControllerAxis
  = ControllerAxisLeftX
  | ControllerAxisLeftY
  | ControllerAxisRightX
  | ControllerAxisRightY
  | ControllerAxisTriggerLeft
  | ControllerAxisTriggerRight
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving (Primlike) via (PrimlikeEnum ControllerAxis)

instance NFData ControllerAxis
