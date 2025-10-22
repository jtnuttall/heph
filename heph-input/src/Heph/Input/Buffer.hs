{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Heph.Input.Buffer (
  InputButton (..),
  Axis (..),
  BufferedInput (..),
  InputBuffer (..),
  copyInputBuffer,
  newBufferedInput,
  prepareBufferedInput,
) where

import Heph.Input.Internal.BoundedArray.Primitive.Mutable (IOBoundedArray)
import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Control.DeepSeq
import Control.Monad.IO.Class
import Generic.Data

data InputButton
  = ScancodeButton Scancode
  | InputMouseButton MouseButton
  | ControllerButton ControllerButton
  deriving (Generic, Show, Eq, Ord)

instance NFData InputButton

instance Enum InputButton where
  toEnum = gtoFiniteEnum
  {-# INLINE toEnum #-}
  fromEnum = gfromFiniteEnum
  {-# INLINE fromEnum #-}

instance Bounded InputButton where
  minBound = gminBound
  {-# INLINE minBound #-}
  maxBound = gmaxBound
  {-# INLINE maxBound #-}

data Axis
  = MouseAxis MouseAxis
  | ControllerAxis ControllerAxis
  deriving (Generic, Show, Eq, Ord)

instance NFData Axis

instance Enum Axis where
  toEnum = gtoFiniteEnum
  {-# INLINE toEnum #-}
  fromEnum = gfromFiniteEnum
  {-# INLINE fromEnum #-}

instance Bounded Axis where
  minBound = gminBound
  {-# INLINE minBound #-}
  maxBound = gmaxBound
  {-# INLINE maxBound #-}

data BufferedInput = BufferedInput
  { lastInput :: InputBuffer
  , thisInput :: InputBuffer
  }
  deriving (Generic)

instance NFData BufferedInput

newBufferedInput :: (MonadIO m) => m BufferedInput
newBufferedInput = BufferedInput <$> newInputBuffer <*> newInputBuffer
{-# INLINE newBufferedInput #-}

prepareBufferedInput :: (MonadIO m) => BufferedInput -> m ()
prepareBufferedInput state = liftIO do
  copyInputBuffer state.lastInput state.thisInput
  MPA.write state.thisInput.mouseAxes MouseX 0
  MPA.write state.thisInput.mouseAxes MouseY 0
{-# INLINE prepareBufferedInput #-}

data InputBuffer = InputBuffer
  { kbScancodes :: IOBoundedArray Scancode Bool
  , mouseButtons :: IOBoundedArray MouseButton Bool
  , controllerButtons :: IOBoundedArray ControllerButton Bool
  , mouseAxes :: IOBoundedArray MouseAxis Float
  , controllerAxes :: IOBoundedArray ControllerAxis Float
  }
  deriving (Generic)

instance NFData InputBuffer

newInputBuffer :: (MonadIO m) => m InputBuffer
newInputBuffer =
  liftIO $
    InputBuffer
      <$> MPA.new False
      <*> MPA.new False
      <*> MPA.new False
      <*> MPA.new 0
      <*> MPA.new 0

copyInputBuffer :: (MonadIO m) => InputBuffer -> InputBuffer -> m ()
copyInputBuffer dest src = liftIO do
  MPA.copy dest.kbScancodes src.kbScancodes
  MPA.copy dest.mouseButtons src.mouseButtons
  MPA.copy dest.controllerButtons src.controllerButtons
  MPA.copy dest.mouseAxes src.mouseAxes
  MPA.copy dest.controllerAxes src.controllerAxes
{-# INLINE copyInputBuffer #-}
