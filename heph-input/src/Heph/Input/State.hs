{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Heph.Input.State (
  InputButton (..),
  ButtonState (..),
  Axis (..),
  InputBuffer,
  newInputState,
  newInputBuffer,
  HasInputState (..),
  withInputState,
)
where

import Heph.Input.Internal.BoundedArray.Primitive.Mutable (IOBoundedArray)
import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Control.DeepSeq
import Control.Monad.IO.Class
import GHC.Generics
import Heph.Input.Internal.BoundedArray.Primitive (BoundedArray)
import Heph.Input.Internal.BoundedArray.Primitive qualified as PA

data InputButton
  = ScancodeButton Scancode
  | InputMouseButton MouseButton
  | ControllerButton ControllerButton
  deriving (Generic, Show, Eq, Ord)

instance NFData InputButton

data ButtonState = JustPressed | JustReleased | Held | NotPressed
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)

instance NFData ButtonState

data Axis
  = MouseAxis MouseAxis
  | ControllerAxis ControllerAxis
  deriving (Generic, Show, Eq, Ord)

instance NFData Axis

data InputBuffer f i a = InputBuffer
  { lastInput :: f i a
  , thisInput :: f i a
  }
  deriving (Generic)

instance (NFData (f i a)) => NFData (InputBuffer f i a)

newInputBuffer
  :: (MonadIO m, Enum i, Bounded i, MPA.Primlike a) => a -> m (InputBuffer IOBoundedArray i a)
newInputBuffer a = liftIO $ InputBuffer <$> MPA.new a <*> MPA.new a

prepareInputBuffer
  :: (MonadIO m, Enum i, Bounded i, MPA.Primlike a) => InputBuffer IOBoundedArray i a -> m ()
prepareInputBuffer buf = liftIO $ MPA.copy buf.lastInput buf.thisInput

thisValue :: (Enum i, Bounded i, MPA.Primlike a) => InputBuffer BoundedArray i a -> i -> a
thisValue buf = PA.index buf.thisInput
{-# INLINE thisValue #-}

deltaInput :: (Enum i, Bounded i, MPA.Primlike a) => InputBuffer BoundedArray i a -> i -> (a, a)
deltaInput buf i = (PA.index buf.lastInput i, thisValue buf i)
{-# INLINE deltaInput #-}

unsafeFreezeInputBuffer
  :: (MonadIO m, Enum i, Bounded i, MPA.Primlike a)
  => InputBuffer IOBoundedArray i a
  -> m (InputBuffer BoundedArray i a)
unsafeFreezeInputBuffer buf = liftIO do
  lastInput <- PA.unsafeFreeze buf.lastInput
  thisInput <- PA.unsafeFreeze buf.thisInput
  pure InputBuffer{..}

class HasInputState s where
  buttonValue :: s -> InputButton -> Bool
  buttonDelta :: s -> InputButton -> ButtonState
  axisValue :: s -> Axis -> Float

data InputState f = InputState
  { kbScancodes :: InputBuffer f Scancode Bool
  , mouseButtons :: InputBuffer f MouseButton Bool
  , controllerButtons :: InputBuffer f ControllerButton Bool
  , mouseAxes :: f MouseAxis Float
  , controllerAxes :: f ControllerAxis Float
  }
  deriving (Generic)

instance NFData (InputState IOBoundedArray)
instance NFData (InputState BoundedArray)

instance HasInputState (InputState BoundedArray) where
  buttonValue s = \case
    ScancodeButton sc -> thisValue s.kbScancodes sc
    InputMouseButton mb -> thisValue s.mouseButtons mb
    ControllerButton cb -> thisValue s.controllerButtons cb
  {-# INLINEABLE buttonValue #-}
  buttonDelta s i =
    let delta = case i of
          ScancodeButton sc -> deltaInput s.kbScancodes sc
          InputMouseButton mb -> deltaInput s.mouseButtons mb
          ControllerButton cb -> deltaInput s.controllerButtons cb
     in case delta of
          (True, True) -> Held
          (True, False) -> JustReleased
          (False, True) -> JustPressed
          (False, False) -> NotPressed
  {-# INLINEABLE buttonDelta #-}
  axisValue s = \case
    MouseAxis ma -> PA.index s.mouseAxes ma
    ControllerAxis ca -> PA.index s.controllerAxes ca
  {-# INLINEABLE axisValue #-}

newInputState :: (MonadIO m) => m (InputState IOBoundedArray)
newInputState =
  liftIO $
    InputState
      <$> newInputBuffer False
      <*> newInputBuffer False
      <*> newInputBuffer False
      <*> MPA.new 0
      <*> MPA.new 0

withInputState
  :: (MonadIO m) => InputState IOBoundedArray -> (forall s. (HasInputState s) => s -> m r) -> m r
withInputState state act = do
  prepareInputState state
  frozen <- unsafeFreezeInputState state
  act frozen

prepareInputState :: (MonadIO m) => InputState IOBoundedArray -> m ()
prepareInputState state = liftIO do
  prepareInputBuffer state.kbScancodes
  prepareInputBuffer state.mouseButtons
  prepareInputBuffer state.controllerButtons
  MPA.set state.mouseAxes 0

unsafeFreezeInputState :: (MonadIO m) => InputState IOBoundedArray -> m (InputState BoundedArray)
unsafeFreezeInputState mut = liftIO do
  kbScancodes <- unsafeFreezeInputBuffer mut.kbScancodes
  mouseButtons <- unsafeFreezeInputBuffer mut.mouseButtons
  controllerButtons <- unsafeFreezeInputBuffer mut.controllerButtons
  mouseAxes <- PA.unsafeFreeze mut.mouseAxes
  controllerAxes <- PA.unsafeFreeze mut.controllerAxes
  pure InputState{..}
