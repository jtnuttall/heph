{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Heph.Input.Internal.State (
  InputButton (..),
  ButtonState (..),
  Axis (..),
  InputBuffer,
  newInputState,
  newInputBuffer,
  HasInputState (..),
  withInputState,
  unsafeWithInputState,
  MutableInputState,
  setScancode,
  setMouseButton,
  setControllerButton,
  setMouseAxis,
  modifyMouseAxis,
  setControllerAxis,
  modifyControllerAxis,
)
where

import Heph.Input.Internal.BoundedArray.Primitive (BoundedArray)
import Heph.Input.Internal.BoundedArray.Primitive qualified as PA
import Heph.Input.Internal.BoundedArray.Primitive.Mutable (IOBoundedArray)
import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Control.DeepSeq
import Control.Monad.IO.Class
import GHC.Generics

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

alterInputBuffer :: (Monad m) => (f i a -> m (g i a)) -> InputBuffer f i a -> m (InputBuffer g i a)
alterInputBuffer f buf = do
  lastInput <- f buf.lastInput
  thisInput <- f buf.thisInput
  pure InputBuffer{..}

thisValue :: (Enum i, Bounded i, MPA.Primlike a) => InputBuffer BoundedArray i a -> i -> a
thisValue buf = PA.index buf.thisInput
{-# INLINE thisValue #-}

setThisValue
  :: (Enum i, Bounded i, MPA.Primlike e, MonadIO m) => InputBuffer IOBoundedArray i e -> i -> e -> m ()
setThisValue buf i = liftIO . MPA.write buf.thisInput i
{-# INLINE setThisValue #-}

modifyThisValue
  :: (Enum i, Bounded i, MPA.Primlike e, MonadIO m)
  => InputBuffer IOBoundedArray i e
  -> i
  -> (e -> e)
  -> m ()
modifyThisValue buf i = liftIO . MPA.modify buf.thisInput i

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
  { kbScancodes :: {-# UNPACK #-} InputBuffer f Scancode Bool
  , mouseButtons :: {-# UNPACK #-} InputBuffer f MouseButton Bool
  , controllerButtons :: {-# UNPACK #-} InputBuffer f ControllerButton Bool
  , mouseAxes :: {-# UNPACK #-} InputBuffer f MouseAxis Float
  , controllerAxes :: {-# UNPACK #-} InputBuffer f ControllerAxis Float
  }
  deriving (Generic)

instance NFData (InputState IOBoundedArray)
instance NFData (InputState BoundedArray)

alterInputState
  :: (Monad m)
  => (forall i a. (Enum i, Bounded i, MPA.Primlike a) => f i a -> m (g i a))
  -> InputState f
  -> m (InputState g)
alterInputState f state = do
  kbScancodes <- alterInputBuffer f state.kbScancodes
  mouseButtons <- alterInputBuffer f state.mouseButtons
  controllerButtons <- alterInputBuffer f state.controllerButtons
  mouseAxes <- alterInputBuffer f state.mouseAxes
  controllerAxes <- alterInputBuffer f state.controllerAxes

  pure InputState{..}

instance HasInputState (InputState BoundedArray) where
  buttonValue s = \case
    ScancodeButton sc -> thisValue s.kbScancodes sc
    InputMouseButton mb -> thisValue s.mouseButtons mb
    ControllerButton cb -> thisValue s.controllerButtons cb
  {-# INLINE buttonValue #-}
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
  {-# INLINE buttonDelta #-}
  axisValue s = \case
    MouseAxis ma -> thisValue s.mouseAxes ma
    ControllerAxis ca -> thisValue s.controllerAxes ca
  {-# INLINE axisValue #-}

newtype MutableInputState = MutableInputState (InputState IOBoundedArray)

newInputState :: (MonadIO m) => m MutableInputState
newInputState =
  liftIO $
    fmap MutableInputState $
      InputState
        <$> newInputBuffer False
        <*> newInputBuffer False
        <*> newInputBuffer False
        <*> newInputBuffer 0
        <*> newInputBuffer 0

-- | Performs a full copy of the input state. This is safe and allows the state to escape the
-- closure, but will perform more poorly.
withInputState
  :: (MonadIO m)
  => MutableInputState
  -> (InputState IOBoundedArray -> m a)
  -> (forall s. (HasInputState s) => a -> s -> m r)
  -> m r
withInputState (MutableInputState state) update act = do
  prepareInputState state
  res <- update state
  frozen <- liftIO $ alterInputState PA.freeze state
  act res frozen

-- | This should allow GHC to reorder array indexing at zero cost, but be warned that no
-- part of the immutable input state should escape the closure, and no modifications to the
-- input state should be made outside of the modification function, as it internally unsafely
-- freezes the mutable array.
unsafeWithInputState
  :: (MonadIO m)
  => MutableInputState
  -> (InputState IOBoundedArray -> m a)
  -> (forall s. (HasInputState s) => a -> s -> m r)
  -> m r
unsafeWithInputState (MutableInputState state) update act = do
  prepareInputState state
  res <- update state
  frozen <- liftIO $ alterInputState PA.unsafeFreeze state
  act res frozen

setScancode :: (MonadIO m) => InputState IOBoundedArray -> Scancode -> Bool -> m ()
setScancode mut = setThisValue mut.kbScancodes

setMouseButton :: (MonadIO m) => InputState IOBoundedArray -> MouseButton -> Bool -> m ()
setMouseButton mut = setThisValue mut.mouseButtons

setControllerButton :: (MonadIO m) => InputState IOBoundedArray -> ControllerButton -> Bool -> m ()
setControllerButton mut = setThisValue mut.controllerButtons

setMouseAxis :: (MonadIO m) => InputState IOBoundedArray -> MouseAxis -> Float -> m ()
setMouseAxis mut = setThisValue mut.mouseAxes

modifyMouseAxis :: (MonadIO m) => InputState IOBoundedArray -> MouseAxis -> (Float -> Float) -> m ()
modifyMouseAxis mut = modifyThisValue mut.mouseAxes

setControllerAxis :: (MonadIO m) => InputState IOBoundedArray -> ControllerAxis -> Float -> m ()
setControllerAxis mut = setThisValue mut.controllerAxes

modifyControllerAxis
  :: (MonadIO m) => InputState IOBoundedArray -> ControllerAxis -> (Float -> Float) -> m ()
modifyControllerAxis mut = modifyThisValue mut.controllerAxes

prepareInputState :: (MonadIO m) => InputState IOBoundedArray -> m ()
prepareInputState state = liftIO do
  prepareInputBuffer state.kbScancodes
  prepareInputBuffer state.mouseButtons
  prepareInputBuffer state.controllerButtons
  prepareInputBuffer state.mouseAxes
  MPA.set state.mouseAxes.thisInput 0
  prepareInputBuffer state.controllerAxes
