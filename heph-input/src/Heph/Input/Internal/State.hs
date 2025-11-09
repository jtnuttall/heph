{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Heph.Input.Internal.State (
  InputState,
  InputButton (..),
  ButtonState (..),
  Axis (..),
  InputBuffer,
  newInputState,
  newInputBuffer,
  setScancode,
  setMouseButton,
  setControllerButton,
  setMouseAxis,
  modifyMouseAxis,
  setControllerAxis,
  modifyControllerAxis,
  prepareInputState,
  buttonValue,
  buttonDelta,
  axisValue,
  axisDelta,
)
where

import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Control.DeepSeq
import Control.Monad.Primitive
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

instance Semigroup ButtonState where
  JustPressed <> _ = JustPressed
  _ <> JustPressed = JustPressed
  JustReleased <> _ = JustReleased
  _ <> JustReleased = JustReleased
  Held <> _ = Held
  _ <> Held = Held
  NotPressed <> NotPressed = NotPressed

instance Monoid ButtonState where
  mempty = NotPressed

data Axis
  = MouseAxis MouseAxis
  | ControllerAxis ControllerAxis
  deriving (Generic, Show, Eq, Ord)

instance NFData Axis

data InputBuffer s i a = InputBuffer
  { lastInput :: MPA.MBoundedArray s i a
  , thisInput :: MPA.MBoundedArray s i a
  }
  deriving (Generic)

instance NFData (InputBuffer s i a)

newInputBuffer
  :: (PrimMonad m, Enum i, Bounded i, MPA.Primlike a)
  => a
  -> m (InputBuffer (PrimState m) i a)
newInputBuffer a = InputBuffer <$> MPA.new a <*> MPA.new a

prepareInputBuffer
  :: (PrimMonad m, Enum i, Bounded i, MPA.Primlike a) => InputBuffer (PrimState m) i a -> m ()
prepareInputBuffer buf = MPA.copy buf.lastInput buf.thisInput

thisValue
  :: (Enum i, Bounded i, MPA.Primlike a, PrimMonad m)
  => InputBuffer (PrimState m) i a
  -> i
  -> m a
thisValue buf = MPA.read buf.thisInput
{-# INLINE thisValue #-}

setThisValue
  :: (Enum i, Bounded i, MPA.Primlike e, PrimMonad m)
  => InputBuffer (PrimState m) i e
  -> i
  -> e
  -> m ()
setThisValue buf = MPA.write buf.thisInput
{-# INLINE setThisValue #-}

modifyThisValue
  :: (Enum i, Bounded i, MPA.Primlike e, PrimMonad m)
  => InputBuffer (PrimState m) i e
  -> i
  -> (e -> e)
  -> m ()
modifyThisValue buf = MPA.modify buf.thisInput

deltaInput
  :: (Enum i, Bounded i, MPA.Primlike a, PrimMonad m)
  => InputBuffer (PrimState m) i a
  -> i
  -> m (a, a)
deltaInput buf i = do
  lastInput <- MPA.read buf.lastInput i
  thisInput <- MPA.read buf.thisInput i
  pure (lastInput, thisInput)
{-# INLINE deltaInput #-}

data InputState s = InputState
  { kbScancodes :: {-# UNPACK #-} InputBuffer s Scancode Bool
  , mouseButtons :: {-# UNPACK #-} InputBuffer s MouseButton Bool
  , controllerButtons :: {-# UNPACK #-} InputBuffer s ControllerButton Bool
  , mouseAxes :: {-# UNPACK #-} InputBuffer s MouseAxis Float
  , controllerAxes :: {-# UNPACK #-} InputBuffer s ControllerAxis Float
  }
  deriving (Generic)

instance NFData (InputState s)

buttonValue
  :: (PrimMonad m) => InputState (PrimState m) -> InputButton -> m Bool
buttonValue s = \case
  ScancodeButton sc -> thisValue s.kbScancodes sc
  InputMouseButton mb -> thisValue s.mouseButtons mb
  ControllerButton cb -> thisValue s.controllerButtons cb
{-# INLINE buttonValue #-}

buttonDelta
  :: (PrimMonad m) => InputState (PrimState m) -> InputButton -> m ButtonState
buttonDelta s i = do
  delta <- case i of
    ScancodeButton sc -> deltaInput s.kbScancodes sc
    InputMouseButton mb -> deltaInput s.mouseButtons mb
    ControllerButton cb -> deltaInput s.controllerButtons cb
  pure case delta of
    (True, True) -> Held
    (True, False) -> JustReleased
    (False, True) -> JustPressed
    (False, False) -> NotPressed
{-# INLINE buttonDelta #-}

axisValue :: (PrimMonad m) => InputState (PrimState m) -> Axis -> m Float
axisValue s = \case
  MouseAxis ma -> thisValue s.mouseAxes ma
  ControllerAxis ca -> thisValue s.controllerAxes ca
{-# INLINE axisValue #-}

axisDelta :: (PrimMonad m) => InputState (PrimState m) -> Axis -> m (Float, Float)
axisDelta s = \case
  MouseAxis ma -> deltaInput s.mouseAxes ma
  ControllerAxis ca -> deltaInput s.controllerAxes ca
{-# INLINE axisDelta #-}

newInputState :: (PrimMonad m) => m (InputState (PrimState m))
newInputState =
  InputState
    <$> newInputBuffer False
    <*> newInputBuffer False
    <*> newInputBuffer False
    <*> newInputBuffer 0
    <*> newInputBuffer 0

setScancode :: (PrimMonad m) => InputState (PrimState m) -> Scancode -> Bool -> m ()
setScancode mut = setThisValue mut.kbScancodes

setMouseButton :: (PrimMonad m) => InputState (PrimState m) -> MouseButton -> Bool -> m ()
setMouseButton mut = setThisValue mut.mouseButtons

setControllerButton :: (PrimMonad m) => InputState (PrimState m) -> ControllerButton -> Bool -> m ()
setControllerButton mut = setThisValue mut.controllerButtons

setMouseAxis :: (PrimMonad m) => InputState (PrimState m) -> MouseAxis -> Float -> m ()
setMouseAxis mut = setThisValue mut.mouseAxes

modifyMouseAxis
  :: (PrimMonad m)
  => InputState (PrimState m)
  -> MouseAxis
  -> (Float -> Float)
  -> m ()
modifyMouseAxis mut = modifyThisValue mut.mouseAxes

setControllerAxis :: (PrimMonad m) => InputState (PrimState m) -> ControllerAxis -> Float -> m ()
setControllerAxis mut = setThisValue mut.controllerAxes

modifyControllerAxis
  :: (PrimMonad m)
  => InputState (PrimState m)
  -> ControllerAxis
  -> (Float -> Float)
  -> m ()
modifyControllerAxis mut = modifyThisValue mut.controllerAxes

prepareInputState :: (PrimMonad m) => InputState (PrimState m) -> m ()
prepareInputState state = do
  prepareInputBuffer state.kbScancodes
  prepareInputBuffer state.mouseButtons
  prepareInputBuffer state.controllerButtons
  prepareInputBuffer state.mouseAxes
  MPA.set state.mouseAxes.thisInput 0
  prepareInputBuffer state.controllerAxes
{-# INLINEABLE prepareInputState #-}
