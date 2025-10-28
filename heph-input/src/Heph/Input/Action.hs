{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Heph.Input.Action (
  ActionMapping (..),
  (~>),
  ActionSource (..),
  InputSource (..),
  Sensitivity (..),
  Deadzone (..),
  pattern Key,
  pattern GamepadButton,
  pattern MouseButton,
  pattern GamepadTrigger,
  pattern GamepadStick,
  pattern LeftStick,
  pattern RightStick,
  pattern MouseAxis1D,
  pattern MouseAxis2D,
  pattern MouseMotion,
  pattern DPad,
  pattern AsAxis,
  Actionlike (..),
  AggregateInput (..),
  ButtonState (..),
  AbsoluteInput,
  DeltaInput,
  deltaInput,
  absoluteInput,
  HasActionState (..),
)
where

import Heph.Input.Buffer
import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Control.DeepSeq
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Foldable
import Data.Kind
import Data.Monoid (Any (..))
import Data.Ord (comparing)
import Data.Primitive.SmallArray
import Data.Set (Set)
import Data.Set qualified as S
import Data.Traversable
import Foreign (fromBool)
import GHC.Generics
import Linear.Metric (quadrance)
import Linear.V2
import Type.Reflection

data ActionSource = Button | Axis1D | Axis2D
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)

newtype Sensitivity = Sensitivity Float
  deriving (NFData, Show, Eq, Ord)

newtype Deadzone = Deadzone Float
  deriving (NFData, Show, Eq, Ord)

-- | A typeclass that enables a GADT to be used as a set of actions. It provides
-- a dense, zero-based mapping between action constructors and integer IDs.
--
-- This mapping is critical for the performance and safety of the 'ActionMap'.
-- The internal implementation of 'ActionMap' relies on a bounded array lookup,
-- which is only safe if the 'Actionlike' instance obeys these laws:
--
-- 1. @'fromActionId' . 'toActionId' == 'id'@ (Round-trip property)
-- 2. The generated IDs must form a continuous range from @0@ to @'maxActionId'@.
--
-- == IMPORTANT: Instance Declaration ==
-- You should never write a manual instance of 'Actionlike'. Doing so is
-- extremely unsafe and will likely lead to memory corruption.
--
-- Always use the provided Template Haskell function @makeAction@ to derive a
-- correct instance for your action type.
--
-- @
-- data MyAction (src :: ActionSource) where
--   Jump :: MyAction Button
--   Move :: MyAction Axis2D
--
-- makeAction ''MyAction
-- @
--
-- TODO: Most of this is outmoded. Need to update once refactor is done.
class (Typeable act, NFData (ActionMap2 act)) => Actionlike (act :: ActionSource -> Type) where
  -- TODO: Removing unsafe coerce, move to case-of-known-constructor
  data ActionMap2 act

  -- | Prefer 'compileActionsIO'. If you use this function, be sure to force it:
  --
  -- @@
  -- let !myActionMap = compileActions myActionMappings
  -- @@
  compileActions :: [ActionMapping act] -> ActionMap2 act

  actionSources
    :: (Typeable src)
    => ActionMap2 act
    -> act src
    -> SmallArray (InputSource src)

-- compileActionsIO :: MonadIO m => Actionlike act => [ActionMapping act] -> m (ActionMap2 act)
-- compileActionsIO mappings = do
--   let !m = compileActions mappings
--   evaluate (rnf m)
--   pure m

data InputSource (k :: ActionSource) where
  SourceButton :: InputButton -> InputSource Button
  SourceStick1D
    :: ControllerAxis
    -> Sensitivity
    -> Deadzone
    -> InputSource Axis1D
  SourceMouse1D
    :: MouseAxis
    -> Sensitivity
    -> InputSource Axis1D
  SourceButtonAsAxis :: InputButton -> InputSource Axis1D
  SourceStick
    :: ControllerAxis
    -- ^ X
    -> ControllerAxis
    -- ^ Y
    -> Sensitivity
    -> Deadzone
    -> InputSource Axis2D
  SourceMouseMotion
    :: MouseAxis
    -- ^ X
    -> MouseAxis
    -- ^ Y
    -> Sensitivity
    -> InputSource Axis2D
  SourceDPad
    :: InputButton
    -- ^ left
    -> InputButton
    -- ^ up
    -> InputButton
    -- ^ down
    -> InputButton
    -- ^ right
    -> InputSource Axis2D

deriving instance Eq (InputSource k)
deriving instance Ord (InputSource k)
deriving instance Show (InputSource k)

instance NFData (InputSource k) where
  rnf = \case
    SourceButton btn -> rnf btn
    SourceStick1D axis sens dz -> rnf axis `seq` rnf sens `seq` rnf dz
    SourceMouse1D axis sens -> rnf axis `seq` rnf sens
    SourceButtonAsAxis btn -> rnf btn
    SourceStick a1 a2 sens dz -> rnf a1 `seq` rnf a2 `seq` rnf sens `seq` rnf dz
    SourceMouseMotion a1 a2 sens -> rnf a1 `seq` rnf a2 `seq` rnf sens
    SourceDPad b0 b1 b2 b3 -> rnf b0 `seq` rnf b1 `seq` rnf b2 `seq` rnf b3

pattern Key :: Scancode -> InputSource Button
pattern Key sc = SourceButton (ScancodeButton sc)

pattern GamepadButton :: ControllerButton -> InputSource Button
pattern GamepadButton cb = SourceButton (ControllerButton cb)

pattern MouseButton :: MouseButton -> InputSource Button
pattern MouseButton mb = SourceButton (InputMouseButton mb)

pattern GamepadTrigger :: ControllerAxis -> Float -> Float -> InputSource Axis1D
pattern GamepadTrigger{_trigger, _triggerSensitivity, _triggerDeadzone} =
  SourceStick1D _trigger (Sensitivity _triggerSensitivity) (Deadzone _triggerDeadzone)

pattern GamepadStick :: ControllerAxis -> ControllerAxis -> Float -> Float -> InputSource Axis2D
pattern GamepadStick{_stickX, _stickY, _stickSensitivity, _stickDeadzone} =
  SourceStick _stickX _stickY (Sensitivity _stickSensitivity) (Deadzone _stickDeadzone)

pattern LeftStick :: Float -> Float -> InputSource Axis2D
pattern LeftStick{_leftStickSensitivity, _leftStickDeadzone} =
  SourceStick
    ControllerAxisLeftX
    ControllerAxisLeftY
    (Sensitivity _leftStickSensitivity)
    (Deadzone _leftStickDeadzone)

pattern RightStick :: Float -> Float -> InputSource Axis2D
pattern RightStick{_rightStickSensitivity, _rightStickDeadzone} =
  SourceStick
    ControllerAxisRightX
    ControllerAxisRightY
    (Sensitivity _rightStickSensitivity)
    (Deadzone _rightStickDeadzone)

pattern MouseAxis1D :: MouseAxis -> Float -> InputSource Axis1D
pattern MouseAxis1D{_mouseAxis, _mouseSensitivity1D} =
  SourceMouse1D _mouseAxis (Sensitivity _mouseSensitivity1D)

pattern MouseAxis2D :: MouseAxis -> MouseAxis -> Float -> InputSource Axis2D
pattern MouseAxis2D{_mouseX, _mouseY, _mouseSensitivity2D} =
  SourceMouseMotion _mouseX _mouseY (Sensitivity _mouseSensitivity2D)

pattern MouseMotion :: Float -> InputSource Axis2D
pattern MouseMotion{_mouseSensitivity} =
  SourceMouseMotion MouseX MouseY (Sensitivity _mouseSensitivity)

pattern DPad
  :: InputSource Button
  -> InputSource Button
  -> InputSource Button
  -> InputSource Button
  -> InputSource Axis2D
pattern DPad{_dpadLeft, _dpadUp, _dpadDown, _dpadRight} <-
  (unDPad -> Just (_dpadLeft, _dpadUp, _dpadDown, _dpadRight))
  where
    DPad (SourceButton l) (SourceButton u) (SourceButton d) (SourceButton r) =
      SourceDPad l u d r

unDPad
  :: InputSource Axis2D
  -> Maybe (InputSource Button, InputSource Button, InputSource Button, InputSource Button)
unDPad (SourceDPad l u d r) = Just (SourceButton l, SourceButton u, SourceButton d, SourceButton r)
unDPad _ = Nothing

--
pattern AsAxis :: InputSource Button -> InputSource Axis1D
pattern AsAxis btn <- (unAxis -> Just btn)
  where
    AsAxis (SourceButton btn) = SourceButtonAsAxis btn

unAxis :: InputSource Axis1D -> Maybe (InputSource Button)
unAxis (SourceButtonAsAxis btn) = Just (SourceButton btn)
unAxis _ = Nothing

-- TODO: User should be able to determine ordering in whichever way they please, which means that
-- I will need to move away from 'Set'. Since n is small (even large games unlikely to have more
-- than 4-5 actions per mapping), and we compile to an efficient form, it should be acceptable to
-- do a quadratic insertion on a linked list.
data ActionMapping act where
  ActionMapping
    :: (Typeable src, HasActionState src)
    => act src
    -> Set (InputSource src)
    -> ActionMapping act

(~>)
  :: (HasActionState src)
  => act src
  -> [InputSource src]
  -> ActionMapping act
(~>) act src = ActionMapping act (S.fromList src)
infixr 8 ~>

-- We should always be reducing our list of action mappings to normal form, so
-- inlining the convenience wrapper leads to code bloat from Set method inlines
-- with no real benefit.
{-# NOINLINE (~>) #-}

data ButtonState = JustPressed | JustReleased | Held | NotPressed
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)

instance NFData ButtonState

toButtonState :: Bool -> Bool -> ButtonState
toButtonState old new
  | old && new = Held
  | not old && new = JustPressed
  | old && not new = JustReleased
  | otherwise = NotPressed
{-# INLINEABLE toButtonState #-}

type family AbsoluteInput (src :: ActionSource) = (state :: Type) | state -> src where
  AbsoluteInput Button = Bool
  AbsoluteInput Axis1D = Float
  AbsoluteInput Axis2D = V2 Float

type family RawDeltaInput (src :: ActionSource) = (state :: Type) | state -> src where
  RawDeltaInput Button = (Bool, Bool)
  RawDeltaInput Axis1D = Float
  RawDeltaInput Axis2D = V2 Float

type family DeltaInput (src :: ActionSource) = (state :: Type) | state -> src where
  DeltaInput Button = ButtonState
  DeltaInput Axis1D = Float
  DeltaInput Axis2D = V2 Float

-- | Defines how multiple input sources are combined for a single action.
--
-- When multiple input sources are bound to the same action (e.g., both keyboard
-- and gamepad for Jump), their values must be aggregated into a single result.
-- This class defines the aggregation strategy for each action source type.
--
-- ==== __Aggregation Strategies__
--
-- * __'Button'__: OR'd together (any pressed = action pressed)
-- * __'Axis1D'__: Maximum by absolute value
-- * __'Axis2D'__: Vector with largest magnitude (quadrance)
--
-- ==== __Important Behavior Note__
--
-- For axes, the magnitude-based aggregation means keyboard input (which returns
-- ±1.0) will override partial gamepad input. For example:
--
-- @
-- -- WASD returns V2 1.0 0.0 (magnitude 1.0)
-- -- Gamepad stick returns V2 0.5 0.0 (magnitude 0.5)
-- -- Result: V2 1.0 0.0 (keyboard wins)
-- @
--
-- If you need different aggregation behavior, consider using separate actions
-- for keyboard and gamepad or implementing custom aggregation logic.
class AggregateInput (src :: ActionSource) where
  aggregateAbsolute :: (Foldable f) => f (AbsoluteInput src) -> AbsoluteInput src
  aggregateDelta :: (Foldable f) => f (RawDeltaInput src) -> DeltaInput src

instance AggregateInput Button where
  aggregateAbsolute = or
  {-# INLINE aggregateAbsolute #-}
  aggregateDelta = uncurry toButtonState . bimap getAny getAny . foldMap' (bimap Any Any)
  {-# INLINE aggregateDelta #-}

instance AggregateInput Axis1D where
  aggregateAbsolute = maximumOrDef 0
  {-# INLINE aggregateAbsolute #-}
  aggregateDelta = maximumOrDef 0
  {-# INLINE aggregateDelta #-}

instance AggregateInput Axis2D where
  aggregateAbsolute = maximumOrDefBy 0 (comparing quadrance)
  {-# INLINE aggregateAbsolute #-}
  aggregateDelta = maximumOrDefBy 0 (comparing quadrance)
  {-# INLINE aggregateDelta #-}

-- | Query the current state of an action.
--
-- Returns the aggregated value of all input sources bound to the action:
--
-- * 'Button' actions return 'Bool' (pressed or not)
-- * 'Axis1D' actions return 'Float' (current axis value)
-- * 'Axis2D' actions return @'V2' 'Float'@ (current 2D vector)
--
-- Use this for continuous actions like movement and aiming.
--
-- ==== __Examples__
--
-- @
-- -- Check if sprint is currently held
-- isSprinting <- 'absoluteInput' buffered actionMap Sprint
-- when isSprinting $ increaseSpeed
--
-- -- Get current movement direction
-- moveVec <- 'absoluteInput' buffered actionMap Move
-- -- moveVec :: V2 Float, e.g., V2 0.707 0.707 for diagonal movement
-- applyMovement moveVec
-- @
--
-- ==== __Performance__
--
-- This function is designed for frequent use in game loops and is heavily
-- optimized with INLINE pragmas and strict evaluation.
absoluteInput
  :: (MonadIO m, Actionlike act, HasActionState src)
  => BufferedInput
  -> ActionMap2 act
  -> act src
  -> m (AbsoluteInput src)
absoluteInput s mp act = do
  r <- for (actionSources mp act) (absoluteBufferedInput s)
  pure $! aggregateAbsolute r
{-# INLINE absoluteInput #-}

-- | Query the state change of an action since the last frame.
--
-- Returns the delta (change) in action state:
--
-- * 'Button' actions return 'ButtonState' ('JustPressed', 'Held', 'JustReleased', or 'NotPressed')
-- * 'Axis1D' actions return 'Float' (change in axis value)
-- * 'Axis2D' actions return @'V2' 'Float'@ (change in 2D vector)
--
-- Use this for event-driven actions like jumping, shooting, or menu navigation.
--
-- ==== __Examples__
--
-- @
-- -- Trigger jump only on button press, not while held
-- jumpState <- 'deltaInput' buffered actionMap Jump
-- when (jumpState == 'JustPressed') $ do
--   applyJumpVelocity
--
-- -- Mouse look with delta movement
-- lookDelta <- 'deltaInput' buffered actionMap Look
-- -- lookDelta :: V2 Float, e.g., V2 2.5 (-1.3) for mouse movement
-- rotateCamera lookDelta
-- @
--
-- ==== __Frame Preparation__
--
-- For delta queries to work correctly, you must call 'prepareBufferedInput'
-- at the start of each frame to swap the input buffers.
deltaInput
  :: (MonadIO m, Actionlike act, HasActionState src)
  => BufferedInput
  -> ActionMap2 act
  -> act src
  -> m (DeltaInput src)
deltaInput s mp act = do
  r <- for (actionSources mp act) (deltaBufferedInput s)
  pure $! aggregateDelta r
{-# INLINE deltaInput #-}

class (Typeable a, AggregateInput a) => HasActionState (a :: ActionSource) where
  absoluteBufferedInput :: (MonadIO m) => BufferedInput -> InputSource a -> m (AbsoluteInput a)
  deltaBufferedInput :: (MonadIO m) => BufferedInput -> InputSource a -> m (RawDeltaInput a)

instance HasActionState Button where
  absoluteBufferedInput state (SourceButton btn) = readButtonState state btn (\_ c -> c)
  {-# INLINEABLE absoluteBufferedInput #-}
  deltaBufferedInput state (SourceButton btn) = readButtonState state btn (,)
  {-# INLINEABLE deltaBufferedInput #-}

readButtonState :: (MonadIO m) => BufferedInput -> InputButton -> (Bool -> Bool -> r) -> m r
readButtonState cxt button f = liftIO case button of
  ScancodeButton sc ->
    f
      <$> MPA.read cxt.lastInput.kbScancodes sc
      <*> MPA.read cxt.thisInput.kbScancodes sc
  InputMouseButton mb ->
    f
      <$> MPA.read cxt.lastInput.mouseButtons mb
      <*> MPA.read cxt.thisInput.mouseButtons mb
  ControllerButton cb ->
    f
      <$> MPA.read cxt.lastInput.controllerButtons cb
      <*> MPA.read cxt.thisInput.controllerButtons cb
{-# INLINE readButtonState #-}

instance HasActionState Axis1D where
  absoluteBufferedInput state = \case
    SourceStick1D axis sens dz -> readAxis state (ControllerAxis axis) sens dz (\_ c -> c)
    SourceMouse1D axis sens -> readAxis state (MouseAxis axis) sens (Deadzone 0) (\_ c -> c)
    SourceButtonAsAxis btn -> readButtonState state btn (\_ c -> fromBool c)
  {-# INLINEABLE absoluteBufferedInput #-}
  deltaBufferedInput state = \case
    SourceStick1D axis sens dz -> readAxis state (ControllerAxis axis) sens dz subtract
    SourceMouse1D axis sens -> readAxis state (MouseAxis axis) sens (Deadzone 0) subtract
    SourceButtonAsAxis btn -> readButtonState state btn (\l c -> fromBool c - fromBool l)
  {-# INLINEABLE deltaBufferedInput #-}

--
readAxis
  :: (MonadIO m)
  => BufferedInput
  -> Axis
  -> Sensitivity
  -> Deadzone
  -> (Float -> Float -> Float)
  -> m Float
readAxis cxt axis (Sensitivity sens) (Deadzone dz) g = liftIO case axis of
  MouseAxis a ->
    f
      <$> MPA.read cxt.lastInput.mouseAxes a
      <*> MPA.read cxt.thisInput.mouseAxes a
  ControllerAxis a ->
    f
      <$> MPA.read cxt.lastInput.controllerAxes a
      <*> MPA.read cxt.thisInput.controllerAxes a
 where
  f = g . applySensitivity . applyDeadzone
  applySensitivity = (* sens)
  applyDeadzone v
    | abs v < dz = 0
    | otherwise = signum v * ((abs v - dz) / (1.0 - dz))
{-# INLINE readAxis #-}

instance HasActionState Axis2D where
  absoluteBufferedInput state = \case
    SourceStick x y sens dz ->
      V2
        <$> absoluteBufferedInput state (SourceStick1D x sens dz)
        <*> absoluteBufferedInput state (SourceStick1D y sens dz)
    SourceMouseMotion x y sens ->
      V2
        <$> absoluteBufferedInput state (SourceMouse1D x sens)
        <*> absoluteBufferedInput state (SourceMouse1D y sens)
    SourceDPad a w s d -> do
      as <-
        V2
          <$> absoluteBufferedInput state (SourceButtonAsAxis a)
          <*> absoluteBufferedInput state (SourceButtonAsAxis s)
      dw <-
        V2
          <$> absoluteBufferedInput state (SourceButtonAsAxis d)
          <*> absoluteBufferedInput state (SourceButtonAsAxis w)
      pure (dw - as)
  {-# INLINEABLE absoluteBufferedInput #-}
  deltaBufferedInput state = \case
    SourceStick x y sens dz ->
      V2
        <$> deltaBufferedInput state (SourceStick1D x sens dz)
        <*> deltaBufferedInput state (SourceStick1D y sens dz)
    SourceMouseMotion x y sens ->
      V2
        <$> deltaBufferedInput state (SourceMouse1D x sens)
        <*> deltaBufferedInput state (SourceMouse1D y sens)
    SourceDPad a w s d -> do
      as <-
        V2
          <$> deltaBufferedInput state (SourceButtonAsAxis a)
          <*> deltaBufferedInput state (SourceButtonAsAxis s)
      dw <-
        V2
          <$> deltaBufferedInput state (SourceButtonAsAxis d)
          <*> deltaBufferedInput state (SourceButtonAsAxis w)
      pure (dw - as)
  {-# INLINEABLE deltaBufferedInput #-}

maximumOrDef :: (Foldable t, Ord a) => a -> t a -> a
maximumOrDef def xs
  | null xs = def
  | otherwise = maximum xs
{-# INLINE maximumOrDef #-}

maximumOrDefBy :: (Foldable t) => a -> (a -> a -> Ordering) -> t a -> a
maximumOrDefBy def f xs
  | null xs = def
  | otherwise = maximumBy f xs
{-# INLINE maximumOrDefBy #-}
