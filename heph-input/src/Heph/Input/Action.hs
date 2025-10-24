{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

-- |
-- High-performance, type-safe input action system for games and interactive applications.
--
-- This module provides the core abstractions for mapping low-level input events
-- (keyboard, mouse, gamepad) to high-level application actions with compile-time
-- validation and zero-cost runtime abstractions.
--
-- = Core Concepts
--
-- [Actions] Application-defined semantic input concepts (Jump, Move, Fire, etc.)
-- [Input Sources] Hardware input primitives (keys, buttons, analog sticks)
-- [Action Mappings] Validated bindings between actions and their input sources
-- [Input Aggregation] Combining multiple input sources into single action values
--
-- = Type Safety
--
-- The system uses phantom types, type families, and runtime reflection to ensure:
--
-- * Actions can only be bound to compatible input sources
-- * Input values have the correct type for their action
-- * Type compatibility verified through compile-time phantom types and runtime 'Type.Reflection'
-- * Efficient type erasure with 'unsafeCoerce' after reflection-based validation
--
-- = Performance
--
-- Key performance features:
--
-- * Bounded arrays for O(1) input lookups
-- * Double-buffered input state for efficient delta computation
-- * INLINE pragmas on hot path functions
-- * Carefully controlled use of 'unsafeCoerce' for type erasure
--
-- = Memory Layout
--
-- Input state is stored in contiguous bounded arrays indexed by 'Enum' instances,
-- providing cache-efficient access patterns and predictable memory usage.
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
  ActionMap,
  newActionMap,
  readActions,
  SomeAction (..),
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
import Heph.Input.Internal.BoundedArray.Boxed qualified as BA
import Heph.Input.Internal.BoundedArray.Boxed.Mutable qualified as MBA
import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Control.DeepSeq
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Coerce (coerce)
import Data.Foldable
import Data.Function
import Data.Kind
import Data.Monoid (Any (..))
import Data.Ord (clamp, comparing)
import Data.Primitive.SmallArray
import Data.Set (Set)
import Data.Set qualified as S
import Data.Traversable
import Data.Type.Equality
import Foreign (fromBool)
import GHC.Exts qualified
import GHC.Generics
import GHC.Stack (HasCallStack)
import Linear.Metric (quadrance)
import Linear.V2
import Type.Reflection
import Unsafe.Coerce (unsafeCoerce)

data ActionSource = Button | Axis1D | Axis2D
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)

instance NFData ActionSource

newtype Sensitivity = Sensitivity Float
  deriving (NFData, Show, Eq, Ord)

newtype Deadzone = Deadzone Float
  deriving (NFData, Show, Eq, Ord)

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

deriving instance Eq (InputSource k)
deriving instance Ord (InputSource k)

data SomeInputSource where
  SomeInputSource :: (Typeable src, HasActionState src) => InputSource src -> SomeInputSource

instance Eq SomeInputSource where
  (SomeInputSource s1) == (SomeInputSource s2) =
    case testEquality (typeOf s1) (typeOf s2) of
      Just Refl -> s1 == s2
      Nothing -> False

instance Ord SomeInputSource where
  compare (SomeInputSource s1) (SomeInputSource s2) =
    case testEquality (typeOf s1) (typeOf s2) of
      Just Refl -> compare s1 s2
      Nothing -> compare (someTypeRep s1) (someTypeRep s2)

data ActionMapping act where
  ActionMapping
    :: (Typeable src, HasActionState src)
    => act src
    -> Set (InputSource src)
    -> ActionMapping act

-- | This is purely defensive. If the library is used as intended, with 'makeAction',
-- you should never see either of these.
data ActionMapException where
  InvalidActionMapping :: (Actionlike act) => ActionMapping act -> ActionMapException
  InvalidActionMaxBound :: (Actionlike act) => SomeAction act -> ActionMapException

instance Show ActionMapException where
  show = \case
    InvalidActionMapping (ActionMapping act src) ->
      unlines
        [ "InvalidActionMapping: Actionlike instance " <> show (toActionId asEnum) <> " is invalid!"
        , "fromActionId (toActionId " <> show (toActionId asEnum) <> ") = " <> show (toActionId roundTrip)
        , ""
        , "Action type: " <> show (typeOf act)
        , "InputSource type: " <> show (typeOf src)
        ]
     where
      (asEnum, roundTrip) = actionRoundTrip act
    InvalidActionMaxBound (SomeAction act) ->
      unlines
        [ "InvalidActionMaxBound: Actionlike " <> show (typeRepTyCon (typeOf act)) <> " is invalid!"
        , "maxActionId = " <> show (maxActionIdOf act)
        , "toActionId = " <> show (toActionId (SomeAction act))
        , "Action type: " <> show (typeOf act)
        ]

instance Exception ActionMapException

assertValidMapping
  :: forall act m. (HasCallStack, MonadThrow m, Actionlike act) => ActionMapping act -> m ()
assertValidMapping (ActionMapping act src)
  | not isValidMapping = throwM $ InvalidActionMapping (ActionMapping act src)
  | toActionId (SomeAction act) > maxActionId @act = throwM $ InvalidActionMaxBound (SomeAction act)
  | otherwise = pure ()
 where
  isValidMapping =
    case actionRoundTrip act of
      (_, SomeAction act') ->
        case testEquality (typeOf act) (typeOf act') of
          Just Refl
            | toActionId (SomeAction act) == toActionId (SomeAction act') -> True
            | otherwise -> False
          Nothing -> False

actionRoundTrip
  :: (Actionlike act, Typeable src) => act src -> (SomeAction act, SomeAction act)
actionRoundTrip act =
  let asEnum = ActionAsEnum (SomeAction act)
      ActionAsEnum roundTrip = toEnum (fromEnum asEnum)
   in (coerce asEnum, roundTrip)

(~>)
  :: (HasActionState src)
  => act src
  -> [InputSource src]
  -> ActionMapping act
(~>) act src = ActionMapping act (S.fromList src)
infixr 8 ~>
{-# NOINLINE (~>) #-}

-- | A compiled, immutable map from actions to their bound 'InputSource's.
--
-- The 'ActionMap' is the runtime representation of the user-defined action
-- mappings. It is optimized for high-performance lookups, using a bounded
-- array internally. It is constructed once via 'newActionMap' from a list of
-- 'ActionMapping's and is treated as immutable thereafter.
data ActionMap act where
  -- | For maximum performance, this structure stores its input sources in a
  -- type-erased format using 'unsafeCoerce'. This is a deliberate design
  -- choice that is guaranteed to be safe by the surrounding API.
  --
  -- The safety invariant is established by the 'ActionMapping' constructor and
  -- the 'Actionlike' instance:
  --
  -- - The 'ActionMapping' ensures an @act src@ can only be mapped to sources of
  --   type @InputSource src@.
  -- - The 'Actionlike' instance guarantees a valid index for the lookup.
  -- - The 'readActions' function then safely coerces the sources back to their
  --   correct type, restoring type information that was proven correct when
  --   the map was created.
  --
  -- NOTE: I benchmarked several possible implementations. This implementation
  -- is faster than alternatives like safe casting using 'Data.Typeable.cast'
  -- by at least an order of magnitude, and averts any need to pull in singletons
  -- or implement custom type-level structures to construct a sound structure
  -- with similar performance.
  --
  -- As an added benefit, this representation seems to have really good L1/L2 cache
  -- locality on query.
  VeryUnsafeNoGoodActionMap
    :: BA.BoundedArray (ActionAsEnum (SomeAction act)) (SmallArray GHC.Exts.Any)
    -> ActionMap act

instance NFData (ActionMap act) where
  rnf (VeryUnsafeNoGoodActionMap mp) = foldl' (\_ -> rnf . toInputSource) () mp
   where
    toInputSource :: SmallArray GHC.Exts.Any -> SmallArray (InputSource src)
    toInputSource = unsafeCoerce

readActions
  :: (Actionlike act, Typeable src) => ActionMap act -> act src -> SmallArray (InputSource src)
readActions (VeryUnsafeNoGoodActionMap mp) act =
  unsafeCoerce <$> BA.index mp (ActionAsEnum (SomeAction act))
{-# INLINE readActions #-}

-- | Create a compiled action map from a list of action mappings.
--
-- This is the primary way to construct an 'ActionMap'. It validates all mappings,
-- aggregates multiple bindings to the same action, and compiles them into an
-- efficient lookup structure.
--
-- This function should be called once during initialization. The resulting
-- 'ActionMap' is immutable and can be reused across frames.
--
-- ==== __Examples__
--
-- @
-- data MyAction (src :: 'ActionSource') where
--   Jump :: MyAction 'Button'
--   Move :: MyAction 'Axis2D'
--
-- 'makeAction' ''MyAction
--
-- actionMap :: 'ActionMap' MyAction
-- actionMap = newActionMap
--   [ Jump '~>' ['Key' ScancodeSpace, 'GamepadButton' ControllerButtonA]
--   , Move '~>' ['LeftStick' 1.0 0.15]
--   ]
-- @
--
-- ==== __Validation__
--
-- The function validates that:
--
-- * All action IDs are within valid range
-- * The 'Actionlike' instance satisfies round-trip laws
-- * Sensitivity and deadzone values are clamped to safe ranges
--
-- Invalid configurations will result in runtime exceptions during construction.
newActionMap :: (HasCallStack, Actionlike act) => [ActionMapping act] -> ActionMap act
newActionMap mbEmptyActs = VeryUnsafeNoGoodActionMap $ BA.runArray do
  actionMap <- MBA.new []
  for_ @[] [minBound .. maxBound] \i -> do
    let !mapping = mappings `BA.index` i
        !arr = smallArrayFromList . map (\(SomeInputSource !src') -> unsafeCoerce src') . toList $ mapping
    MBA.write actionMap i arr
  pure $! actionMap
 where
  -- Scrub bad inputs.
  acts =
    mbEmptyActs
      & filter (\(ActionMapping _ src) -> not (null src))
      & map (\(ActionMapping act src) -> ActionMapping act (S.map cleanSource src))

  -- Make sure that input sources are reasonable and will not cause div by zero errors.
  cleanSource = \case
    SourceStick1D a sens (Deadzone dz) -> SourceStick1D a sens (Deadzone (clamp (-0.99, 0.99) dz))
    SourceStick x y sens (Deadzone dz) -> SourceStick x y sens (Deadzone (clamp (-0.99, 0.99) dz))
    src -> src

  -- A bounded array is essentially a highly specialized hashmap with a perfect
  -- hashing function defined by fromEnum and an exact capacity, which means we
  -- should be able to loop quite efficiently here to "compile" the input map.
  !mappings = BA.runArray do
    mp <- MBA.new mempty
    for_ acts \(ActionMapping act src) -> do
      assertValidMapping (ActionMapping act src)
      let !i = ActionAsEnum (SomeAction act)
      !old <- MBA.read mp i
      let !new = old `S.union` S.map SomeInputSource src
      MBA.write mp i new
    pure $! mp

data SomeAction (f :: ActionSource -> Type) where
  SomeAction :: (Typeable src, Typeable (f src)) => f src -> SomeAction f

instance Show (SomeAction f) where
  show (SomeAction f) = show (typeOf f)

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
class (Typeable f) => Actionlike f where
  toActionId :: SomeAction f -> Int
  fromActionId :: Int -> SomeAction f
  maxActionId :: Int

maxActionIdOf :: forall act src. (Actionlike act) => act src -> Int
maxActionIdOf _ = maxActionId @act

newtype ActionAsEnum act = ActionAsEnum act

instance (Actionlike act) => Enum (ActionAsEnum (SomeAction act)) where
  toEnum = ActionAsEnum . fromActionId
  {-# INLINE toEnum #-}
  fromEnum (ActionAsEnum act) = toActionId act
  {-# INLINE fromEnum #-}

instance (Actionlike act) => Bounded (ActionAsEnum (SomeAction act)) where
  minBound = ActionAsEnum $ fromActionId 0
  {-# INLINE minBound #-}
  maxBound = ActionAsEnum $ fromActionId (maxActionId @act)
  {-# INLINE maxBound #-}

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
  -> ActionMap act
  -> act src
  -> m (AbsoluteInput src)
absoluteInput s mp act = do
  r <- for (readActions mp act) (absoluteBufferedInput s)
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
  -> ActionMap act
  -> act src
  -> m (DeltaInput src)
deltaInput s mp act = do
  r <- for (readActions mp act) (deltaBufferedInput s)
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
