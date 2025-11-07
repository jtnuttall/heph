{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Heph.Input.Action2 (
  ActionMapping (..),
  ActionSet (..),
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
  HasAggregationStrategy (..),
)
where

import Heph.Input.Internal.State
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Control.DeepSeq
import Data.Bits
import Data.Foldable
import Data.Kind
import Data.Ord (comparing)
import Data.Primitive.SmallArray
import GHC.Generics
import Linear (V2, quadrance)
import Type.Reflection

data ActionSource = Button | Axis1D | Axis2D
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)

type family CurrentInputOf (src :: ActionSource) = (input :: Type) | input -> src where
  CurrentInputOf Button = Bool
  CurrentInputOf Axis1D = Float
  CurrentInputOf Axis2D = V2 Float

type family DeltaInputOf (src :: ActionSource) = (input :: Type) | input -> src where
  DeltaInputOf Button = ButtonState
  DeltaInputOf Axis1D = Float
  DeltaInputOf Axis2D = V2 Float

-- | A newtype for holding input sources - eta reduces the type so it can be
-- used with 'DMap'
newtype ActionSet (src :: ActionSource) = ActionSet {unActionSet :: [InputSource src]}
  deriving (NFData, Semigroup, Monoid)

-- TODO: This may actually just be 'DSum' at this point ha
data ActionMapping act where
  (:=>)
    :: act src
    -> [InputSource src]
    -> ActionMapping act

newtype Sensitivity = Sensitivity Float
  deriving (NFData, Show, Eq, Ord)

newtype Deadzone = Deadzone Float
  deriving (NFData, Show, Eq, Ord)

class (Typeable act, NFData (ActionMap act)) => Actionlike (act :: ActionSource -> Type) where
  data ActionMap act

  -- | Be sure to force:
  --
  -- @@
  -- let !myActionMap = compileActions myActionMappings
  -- @@
  compileActions :: (Foldable t) => t (ActionMapping act) -> ActionMap act

  actionSources
    :: (Typeable src)
    => ActionMap act
    -> act src
    -> SmallArray (InputSource src)

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

class HasAggregationStrategy (c :: act src) where
  aggregateThisInput :: (Traversable t) => proxy c -> t (CurrentInputOf src) -> CurrentInputOf src
  aggregateDeltaInput :: (Traversable t) => proxy c -> t (DeltaInputOf src) -> DeltaInputOf src

instance {-# OVERLAPPABLE #-} HasAggregationStrategy (c :: act Button) where
  aggregateThisInput _ = getIor . foldMap Ior
  aggregateDeltaInput _ = fold

instance {-# OVERLAPPABLE #-} HasAggregationStrategy (c :: act Axis1D) where
  aggregateThisInput _ = safeMaximumBy abs 0
  aggregateDeltaInput _ = safeMaximumBy abs 0

instance {-# OVERLAPPABLE #-} HasAggregationStrategy (c :: act Axis2D) where
  aggregateThisInput _ = safeMaximumBy quadrance 0
  aggregateDeltaInput _ = safeMaximumBy quadrance 0

--------------------------------------------------------------------------------
-- Utitilities
--------------------------------------------------------------------------------

safeMaximumBy :: (Foldable t, Ord a) => (p -> a) -> p -> t p -> p
safeMaximumBy f def xs
  | null xs = def
  | otherwise = maximumBy (comparing f) xs
