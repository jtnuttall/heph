{-# LANGUAGE PatternSynonyms #-}

-- |
-- Type-safe, high-performance input action mapping system.
--
-- This module provides the primary interface for defining and using input actions
-- in games and interactive applications. Instead of checking low-level input
-- (\"is the Space key pressed?\"), you define semantic actions (\"is Jump active?\")
-- and bind them to input sources.
--
-- = Quick Start
--
-- Define your actions as a GADT, generate the typeclass instance, and create bindings:
--
-- @
-- {\-# LANGUAGE DataKinds #-\}
-- {\-# LANGUAGE TemplateHaskell #-\}
-- {\-# LANGUAGE TypeFamilies #-\}
--
-- import Heph.Input
--
-- -- Define your game's actions
-- data GameAction (src :: 'ActionSource') where
--   Jump   :: GameAction 'Button'
--   Sprint :: GameAction 'Button'
--   Move   :: GameAction 'Axis2D'
--
-- -- Generate the Actionlike instance
-- 'makeAction' ''GameAction
--
-- -- Create input bindings
-- bindings :: ['ActionMapping' GameAction]
-- bindings =
--   [ Jump   '~>' ['Key' ScancodeSpace, 'GamepadButton' ControllerButtonA]
--   , Sprint '~>' ['Key' ScancodeLShift]
--   , Move   '~>' ['DPad' ('Key' ScancodeA) ('Key' ScancodeW) ('Key' ScancodeS) ('Key' ScancodeD)
--               , 'LeftStick' 1.0 0.15
--               ]
--   ]
--
-- -- Compile the action map (once at initialization)
-- actionMap :: 'ActionMap' GameAction
-- actionMap = 'newActionMap' bindings
-- @
--
-- = Runtime Usage
--
-- Query actions in your game loop:
--
-- @
-- -- Initialize input system
-- buffered <- 'newBufferedInput'
--
-- -- At start of each frame
-- 'prepareBufferedInput' buffered
--
-- -- Process events and write to buffer...
-- -- (requires SDL integration library)
--
-- -- Query actions
-- jumpPressed <- 'deltaInput' buffered actionMap Jump
-- moveVec <- 'absoluteInput' buffered actionMap Move
-- @
--
-- = Key Concepts
--
-- * __Actions__: Semantic input concepts (Jump, Move, Fire) defined as a GADT
-- * __Input Sources__: Hardware primitives ('Key', 'GamepadButton', 'LeftStick', etc.)
-- * __Action Mappings__: Bindings between actions and their input sources
-- * __Buffered Input__: Double-buffered state for efficient delta queries
--
-- See "Heph.Input.Action" for detailed documentation on the action system.
module Heph.Input (
  module Action,
  module ActionTH,
  module Input,
) where

import Heph.Input.Action as Action (
  AbsoluteInput,
  ActionMapping (..),
  ActionSource (..),
  Actionlike (..),
  AggregateInput (..),
  ButtonState (..),
  Deadzone (..),
  DeltaInput,
  HasActionState (..),
  InputSource (..),
  Sensitivity (..),
  absoluteInput,
  deltaInput,
  (~>),
  pattern AsAxis,
  pattern DPad,
  pattern GamepadButton,
  pattern GamepadStick,
  pattern GamepadTrigger,
  pattern Key,
  pattern LeftStick,
  pattern MouseAxis1D,
  pattern MouseAxis2D,
  pattern MouseButton,
  pattern MouseMotion,
  pattern RightStick,
 )
import Heph.Input.Action.TH as ActionTH (makeAction)
import Heph.Input.Types.Controller as Input
import Heph.Input.Types.Mouse as Input
import Heph.Input.Types.Scancode as Input
