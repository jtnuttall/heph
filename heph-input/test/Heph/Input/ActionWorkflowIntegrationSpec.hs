{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Heph.Input.ActionWorkflowIntegrationSpec where

import Heph.Input.Action
import Heph.Input.Action.TH
import Heph.Input.Buffer
import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Linear.V2
import Test.Tasty.HUnit

-- Test action GADT for integration tests
data PlayerAction (src :: ActionSource) where
  Jump :: PlayerAction Button
  Crouch :: PlayerAction Button
  Move :: PlayerAction Axis2D
  Look :: PlayerAction Axis1D

makeAction ''PlayerAction

-- Integration test: Full input workflow
unit_fullButtonWorkflow :: Assertion
unit_fullButtonWorkflow = do
  -- Create input buffers
  buffered <- newBufferedInput

  -- Create action map
  let actionMap = compileActions [Jump ~> [Key ScancodeSpace]]

  -- Simulate pressing space
  MPA.write buffered.thisInput.kbScancodes ScancodeSpace True

  -- Read absolute input
  isPressed <- absoluteInput buffered actionMap Jump
  isPressed @?= True

  -- Read delta input (should be JustPressed)
  state <- deltaInput buffered actionMap Jump
  state @?= JustPressed

unit_fullButtonReleaseWorkflow :: Assertion
unit_fullButtonReleaseWorkflow = do
  buffered <- newBufferedInput
  let actionMap = compileActions [Jump ~> [Key ScancodeSpace]]

  -- Press in last frame
  MPA.write buffered.lastInput.kbScancodes ScancodeSpace True
  -- Release in this frame
  MPA.write buffered.thisInput.kbScancodes ScancodeSpace False

  -- Read delta input (should be JustReleased)
  state <- deltaInput buffered actionMap Jump
  state @?= JustReleased

unit_fullButtonHeldWorkflow :: Assertion
unit_fullButtonHeldWorkflow = do
  buffered <- newBufferedInput
  let actionMap = compileActions [Jump ~> [Key ScancodeSpace]]

  -- Held in both frames
  MPA.write buffered.lastInput.kbScancodes ScancodeSpace True
  MPA.write buffered.thisInput.kbScancodes ScancodeSpace True

  state <- deltaInput buffered actionMap Jump
  state @?= Held

unit_multipleSourcesAggregation :: Assertion
unit_multipleSourcesAggregation = do
  buffered <- newBufferedInput
  let actionMap = compileActions [Jump ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]]

  -- Only one source pressed
  MPA.write buffered.thisInput.kbScancodes ScancodeSpace True

  isPressed <- absoluteInput buffered actionMap Jump
  isPressed @?= True

  -- Both sources pressed
  MPA.write buffered.thisInput.controllerButtons ControllerButtonA True

  isPressed2 <- absoluteInput buffered actionMap Jump
  isPressed2 @?= True

unit_axis2DWorkflow :: Assertion
unit_axis2DWorkflow = do
  buffered <- newBufferedInput
  let actionMap =
        compileActions
          [ Move
              ~> [ DPad
                    (Key ScancodeA)
                    (Key ScancodeW)
                    (Key ScancodeS)
                    (Key ScancodeD)
                 ]
          ]

  -- Press W (up) and D (right)
  MPA.write buffered.thisInput.kbScancodes ScancodeW True
  MPA.write buffered.thisInput.kbScancodes ScancodeD True

  movement <- absoluteInput buffered actionMap Move
  movement @?= V2 1.0 1.0

unit_axis2DWorkflowOpposingInputs :: Assertion
unit_axis2DWorkflowOpposingInputs = do
  buffered <- newBufferedInput
  let actionMap =
        compileActions
          [ Move
              ~> [ DPad
                    (Key ScancodeA)
                    (Key ScancodeW)
                    (Key ScancodeS)
                    (Key ScancodeD)
                 ]
          ]

  -- Press W (up) and S (down) - should cancel out
  MPA.write buffered.thisInput.kbScancodes ScancodeW True
  MPA.write buffered.thisInput.kbScancodes ScancodeS True

  movement <- absoluteInput buffered actionMap Move
  movement @?= V2 0.0 0.0

unit_axis1DMouseWorkflow :: Assertion
unit_axis1DMouseWorkflow = do
  buffered <- newBufferedInput
  let actionMap = compileActions [Look ~> [MouseAxis1D MouseX 1.0]]

  -- Set mouse X movement
  MPA.write buffered.thisInput.mouseAxes MouseX 10.0

  lookValue <- absoluteInput buffered actionMap Look
  lookValue @?= 10.0

unit_prepareBufferedInputWorkflow :: Assertion
unit_prepareBufferedInputWorkflow = do
  buffered <- newBufferedInput
  let actionMap = compileActions [Jump ~> [Key ScancodeSpace]]

  -- Press space in first frame
  MPA.write buffered.thisInput.kbScancodes ScancodeSpace True

  state1 <- deltaInput buffered actionMap Jump
  state1 @?= JustPressed

  -- Prepare for next frame
  prepareBufferedInput buffered

  -- Still pressed in second frame
  MPA.write buffered.thisInput.kbScancodes ScancodeSpace True

  state2 <- deltaInput buffered actionMap Jump
  state2 @?= Held
