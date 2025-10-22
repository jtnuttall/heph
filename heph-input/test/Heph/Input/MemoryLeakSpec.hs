{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Heph.Input.MemoryLeakSpec where

import Heph.Input.Action
import Heph.Input.Action.TH
import Heph.Input.Buffer
import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Data.Typeable (Typeable)
import Linear.V2 (V2 (..))
import NoThunks.Class
import Test.Tasty.HUnit

-- Test action for memory leak tests
data TestAction (src :: ActionSource) where
  TestButton :: TestAction Button
  TestButton2 :: TestAction Button
  TestAxis2D :: TestAction Axis2D

makeAction ''TestAction

-- Deriving NoThunks instances via InspectHeap for mutable structures
deriving via
  InspectHeap BufferedInput
  instance
    NoThunks BufferedInput

deriving via
  InspectHeap InputBuffer
  instance
    NoThunks InputBuffer

deriving via
  InspectHeap (ActionMap act)
  instance
    (Typeable act)
    => NoThunks (ActionMap act)

deriving via
  InspectHeap ButtonState
  instance
    NoThunks ButtonState

-- Helper function for cleaner assertions
assertNoThunks :: (NoThunks a) => String -> a -> IO ()
assertNoThunks lbl a = case unsafeNoThunks a of
  Just ti -> assertFailure $ "[" <> lbl <> "] Found unexpected thunks: " <> show ti
  Nothing -> pure ()
{-# INLINE assertNoThunks #-}

-- Test that creating BufferedInput doesn't introduce thunks
unit_newBufferedInput_noThunks :: Assertion
unit_newBufferedInput_noThunks = do
  buffered <- newBufferedInput
  assertNoThunks "new buffered input" buffered

-- Test that writing to buffer doesn't introduce thunks
unit_writeToBuffer_noThunks :: Assertion
unit_writeToBuffer_noThunks = do
  buffered <- newBufferedInput
  MPA.write buffered.thisInput.kbScancodes ScancodeA True
  assertNoThunks "after write" buffered

-- Test that multiple writes don't introduce thunks
unit_multipleWrites_noThunks :: Assertion
unit_multipleWrites_noThunks = do
  buffered <- newBufferedInput
  assertNoThunks "empty" buffered

  MPA.write buffered.thisInput.kbScancodes ScancodeA True
  assertNoThunks "1 write" buffered

  MPA.write buffered.thisInput.kbScancodes ScancodeW True
  assertNoThunks "2 writes" buffered

  MPA.write buffered.thisInput.mouseButtons MouseButtonLeft True
  assertNoThunks "3 writes" buffered

  MPA.write buffered.thisInput.controllerButtons ControllerButtonA True
  assertNoThunks "4 writes" buffered

-- Test that prepareBufferedInput doesn't introduce thunks
unit_prepareBufferedInput_noThunks :: Assertion
unit_prepareBufferedInput_noThunks = do
  buffered <- newBufferedInput

  MPA.write buffered.thisInput.kbScancodes ScancodeSpace True
  MPA.write buffered.thisInput.mouseAxes MouseX 10.0

  prepareBufferedInput buffered
  assertNoThunks "after prepare" buffered

-- Test that reading doesn't introduce thunks
unit_readFromBuffer_noThunks :: Assertion
unit_readFromBuffer_noThunks = do
  buffered <- newBufferedInput
  MPA.write buffered.thisInput.kbScancodes ScancodeA True

  val <- MPA.read buffered.thisInput.kbScancodes ScancodeA
  assertNoThunks "after read" buffered
  assertNoThunks "read value" val

-- Test that ActionMap creation doesn't introduce thunks
unit_newActionMap_scancode_noThunks :: Assertion
unit_newActionMap_scancode_noThunks = do
  let !actionMap = newActionMap [TestButton ~> [Key ScancodeSpace]]
  assertNoThunks "action map" actionMap

unit_newActionMap_dpad_noThunks :: Assertion
unit_newActionMap_dpad_noThunks = do
  let !actionMap =
        newActionMap
          [ TestAxis2D
              ~> [ DPad
                    (Key ScancodeA)
                    (Key ScancodeW)
                    (Key ScancodeS)
                    (Key ScancodeD)
                 ]
          ]
  assertNoThunks "action map" actionMap

unit_newActionMap_multibutton_noThunks :: Assertion
unit_newActionMap_multibutton_noThunks = do
  let !actionMap =
        newActionMap
          [ TestButton ~> [Key ScancodeSpace]
          , TestButton2 ~> [Key ScancodeA]
          ]
  assertNoThunks "action map" actionMap

unit_newActionMap_multimap_noThunks :: Assertion
unit_newActionMap_multimap_noThunks = do
  let !actionMap = newActionMap [TestButton ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]]
  assertNoThunks "action map" actionMap

unit_newActionMap_multimap_multiinput_noThunks :: Assertion
unit_newActionMap_multimap_multiinput_noThunks = do
  let !actionMap =
        newActionMap
          [ TestButton ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]
          , TestAxis2D
              ~> [ DPad
                    (Key ScancodeA)
                    (Key ScancodeW)
                    (Key ScancodeS)
                    (Key ScancodeD)
                 ]
          ]
  assertNoThunks "action map" actionMap

-- Test that reading actions doesn't introduce thunks
unit_absoluteInput_noThunks :: Assertion
unit_absoluteInput_noThunks = do
  buffered <- newBufferedInput
  let actionMap = newActionMap [TestButton ~> [Key ScancodeSpace]]

  MPA.write buffered.thisInput.kbScancodes ScancodeSpace True

  isPressed <- absoluteInput buffered actionMap TestButton
  assertNoThunks "after absoluteInput" buffered
  isPressed @?= True

-- Test that delta input doesn't introduce thunks
unit_deltaInput_noThunks :: Assertion
unit_deltaInput_noThunks = do
  buffered <- newBufferedInput
  let actionMap = newActionMap [TestButton ~> [Key ScancodeSpace]]

  MPA.write buffered.thisInput.kbScancodes ScancodeSpace True

  state <- deltaInput buffered actionMap TestButton
  assertNoThunks "after deltaInput" buffered
  state @?= JustPressed

-- Test full workflow doesn't introduce thunks
unit_fullWorkflow_noThunks :: Assertion
unit_fullWorkflow_noThunks = do
  buffered <- newBufferedInput
  assertNoThunks "initial" buffered

  let !actionMap =
        newActionMap
          [ TestButton ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]
          , TestAxis2D
              ~> [ DPad
                    (Key ScancodeA)
                    (Key ScancodeW)
                    (Key ScancodeS)
                    (Key ScancodeD)
                 ]
          ]
  assertNoThunks "action map created" actionMap

  -- Frame 1: Press space
  MPA.write buffered.thisInput.kbScancodes ScancodeSpace True
  assertNoThunks "frame 1 write" buffered

  state1 <- deltaInput buffered actionMap TestButton
  assertNoThunks "frame 1 delta" state1

  prepareBufferedInput buffered
  assertNoThunks "frame 1 prepare" buffered

  -- Frame 2: Hold space, press W and D
  MPA.write buffered.thisInput.kbScancodes ScancodeSpace True
  MPA.write buffered.thisInput.kbScancodes ScancodeW True
  MPA.write buffered.thisInput.kbScancodes ScancodeD True
  assertNoThunks "frame 2 writes" buffered

  state2 <- deltaInput buffered actionMap TestButton
  movement <- absoluteInput buffered actionMap TestAxis2D
  assertNoThunks "frame 2 reads" buffered
  state2 @?= Held
  movement @?= V2 1.0 1.0

  prepareBufferedInput buffered
  assertNoThunks "frame 2 prepare" buffered
