{-# LANGUAGE OverloadedRecordDot #-}

module Heph.Input.BufferSpec where

import Heph.Input.Buffer
import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Data.List (nub)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

-- BufferedInput unit tests
unit_newBufferedInput :: Assertion
unit_newBufferedInput = do
  buffered <- newBufferedInput
  -- Check that we can read from the buffers without errors
  val <- MPA.read buffered.thisInput.kbScancodes ScancodeA
  val @?= False

unit_prepareBufferedInputSwapsBuffers :: Assertion
unit_prepareBufferedInputSwapsBuffers = do
  buffered <- newBufferedInput

  -- Write to thisInput
  MPA.write buffered.thisInput.kbScancodes ScancodeW True

  -- Prepare should copy thisInput to lastInput and clear mouse axes
  prepareBufferedInput buffered

  -- Check that lastInput now has the value
  lastVal <- MPA.read buffered.lastInput.kbScancodes ScancodeW
  lastVal @?= True

  -- Check that mouse axes are cleared
  mouseX <- MPA.read buffered.thisInput.mouseAxes MouseX
  mouseY <- MPA.read buffered.thisInput.mouseAxes MouseY
  mouseX @?= 0
  mouseY @?= 0

unit_prepareBufferedInputKeepsNonMouseState :: Assertion
unit_prepareBufferedInputKeepsNonMouseState = do
  buffered <- newBufferedInput

  -- Set some state
  MPA.write buffered.thisInput.kbScancodes ScancodeA True
  MPA.write buffered.thisInput.mouseButtons MouseButtonLeft True

  prepareBufferedInput buffered

  -- Non-mouse state should be preserved in lastInput
  kbVal <- MPA.read buffered.lastInput.kbScancodes ScancodeA
  mbVal <- MPA.read buffered.lastInput.mouseButtons MouseButtonLeft

  kbVal @?= True
  mbVal @?= True

-- InputButton Enum property tests
hprop_inputButtonEnumRoundtrip :: Property
hprop_inputButtonEnumRoundtrip = property $ do
  ib <- forAll genInputButton
  toEnum (fromEnum ib) === ib

hprop_inputButtonAllValuesUnique :: Property
hprop_inputButtonAllValuesUnique = property $ do
  let allButtons = [minBound .. maxBound] :: [InputButton]
      allEnums = map fromEnum allButtons
  length (nub allEnums) === length allEnums

hprop_inputButtonOrdConsistentWithEnum :: Property
hprop_inputButtonOrdConsistentWithEnum = property $ do
  ib1 <- forAll genInputButton
  ib2 <- forAll genInputButton
  compare ib1 ib2 === compare (fromEnum ib1) (fromEnum ib2)

-- Axis Enum property tests
hprop_axisEnumRoundtrip :: Property
hprop_axisEnumRoundtrip = property $ do
  axis <- forAll genAxis
  toEnum (fromEnum axis) === axis

hprop_axisAllValuesUnique :: Property
hprop_axisAllValuesUnique = property $ do
  let allAxes = [minBound .. maxBound] :: [Axis]
      allEnums = map fromEnum allAxes
  length (nub allEnums) === length allEnums

-- Generators
genInputButton :: Gen InputButton
genInputButton =
  Gen.choice
    [ ScancodeButton <$> Gen.enumBounded
    , InputMouseButton <$> Gen.enumBounded
    , ControllerButton <$> Gen.enumBounded
    ]

genAxis :: Gen Axis
genAxis =
  Gen.choice
    [ MouseAxis <$> Gen.enumBounded
    , ControllerAxis <$> Gen.enumBounded
    ]
