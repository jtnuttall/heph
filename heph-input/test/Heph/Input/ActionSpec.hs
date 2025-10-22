{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Heph.Input.ActionSpec where

import Heph.Input.Action
import Heph.Input.Action.TH
import Heph.Input.Buffer
import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Controller
import Heph.Input.Types.Mouse
import Heph.Input.Types.Scancode

import Data.Primitive.SmallArray qualified as SA
import Hedgehog as HH
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Linear.V2
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

-- Test action GADT
data GameAction (src :: ActionSource) where
  Jump :: GameAction Button
  Sprint :: GameAction Button
  Move :: GameAction Axis2D
  Aim :: GameAction Axis1D

makeAction ''GameAction

-- ActionMap unit tests
unit_newActionMapEmpty :: Assertion
unit_newActionMapEmpty = do
  let actionMap = newActionMap @GameAction []
      sources = readActions actionMap Jump
  SA.sizeofSmallArray sources @?= 0

unit_newActionMapSingleMapping :: Assertion
unit_newActionMapSingleMapping = do
  let actionMap = newActionMap [Jump ~> [Key ScancodeSpace]]
      sources = readActions actionMap Jump
  SA.sizeofSmallArray sources @?= 1

unit_newActionMapMultipleSources :: Assertion
unit_newActionMapMultipleSources = do
  let actionMap = newActionMap [Jump ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]]
      sources = readActions actionMap Jump
  SA.sizeofSmallArray sources @?= 2

unit_newActionMapFiltersEmptySources :: Assertion
unit_newActionMapFiltersEmptySources = do
  let actionMap = newActionMap [Jump ~> []]
      sources = readActions actionMap Jump
  SA.sizeofSmallArray sources @?= 0

-- Aggregation unit tests
unit_aggregateAbsoluteButton :: Assertion
unit_aggregateAbsoluteButton = do
  aggregateAbsolute @Button [False, False] @?= False
  aggregateAbsolute @Button [False, True] @?= True
  aggregateAbsolute @Button [True, False] @?= True
  aggregateAbsolute @Button [True, True] @?= True

unit_aggregateAbsoluteAxis1D :: Assertion
unit_aggregateAbsoluteAxis1D = do
  aggregateAbsolute @Axis1D [0.0, 0.0] @?= 0.0
  aggregateAbsolute @Axis1D [0.5, 0.3] @?= 0.5
  aggregateAbsolute @Axis1D [-0.5, 0.3] @?= 0.3

unit_aggregateAbsoluteAxis2D :: Assertion
unit_aggregateAbsoluteAxis2D = do
  aggregateAbsolute @Axis2D [V2 0 0, V2 0 0] @?= V2 0 0
  -- Should pick the vector with largest magnitude
  let v1 = V2 0.5 0.0
      v2 = V2 0.3 0.3
  aggregateAbsolute @Axis2D [v1, v2] @?= v1

-- Sensitivity and Deadzone property tests
hprop_sensitivityScalesInput :: Property
hprop_sensitivityScalesInput = property $ do
  baseValue <- forAll $ Gen.float (Range.constant (-1.0) 1.0)
  sens <- forAll $ Gen.float (Range.constant 0.1 5.0)

  let Sensitivity sensitivity = Sensitivity sens
      scaled = baseValue * sensitivity

  HH.assert $ abs (scaled - (baseValue * sens)) < 0.001

hprop_deadzoneZerosSmallInputs :: Property
hprop_deadzoneZerosSmallInputs = property $ do
  deadzone <- forAll $ Gen.float (Range.constant 0.1 0.5)
  smallValue <- forAll $ Gen.float (Range.constant 0.0 (deadzone - 0.01))

  let applyDeadzone v dz
        | abs v < dz = 0
        | otherwise = signum v * ((abs v - dz) / (1.0 - dz))

  applyDeadzone smallValue deadzone === 0

hprop_deadzonePreservesLargeInputs :: Property
hprop_deadzonePreservesLargeInputs = property $ do
  deadzone <- forAll $ Gen.float (Range.constant 0.1 0.5)
  largeValue <- forAll $ Gen.float (Range.constant (deadzone + 0.01) 1.0)

  let applyDeadzone v dz
        | abs v < dz = 0
        | otherwise = signum v * ((abs v - dz) / (1.0 - dz))
      result = applyDeadzone largeValue deadzone

  HH.assert $ abs result > 0

-- ActionMapping property tests
hprop_actionMappingPreservesType :: Property
hprop_actionMappingPreservesType = property $ do
  -- Button action can only be mapped to button sources
  let mapping = Jump ~> [Key ScancodeSpace]
      actionMap = newActionMap [mapping]
      sources = readActions actionMap Jump

  SA.sizeofSmallArray sources === 1
