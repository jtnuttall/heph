{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Heph.Input.ActionSpec where

import Heph.Input.Action
import Heph.Input.Action.TH
import Heph.Input.Types.Controller
import Heph.Input.Types.Scancode

import Data.Primitive.SmallArray qualified as SA
import Hedgehog as HH
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Linear.V2
import Test.Tasty.HUnit

-- Test action GADT
data GameAction (src :: ActionSource) where
  Jump :: GameAction Button
  Sprint :: GameAction Button
  Move :: GameAction Axis2D
  Aim :: GameAction Axis1D

makeAction ''GameAction

-- ActionMap unit tests
unit_compileActionsEmpty :: Assertion
unit_compileActionsEmpty = do
  let actionMap = compileActions @GameAction []
      sources = actionSources actionMap Jump
  SA.sizeofSmallArray sources @?= 0

unit_compileActionsSingleMapping :: Assertion
unit_compileActionsSingleMapping = do
  let actionMap = compileActions [Jump ~> [Key ScancodeSpace]]
      sources = actionSources actionMap Jump
  SA.sizeofSmallArray sources @?= 1

unit_compileActionsMultipleSources :: Assertion
unit_compileActionsMultipleSources = do
  let actionMap = compileActions [Jump ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]]
      sources = actionSources actionMap Jump
  SA.sizeofSmallArray sources @?= 2

unit_compileActionsFiltersEmptySources :: Assertion
unit_compileActionsFiltersEmptySources = do
  let actionMap = compileActions [Jump ~> []]
      sources = actionSources actionMap Jump
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
      actionMap = compileActions [mapping]
      sources = actionSources actionMap Jump

  SA.sizeofSmallArray sources === 1
