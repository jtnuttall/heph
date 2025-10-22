module Heph.Input.Types.ControllerSpec where

import Heph.Input.Types.Controller

import Data.List (nub)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

-- ControllerButton unit tests
unit_controllerButtonCount :: Assertion
unit_controllerButtonCount = do
  let count = fromEnum (maxBound @ControllerButton) - fromEnum (minBound @ControllerButton) + 1
  count @?= 15

unit_controllerButtonBounds :: Assertion
unit_controllerButtonBounds = do
  minBound @ControllerButton @?= ControllerButtonA
  maxBound @ControllerButton @?= ControllerButtonDpadRight

-- ControllerAxis unit tests
unit_controllerAxisCount :: Assertion
unit_controllerAxisCount = do
  let count = fromEnum (maxBound @ControllerAxis) - fromEnum (minBound @ControllerAxis) + 1
  count @?= 6

unit_controllerAxisBounds :: Assertion
unit_controllerAxisBounds = do
  minBound @ControllerAxis @?= ControllerAxisLeftX
  maxBound @ControllerAxis @?= ControllerAxisTriggerRight

-- ControllerButton property tests
hprop_controllerButtonEnumRoundtrip :: Property
hprop_controllerButtonEnumRoundtrip = property $ do
  cb <- forAll genControllerButton
  toEnum (fromEnum cb) === cb

hprop_controllerButtonAllValuesUnique :: Property
hprop_controllerButtonAllValuesUnique = property $ do
  let allButtons = [minBound .. maxBound] :: [ControllerButton]
      allEnums = map fromEnum allButtons
  length (nub allEnums) === length allEnums

hprop_controllerButtonOrdConsistentWithEnum :: Property
hprop_controllerButtonOrdConsistentWithEnum = property $ do
  cb1 <- forAll genControllerButton
  cb2 <- forAll genControllerButton
  compare cb1 cb2 === compare (fromEnum cb1) (fromEnum cb2)

-- ControllerAxis property tests
hprop_controllerAxisEnumRoundtrip :: Property
hprop_controllerAxisEnumRoundtrip = property $ do
  ca <- forAll genControllerAxis
  toEnum (fromEnum ca) === ca

hprop_controllerAxisAllValuesUnique :: Property
hprop_controllerAxisAllValuesUnique = property $ do
  let allAxes = [minBound .. maxBound] :: [ControllerAxis]
      allEnums = map fromEnum allAxes
  length (nub allEnums) === length allEnums

hprop_controllerAxisOrdConsistentWithEnum :: Property
hprop_controllerAxisOrdConsistentWithEnum = property $ do
  ca1 <- forAll genControllerAxis
  ca2 <- forAll genControllerAxis
  compare ca1 ca2 === compare (fromEnum ca1) (fromEnum ca2)

-- Generators
genControllerButton :: Gen ControllerButton
genControllerButton = Gen.enumBounded

genControllerAxis :: Gen ControllerAxis
genControllerAxis = Gen.enumBounded
