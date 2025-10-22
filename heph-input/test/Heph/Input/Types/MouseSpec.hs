module Heph.Input.Types.MouseSpec where

import Heph.Input.Types.Mouse

import Data.List (nub)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

-- MouseButton unit tests
unit_mouseButtonCount :: Assertion
unit_mouseButtonCount = do
  let count = fromEnum (maxBound @MouseButton) - fromEnum (minBound @MouseButton) + 1
  count @?= 3

unit_mouseButtonBounds :: Assertion
unit_mouseButtonBounds = do
  minBound @MouseButton @?= MouseButtonLeft
  maxBound @MouseButton @?= MouseButtonRight

-- MouseAxis unit tests
unit_mouseAxisCount :: Assertion
unit_mouseAxisCount = do
  let count = fromEnum (maxBound @MouseAxis) - fromEnum (minBound @MouseAxis) + 1
  count @?= 2

unit_mouseAxisBounds :: Assertion
unit_mouseAxisBounds = do
  minBound @MouseAxis @?= MouseX
  maxBound @MouseAxis @?= MouseY

-- MouseButton property tests
hprop_mouseButtonEnumRoundtrip :: Property
hprop_mouseButtonEnumRoundtrip = property $ do
  mb <- forAll genMouseButton
  toEnum (fromEnum mb) === mb

hprop_mouseButtonAllValuesUnique :: Property
hprop_mouseButtonAllValuesUnique = property $ do
  let allButtons = [minBound .. maxBound] :: [MouseButton]
      allEnums = map fromEnum allButtons
  length (nub allEnums) === length allEnums

hprop_mouseButtonOrdConsistentWithEnum :: Property
hprop_mouseButtonOrdConsistentWithEnum = property $ do
  mb1 <- forAll genMouseButton
  mb2 <- forAll genMouseButton
  compare mb1 mb2 === compare (fromEnum mb1) (fromEnum mb2)

-- MouseAxis property tests
hprop_mouseAxisEnumRoundtrip :: Property
hprop_mouseAxisEnumRoundtrip = property $ do
  ma <- forAll genMouseAxis
  toEnum (fromEnum ma) === ma

hprop_mouseAxisAllValuesUnique :: Property
hprop_mouseAxisAllValuesUnique = property $ do
  let allAxes = [minBound .. maxBound] :: [MouseAxis]
      allEnums = map fromEnum allAxes
  length (nub allEnums) === length allEnums

hprop_mouseAxisOrdConsistentWithEnum :: Property
hprop_mouseAxisOrdConsistentWithEnum = property $ do
  ma1 <- forAll genMouseAxis
  ma2 <- forAll genMouseAxis
  compare ma1 ma2 === compare (fromEnum ma1) (fromEnum ma2)

-- Generators
genMouseButton :: Gen MouseButton
genMouseButton = Gen.enumBounded

genMouseAxis :: Gen MouseAxis
genMouseAxis = Gen.enumBounded
