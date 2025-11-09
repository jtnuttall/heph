module Heph.Input.Types.ScancodeSpec where

import Heph.Input.Types.Scancode

import Data.List (nub)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty.HUnit

-- Unit tests
unit_minBoundIsA :: Assertion
unit_minBoundIsA = minBound @Scancode @?= ScancodeA

unit_maxBoundIsNumPeriod :: Assertion
unit_maxBoundIsNumPeriod = maxBound @Scancode @?= ScancodeNumPeriod

unit_allScancodesDefined :: Assertion
unit_allScancodesDefined = do
  let count = fromEnum (maxBound @Scancode) - fromEnum (minBound @Scancode) + 1
  count @?= 103

-- Property tests - Enum laws
hprop_enumRoundtrip :: Property
hprop_enumRoundtrip = property $ do
  sc <- forAll genScancode
  toEnum (fromEnum sc) === sc

hprop_fromEnumToEnumRoundtrip :: Property
hprop_fromEnumToEnumRoundtrip = property $ do
  let minVal = fromEnum (minBound @Scancode)
      maxVal = fromEnum (maxBound @Scancode)
  i <- forAll $ Gen.int (Range.constant minVal maxVal)
  fromEnum (toEnum i :: Scancode) === i

hprop_succPredRoundtrip :: Property
hprop_succPredRoundtrip = property $ do
  sc <- forAll $ Gen.filter (/= maxBound) genScancode
  pred (succ sc) === sc

hprop_predSuccRoundtrip :: Property
hprop_predSuccRoundtrip = property $ do
  sc <- forAll $ Gen.filter (/= minBound) genScancode
  succ (pred sc) === sc

hprop_allValuesUnique :: Property
hprop_allValuesUnique = property $ do
  let allScancodes = [minBound .. maxBound] :: [Scancode]
      allEnums = map fromEnum allScancodes
  length (nub allEnums) === length allEnums

hprop_ordConsistentWithEnum :: Property
hprop_ordConsistentWithEnum = property $ do
  sc1 <- forAll genScancode
  sc2 <- forAll genScancode
  compare sc1 sc2 === compare (fromEnum sc1) (fromEnum sc2)

-- Generators
genScancode :: Gen Scancode
genScancode = Gen.enumBounded
