{-# LANGUAGE AllowAmbiguousTypes #-}

module Heph.Input.Internal.BoundedArray.IndexSpec where

import Heph.Input.Internal.BoundedArray.Index
import Heph.Input.Types.Mouse (MouseButton (..))
import Heph.Input.Types.Scancode (Scancode (..))

import Control.Monad (when)
import Hedgehog as HH
import Hedgehog.Gen qualified as Gen
import Test.Tasty.HUnit

-- Unit tests for specific cases
unit_sizeMouseButton :: Assertion
unit_sizeMouseButton = size @MouseButton @?= 3

unit_offsetMouseButton :: Assertion
unit_offsetMouseButton = offset @MouseButton @?= fromEnum MouseButtonLeft

unit_toIndexMouseButton :: Assertion
unit_toIndexMouseButton = do
  toIndex MouseButtonLeft @?= 0
  toIndex MouseButtonMiddle @?= 1
  toIndex MouseButtonRight @?= 2

-- Property tests for generic laws
hprop_sizeMatchesBounds :: Property
hprop_sizeMatchesBounds = property $ do
  size @Scancode === fromEnum (maxBound @Scancode) - fromEnum (minBound @Scancode) + 1

hprop_toIndexInBounds :: Property
hprop_toIndexInBounds = property $ do
  sc <- forAll genScancode
  let idx = toIndex sc
  HH.assert $ idx >= 0
  HH.assert $ idx < size @Scancode

hprop_toIndexUnique :: Property
hprop_toIndexUnique = property $ do
  sc1 <- forAll genScancode
  sc2 <- forAll genScancode
  when (sc1 /= sc2) $ do
    toIndex sc1 /== toIndex sc2

-- Generators
genScancode :: Gen Scancode
genScancode = Gen.enumBounded

genMouseButton :: Gen MouseButton
genMouseButton = Gen.enumBounded
