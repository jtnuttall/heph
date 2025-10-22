module Heph.Input.Internal.BoundedArray.Primitive.MutableSpec where

import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Heph.Input.Types.Mouse (MouseButton (..))
import Heph.Input.Types.Scancode (Scancode (..))

import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

-- Unit tests
unit_newArrayHasDefaultValue :: Assertion
unit_newArrayHasDefaultValue = do
  arr <- MPA.new @Scancode False
  val <- MPA.read arr ScancodeA
  val @?= False

unit_writeAndRead :: Assertion
unit_writeAndRead = do
  arr <- MPA.new @Scancode False
  MPA.write arr ScancodeW True
  val <- MPA.read arr ScancodeW
  val @?= True

unit_copyArray :: Assertion
unit_copyArray = do
  src <- MPA.new @MouseButton False
  dest <- MPA.new @MouseButton True

  MPA.write src MouseButtonLeft True
  MPA.write src MouseButtonRight True

  MPA.copy dest src

  left <- MPA.read dest MouseButtonLeft
  middle <- MPA.read dest MouseButtonMiddle
  right <- MPA.read dest MouseButtonRight

  left @?= True
  middle @?= False
  right @?= True

-- Property tests
hprop_readWriteRoundtrip :: Property
hprop_readWriteRoundtrip = property $ do
  sc <- forAll genScancode
  val <- forAll Gen.bool

  arr <- MPA.new @Scancode False
  MPA.write arr sc val
  result <- MPA.read arr sc

  result === val

hprop_independentWrites :: Property
hprop_independentWrites = property $ do
  sc1 <- forAll genScancode
  sc2 <- forAll $ Gen.filter (/= sc1) genScancode

  arr <- MPA.new @Scancode False
  MPA.write arr sc1 True
  MPA.write arr sc2 False

  val1 <- MPA.read arr sc1
  val2 <- MPA.read arr sc2

  val1 === True
  val2 === False

hprop_copyPreservesValues :: Property
hprop_copyPreservesValues = property $ do
  allValues <- forAll $ Gen.list (Range.constant 0 10) ((,) <$> genScancode <*> Gen.bool)
  -- Map.fromList keeps the last value for duplicate keys
  let finalState = Map.fromList allValues
      values = Map.toList finalState

  src <- MPA.new @Scancode False
  dest <- MPA.new @Scancode True

  mapM_ (\(sc, val) -> MPA.write src sc val) values
  MPA.copy dest src

  for_ values $ \(sc, expectedVal) -> do
    result <- MPA.read dest sc
    result === expectedVal

-- Generators
genScancode :: Gen Scancode
genScancode = Gen.enumBounded

genMouseButton :: Gen MouseButton
genMouseButton = Gen.enumBounded
