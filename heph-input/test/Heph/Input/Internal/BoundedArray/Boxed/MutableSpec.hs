module Heph.Input.Internal.BoundedArray.Boxed.MutableSpec where

import Heph.Input.Internal.BoundedArray.Boxed.Mutable qualified as MBA
import Heph.Input.Types.Mouse (MouseButton (..))
import Heph.Input.Types.Scancode (Scancode (..))

import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty.HUnit

-- Unit tests
unit_newArrayHasDefaultValue :: Assertion
unit_newArrayHasDefaultValue = do
  arr <- MBA.new @Scancode (Nothing :: Maybe Int)
  val <- MBA.read arr ScancodeA
  val @?= Nothing

unit_writeAndRead :: Assertion
unit_writeAndRead = do
  arr <- MBA.new @Scancode (Nothing :: Maybe Int)
  MBA.write arr ScancodeW (Just 42)
  val <- MBA.read arr ScancodeW
  val @?= Just 42

unit_copyArray :: Assertion
unit_copyArray = do
  src <- MBA.new @MouseButton (Nothing :: Maybe String)
  dest <- MBA.new @MouseButton (Just "default")

  MBA.write src MouseButtonLeft (Just "left")
  MBA.write src MouseButtonRight (Just "right")

  MBA.copy dest src

  left <- MBA.read dest MouseButtonLeft
  middle <- MBA.read dest MouseButtonMiddle
  right <- MBA.read dest MouseButtonRight

  left @?= Just "left"
  middle @?= Nothing
  right @?= Just "right"

-- Property tests
hprop_readWriteRoundtrip :: Property
hprop_readWriteRoundtrip = property $ do
  sc <- forAll genScancode
  val <- forAll $ Gen.maybe (Gen.int (Range.constant 0 100))

  arr <- MBA.new @Scancode Nothing
  MBA.write arr sc val
  result <- MBA.read arr sc

  result === val

hprop_independentWrites :: Property
hprop_independentWrites = property $ do
  sc1 <- forAll genScancode
  sc2 <- forAll $ Gen.filter (/= sc1) genScancode

  arr <- MBA.new @Scancode (Nothing :: Maybe Int)
  MBA.write arr sc1 (Just 1)
  MBA.write arr sc2 (Just 2)

  val1 <- MBA.read arr sc1
  val2 <- MBA.read arr sc2

  val1 === Just 1
  val2 === Just 2

hprop_copyPreservesValues :: Property
hprop_copyPreservesValues = property $ do
  allValues <- forAll $ Gen.list (Range.constant 0 10) ((,) <$> genScancode <*> genMaybeInt)
  -- Map.fromList keeps the last value for duplicate keys
  let finalState = Map.fromList allValues
      values = Map.toList finalState

  src <- MBA.new @Scancode Nothing
  dest <- MBA.new @Scancode (Just 999)

  mapM_ (\(sc, val) -> MBA.write src sc val) values
  MBA.copy dest src

  for_ values $ \(sc, expectedVal) -> do
    result <- MBA.read dest sc
    result === expectedVal

-- Generators
genScancode :: Gen Scancode
genScancode = Gen.enumBounded

genMouseButton :: Gen MouseButton
genMouseButton = Gen.enumBounded

genMaybeInt :: Gen (Maybe Int)
genMaybeInt = Gen.maybe (Gen.int (Range.constant 0 100))
