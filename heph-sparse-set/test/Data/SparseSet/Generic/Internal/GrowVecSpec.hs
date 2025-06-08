{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant do" #-}

module Data.SparseSet.Generic.Internal.GrowVecSpec where

import Control.Monad (foldM, forM)
import Data.Vector.Unboxed.Mutable qualified as VM
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty.HUnit

import Data.SparseSet.Generic.Mutable.Internal.GrowVec qualified as GV

type TestComponent = Int

unit_withCapacity_is_empty :: Assertion
unit_withCapacity_is_empty = do
  vec <- GV.withCapacity @TestComponent @VM.MVector 128
  GV.length vec @?= 0

unit_withCapacity_valid_capacity :: Assertion
unit_withCapacity_valid_capacity = do
  vec <- GV.withCapacity @TestComponent @VM.MVector 0
  vec' <- GV.withCapacity @TestComponent @VM.MVector 1
  assertBool "capacity must be greater than 2" (GV.capacity vec >= 2 && GV.capacity vec' >= 2)

-- HUnit Tests for specific scenarios
unit_new_is_empty :: Assertion
unit_new_is_empty = do
  vec <- GV.new @TestComponent @VM.MVector
  GV.length vec @?= 0

unit_snoc_increases_length :: Assertion
unit_snoc_increases_length = do
  vec <- GV.new @TestComponent @VM.MVector
  vec' <- GV.snoc vec 100
  GV.length vec' @?= 1

unit_read_after_snoc :: Assertion
unit_read_after_snoc = do
  vec <- GV.new @TestComponent @VM.MVector
  vec' <- GV.snoc vec 100
  mVal <- GV.readMaybe vec' 0
  mVal @?= Just 100

unit_swapRemove_middle :: Assertion
unit_swapRemove_middle = do
  vec <- GV.new @TestComponent @VM.MVector
  vec1 <- GV.snoc vec 10
  vec2 <- GV.snoc vec1 20
  vec3 <- GV.snoc vec2 30 -- vec = [10, 20, 30]
  (removed, vec4) <- GV.unsafeSwapRemove vec3 1 -- remove 20
  -- now vec should be [10, 30]
  val0 <- GV.unsafeRead vec4 0
  val1 <- GV.unsafeRead vec4 1
  mVal2 <- GV.readMaybe vec4 2
  removed @?= 20
  GV.length vec4 @?= 2
  val0 @?= 10
  val1 @?= 30
  mVal2 @?= Nothing

unit_compact_preserves_elements :: Assertion
unit_compact_preserves_elements = do
  vec1 <- GV.withCapacity @TestComponent @VM.MVector 100
  vec2 <- GV.snoc vec1 10
  vec3 <- GV.snoc vec2 20
  vec4 <- GV.compact vec3
  GV.length vec4 @?= 2
  val0 <- GV.unsafeRead vec4 0
  val1 <- GV.unsafeRead vec4 1
  val0 @?= 10
  val1 @?= 20

unit_maximum_empty :: Assertion
unit_maximum_empty = do
  vec <- GV.new @TestComponent @VM.MVector
  mMax <- GV.maximum vec
  mMax @?= Nothing

unit_maximum_single :: Assertion
unit_maximum_single = do
  vec <- GV.new @TestComponent @VM.MVector
  vec' <- GV.snoc vec 42
  mMax <- GV.maximum vec'
  mMax @?= Just 42

unit_maximum_multiple :: Assertion
unit_maximum_multiple = do
  vec <- GV.new @TestComponent @VM.MVector
  vec1 <- GV.snoc vec (-10)
  vec2 <- GV.snoc vec1 30
  vec3 <- GV.snoc vec2 5
  mMax <- GV.maximum vec3
  mMax @?= Just 30

unit_clear :: Assertion
unit_clear = do
  vec <- GV.new @TestComponent @VM.MVector
  vec1 <- GV.snoc vec 10
  vec2 <- GV.snoc vec1 20
  let vec3 = GV.cleared vec2
  GV.length vec2 @?= 2 -- Original is unchanged
  GV.length vec3 @?= 0 -- New is empty

-- Hedgehog property-based tests
data GrowVecOp
  = OpSnoc TestComponent
  | OpWrite Int TestComponent
  | OpSwapRemove Int
  | OpClear
  deriving (Show, Eq)

genOp :: Int -> Gen GrowVecOp
genOp currentLen =
  Gen.frequency $
    [ (10, OpSnoc <$> Gen.int (Range.linear 0 1000))
    ]
      <> [ (5, OpWrite <$> Gen.int (Range.linear 0 (currentLen - 1)) <*> Gen.int (Range.linear 0 1000))
         | currentLen > 0
         ]
      <> [ (3, OpSwapRemove <$> Gen.int (Range.linear 0 (currentLen - 1))) | currentLen > 0
         ]
      <> [(1, pure OpClear) | currentLen > 0]

genOps :: Gen [GrowVecOp]
genOps = do
  size <- Gen.int (Range.linear 0 100)
  go size []
 where
  go :: Int -> [GrowVecOp] -> Gen [GrowVecOp]
  go n acc | n <= 0 = pure (reverse acc)
  go n acc = do
    let currentModel = applyOpsToModel (reverse acc) []
    op <- genOp (length currentModel)
    go (n - 1) (op : acc)

-- Pure list model of the GrowVec operations
applyOpToModel :: GrowVecOp -> [TestComponent] -> [TestComponent]
applyOpToModel (OpSnoc c) xs = xs ++ [c]
applyOpToModel (OpWrite idx val) xs = take idx xs ++ [val] ++ drop (idx + 1) xs
applyOpToModel (OpSwapRemove idx) xs
  | idx == lastIdx = take idx xs
  | otherwise = take idx xs ++ [last xs] ++ drop (idx + 1) (take lastIdx xs)
 where
  lastIdx = length xs - 1
applyOpToModel OpClear _ = []

applyOpsToModel :: [GrowVecOp] -> [TestComponent] -> [TestComponent]
applyOpsToModel ops model = foldl (flip applyOpToModel) model ops

hprop_growvec_model :: Property
hprop_growvec_model = property $ do
  ops <- forAll genOps
  let finalModel = applyOpsToModel ops []
  vec <- GV.new @TestComponent @VM.MVector
  vec' <-
    foldM
      ( \v op -> case op of
          OpSnoc c -> GV.snoc v c
          OpWrite idx val -> GV.unsafeWrite v idx val >> pure v
          OpSwapRemove idx -> snd <$> GV.unsafeSwapRemove v idx
          OpClear -> pure $ GV.cleared v
      )
      vec
      ops

  let len = GV.length vec'
  contents <- forM [0 .. len - 1] (GV.unsafeRead vec')

  len === length finalModel
  contents === finalModel
