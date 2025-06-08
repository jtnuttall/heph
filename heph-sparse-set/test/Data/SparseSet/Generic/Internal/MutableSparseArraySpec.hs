module Data.SparseSet.Generic.Internal.MutableSparseArraySpec where

import Control.Monad (foldM)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty.HUnit

import Data.SparseSet.Generic.Mutable.Internal.MutableSparseArray qualified as MSA
import Data.Traversable

-- HUnit Tests for specific scenarios
unit_new_is_empty :: Assertion
unit_new_is_empty = do
  arr <- MSA.new
  mVal <- MSA.lookup arr 0
  c <- MSA.contains arr 0
  mVal @?= Nothing
  c @?= False

unit_insert_and_lookup :: Assertion
unit_insert_and_lookup = do
  arr <- MSA.new
  arr' <- MSA.unsafeInsert arr 10 100
  mVal <- MSA.lookup arr' 10
  mVal @?= Just 100

unit_insert_grows_and_fills :: Assertion
unit_insert_grows_and_fills = do
  arr <- MSA.new
  arr' <- MSA.unsafeInsert arr 10 100
  -- The array should have grown, but indices other than 10 should be empty
  mVal0 <- MSA.lookup arr' 0
  mVal5 <- MSA.lookup arr' 5
  mVal0 @?= Nothing
  mVal5 @?= Nothing

unit_delete_removes_entry :: Assertion
unit_delete_removes_entry = do
  arr <- MSA.new
  arr' <- MSA.unsafeInsert arr 10 100
  deleted <- MSA.delete arr' 10
  mVal <- MSA.lookup arr' 10
  deleted @?= Just 100
  mVal @?= Nothing

unit_clear :: Assertion
unit_clear = do
  arr <- MSA.new
  arr1 <- MSA.unsafeInsert arr 5 50
  arr2 <- MSA.unsafeInsert arr1 10 100
  MSA.clear arr2
  mVal5 <- MSA.lookup arr2 5
  mVal10 <- MSA.lookup arr2 10
  mVal5 @?= Nothing
  mVal10 @?= Nothing

-- Hedgehog property-based tests
data ArrayOp
  = OpInsert Int Int -- key, value
  | OpDelete Int -- key
  | OpClear
  deriving (Show, Eq)

genKey :: Gen Int
genKey = Gen.int (Range.linear 0 100)

genValue :: Gen Int
genValue = Gen.int (Range.linear 0 1000)

genOp :: Gen ArrayOp
genOp =
  Gen.frequency
    [ (10, OpInsert <$> genKey <*> genValue)
    , (5, OpDelete <$> genKey)
    , (1, pure OpClear)
    ]

genOps :: Gen [ArrayOp]
genOps = Gen.list (Range.linear 0 100) genOp

-- Model-based testing
applyOpToModel :: ArrayOp -> Map Int Int -> Map Int Int
applyOpToModel (OpInsert k v) = Map.insert k v
applyOpToModel (OpDelete k) = Map.delete k
applyOpToModel OpClear = const Map.empty

applyOpsToModel :: [ArrayOp] -> Map Int Int -> Map Int Int
applyOpsToModel ops model = foldl (flip applyOpToModel) model ops

getAllKeys :: [ArrayOp] -> [Int]
getAllKeys = nub . foldr opKeys []
 where
  opKeys (OpInsert k _) acc = k : acc
  opKeys (OpDelete k) acc = k : acc
  opKeys OpClear acc = acc

hprop_msa_model :: Property
hprop_msa_model = property $ do
  ops <- forAll genOps
  let allKeys = 0 : getAllKeys ops
  let finalModel = applyOpsToModel ops Map.empty
  arr <- MSA.new
  arr' <-
    foldM
      ( \a op -> case op of
          OpInsert k v -> MSA.unsafeInsert a k v
          OpDelete k -> MSA.delete a k >> pure a
          OpClear -> MSA.clear a >> pure a
      )
      arr
      ops
  sutFinalState <- for allKeys $ \k -> (k,) <$> MSA.lookup arr' k

  let modelFinalState = fmap (\k -> (k, Map.lookup k finalModel)) allKeys
  sutFinalState === modelFinalState
