{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.SparseSet.Unboxed.MutableSpec where

import Control.Monad
import Control.Monad.Primitive
import Data.Foldable
import Data.IORef
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import Data.Typeable (Typeable)
import Data.Vector.Unboxed qualified as U
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import NoThunks.Class
import Test.Tasty.HUnit

import Data.SparseSet.Unboxed.Mutable (MutableSparseSet)
import Data.SparseSet.Unboxed.Mutable qualified as SS

-- Component type for most tests
type TestComponent = Int

-- Entity ID type
type TestEntity = Int

deriving via
  InspectHeap (MutableSparseSet s a)
  instance
    (Typeable a, Typeable s)
    => NoThunks (MutableSparseSet s a)

--------------------------------------------------------------------------------
-- HUnit
--------------------------------------------------------------------------------
assertNoThunks :: (NoThunks a) => String -> a -> IO ()
assertNoThunks lbl a = case unsafeNoThunks a of
  Just ti -> assertFailure $ "[" <> lbl <> "] Found unexpected thunks: " <> show ti
  Nothing -> pure ()
{-# INLINE assertNoThunks #-}

unit_new_empty_set :: Assertion
unit_new_empty_set = do
  set <- SS.new @TestComponent
  len <- SS.length set
  c0 <- SS.contains set 0
  g0 <- SS.lookup set 0
  len @?= 0
  c0 @?= False
  g0 @?= Nothing

unit_new_empty_set_no_thunks :: Assertion
unit_new_empty_set_no_thunks = do
  set <- SS.new @TestComponent
  assertNoThunks "empty set" set
  g0 <- SS.lookup set 0
  assertNoThunks "empty set - nonexisting" g0

unit_single_insert :: Assertion
unit_single_insert = do
  set <- SS.new @TestComponent
  SS.insert set 0 100
  len <- SS.length set
  c0 <- SS.contains set 0
  not_c1 <- SS.contains set 1
  g0 <- SS.lookup set 0
  len @?= 1
  c0 @?= True
  not_c1 @?= False
  g0 @?= Just 100

unit_single_insert_no_thunks :: Assertion
unit_single_insert_no_thunks = do
  set <- SS.new @TestComponent
  SS.insert set 0 100
  assertNoThunks "singleton set" set
  g0 <- SS.lookup set 0
  assertNoThunks "singleton set result" g0

unit_insert_update :: Assertion
unit_insert_update = do
  set <- SS.new @TestComponent
  SS.insert set 0 100
  SS.insert set 0 200
  len <- SS.length set
  g0 <- SS.lookup set 0
  len @?= 1
  g0 @?= Just 200

unit_insert_update_no_thunks :: Assertion
unit_insert_update_no_thunks = do
  set <- SS.new @TestComponent
  SS.insert set 0 100
  SS.insert set 0 200
  len <- SS.length set
  g0 <- SS.lookup set 0
  len @?= 1
  g0 @?= Just 200

unit_delete_existing :: Assertion
unit_delete_existing = do
  set <- SS.new @TestComponent
  SS.insert set 0 100
  SS.insert set 1 101
  deletedVal <- SS.delete set 0
  len <- SS.length set
  c0 <- SS.contains set 0
  g0 <- SS.lookup set 0
  c1 <- SS.contains set 1
  g1 <- SS.lookup set 1
  deletedVal @?= Just 100
  len @?= 1
  c0 @?= False
  g0 @?= Nothing
  c1 @?= True
  g1 @?= Just 101

unit_delete_non_existing :: Assertion
unit_delete_non_existing = do
  set <- SS.new @TestComponent
  SS.insert set 0 100
  deletedVal <- SS.delete set 1 -- Try to delete non-existing
  len <- SS.length set
  c0 <- SS.contains set 0
  g0 <- SS.lookup set 0
  deletedVal @?= Nothing
  len @?= 1
  c0 @?= True
  g0 @?= Just 100

unit_delete_last_element_no_swap :: Assertion
unit_delete_last_element_no_swap = do
  set <- SS.new @TestComponent
  SS.insert set 0 100
  SS.insert set 1 101
  deletedVal <- SS.delete set 1 -- Delete the last inserted (likely last in dense)
  len <- SS.length set
  g1 <- SS.lookup set 1
  g0 <- SS.lookup set 0
  deletedVal @?= Just 101
  len @?= 1
  g1 @?= Nothing
  g0 @?= Just 100

unit_delete_causes_swap :: Assertion
unit_delete_causes_swap = do
  set <- SS.new @TestComponent
  -- Order of insertion might matter for dense array layout if not careful,
  -- but sparse set logic should be independent of insertion order for correctness.
  -- Entities: 0, 10, 5. Values: 100, 110, 105
  -- Assume dense indices map somewhat to insertion:
  -- sparse: 0->DI0, 10->DI1, 5->DI2
  -- dense: [val_for_0, val_for_10, val_for_5]
  -- indices: [0, 10, 5]
  SS.insert set 0 100
  SS.insert set 10 110
  SS.insert set 5 105
  -- At this point, length is 3.
  -- Let's say internal dense layout is [100 (for 0), 110 (for 10), 105 (for 5)]
  -- Indices: [0, 10, 5]
  -- Sparse: 0->0, 10->1, 5->2

  -- Delete entity 0 (at dense index 0).
  -- Element for entity 5 (value 105) at dense_idx 2 should be swapped into dense_idx 0.
  deletedVal <- SS.delete set 0
  len <- SS.length set

  g0 <- SS.lookup set 0
  g10 <- SS.lookup set 10
  g5 <- SS.lookup set 5

  deletedVal @?= Just 100
  len @?= 2
  g0 @?= Nothing
  g10 @?= Just 110
  g5 @?= Just 105

unit_delete_swap_no_thunks :: Assertion
unit_delete_swap_no_thunks = do
  set <- SS.new @TestComponent
  assertNoThunks "empty" set
  SS.insert set 0 100
  assertNoThunks "1 elem" set
  SS.insert set 10 110
  assertNoThunks "2 elem" set
  SS.insert set 5 105
  assertNoThunks "3 elem" set
  deletedVal <- SS.delete set 0
  assertNoThunks "delete" set
  assertNoThunks "deleted value" deletedVal

  g0 <- SS.lookup set 0
  assertNoThunks "get 0" g0
  g10 <- SS.lookup set 10
  assertNoThunks "get 10" g10
  g5 <- SS.lookup set 5
  assertNoThunks "get 5" g5

unit_delete_then_insert :: Assertion
unit_delete_then_insert = do
  set <- SS.new @TestComponent
  SS.insert set 0 100
  SS.insert set 10 110
  SS.insert set 5 105
  deleted <- SS.delete set 0
  l0 <- SS.lookup set 0
  SS.insert set 0 200
  l1 <- SS.lookup set 0
  len <- SS.length set

  deleted @?= Just 100
  l0 @?= Nothing
  l1 @?= Just 200
  len @?= 3

unit_insert_large_index :: Assertion
unit_insert_large_index = do
  set <- SS.new @TestComponent
  SS.insert set 0 100
  SS.insert set 5000 500 -- Test sparse array growth
  len <- SS.length set
  g0 <- SS.lookup set 0
  g5000 <- SS.lookup set 5000
  c5000 <- SS.contains set 5000
  len @?= 2
  g0 @?= Just 100
  g5000 @?= Just 500
  c5000 @?= True

unit_clear :: Assertion
unit_clear = do
  set <- SS.new @TestComponent
  SS.insert set 0 100
  SS.insert set 500 32
  SS.insert set 31 (-5)
  SS.clear set
  SS.mapM_ (\_ -> assertFailure "Set must be empty") set

unit_ifoldM__empty :: Assertion
unit_ifoldM__empty = do
  set <- SS.new @TestComponent
  result <- SS.ifoldM (\acc (e, c) -> pure $ (e, c) : acc) [] set
  result @?= []

unit_mapM__sum :: Assertion
unit_mapM__sum = do
  set <- SS.new @TestComponent
  SS.insert set 10 1
  SS.insert set 20 2
  SS.insert set 30 3
  sumRef <- newIORef 0
  SS.mapM_ (\c -> modifyIORef' sumRef (+ c)) set
  finalSum <- readIORef sumRef
  finalSum @?= 6

-- This new test case verifies the ifoldIntersectionM function
-- using only the public API for Unboxed sparse sets.
unit_ifoldIntersectionM :: Assertion
unit_ifoldIntersectionM = do
  -- Setup:
  -- Set A: Unboxed Ints
  setA <- SS.new @Int
  SS.insert setA 10 100
  SS.insert setA 20 200
  SS.insert setA 30 300

  -- Set B: Unboxed Bools
  setB <- SS.new @Bool
  SS.insert setB 20 True
  SS.insert setB 30 False
  SS.insert setB 40 True

  -- Set C: An empty set
  setCEmpty <- SS.new @Int

  -- The folding function collects the results into a list.
  -- Its type signature matches the arguments from setA and setB.
  let fIntBool acc e ca cb = pure $ (e, ca, cb) : acc

  -- Test 1: Intersection of A (Ints) and B (Bools)
  result1 <- SS.ifoldIntersectionM fIntBool [] setA setB
  -- The result list is built in reverse order, so we sort for a stable comparison.
  sort result1 @?= [(20, 200, True), (30, 300, False)]

  -- Test 2: Intersection of B (Bools) and A (Ints)
  -- For this, the folding function's components must be in the opposite order.
  let fBoolInt acc e cb ca = pure $ (e, ca, cb) : acc
  result2 <- SS.ifoldIntersectionM fBoolInt [] setB setA
  -- The final tuple structure is the same, so the result should be identical.
  sort result2 @?= [(20, 200, True), (30, 300, False)]

  -- Test 3: Intersection with an empty set
  resultEmpty <- SS.ifoldIntersectionM fIntBool [] setA setCEmpty
  resultEmpty @?= []

--------------------------------------------------------------------------------
-- Hedgehog Property-Based Tests
--------------------------------------------------------------------------------

-- Generators
genEntityId :: Gen TestEntity
genEntityId = Gen.int (Range.linear 0 200) -- Range can be adjusted for different test profiles

genSmallEntityId :: Gen TestEntity
genSmallEntityId = Gen.int (Range.linear 0 10)

genComponent :: Gen TestComponent
genComponent = Gen.int (Range.linear (-1000) 1000)

-- Operations for stateful model testing
data SparseSetOp
  = OpInsert TestEntity TestComponent
  | OpDelete TestEntity
  deriving (Show, Eq)

genSparseSetOp :: Gen SparseSetOp
genSparseSetOp =
  Gen.frequency
    [ (7, OpInsert <$> genEntityId <*> genComponent) -- More inserts initially
    , (3, OpDelete <$> genEntityId)
    ]

genSparseSetOpSmallEntities :: Gen SparseSetOp
genSparseSetOpSmallEntities =
  Gen.frequency
    [ (7, OpInsert <$> genSmallEntityId <*> genComponent)
    , (3, OpDelete <$> genSmallEntityId)
    ]

-- Apply a list of operations to a pure model
applyOpsToModel :: [SparseSetOp] -> Map TestEntity TestComponent -> Map TestEntity TestComponent
applyOpsToModel ops model = foldl applyOpToModel model ops
 where
  applyOpToModel m (OpInsert e c) = Map.insert e c m
  applyOpToModel m (OpDelete e) = Map.delete e m

-- Apply a list of operations to the MutableSparseSet
applyOpsToSet
  :: (Foldable t, PrimMonad f) => MutableSparseSet (PrimState f) TestComponent -> t SparseSetOp -> f ()
applyOpsToSet set ops = for_ ops \case
  OpInsert e c -> SS.insert set e c
  OpDelete e -> void $ SS.delete set e -- Ignore deleted value for this helper

extractOpEntities :: SparseSetOp -> [TestEntity]
extractOpEntities = \case
  OpInsert e _ -> [e]
  OpDelete e -> [e]

-- Extract all current (entity, component) pairs and length from the set
extractFullStateFromSet
  :: (PrimMonad m, U.Unbox a) => MutableSparseSet (PrimState m) a -> Set Int -> m (Int, Map Int a)
extractFullStateFromSet set allKnownEntities = do
  len <- SS.length set
  mapEntries <-
    fmap
      (Map.fromList . catMaybes)
      ( for (toList allKnownEntities) \e -> do
          mVal <- SS.lookup set e
          pure $ (e,) <$> mVal
      )
  pure (len, mapEntries)

extractFullStateFromSetViaIterator
  :: (PrimMonad m, U.Unbox a) => MutableSparseSet (PrimState m) a -> m (Int, Map Int a)
extractFullStateFromSetViaIterator set = do
  len <- SS.length set
  entries <- SS.ifoldM (\acc (e, c) -> pure $ Map.insert e c acc) Map.empty set
  pure (len, entries)

hprop_sequential_operations :: Property
hprop_sequential_operations = property do
  -- Generate a sequence of operations
  ops <- forAll $ Gen.list (Range.linear 0 100) genSparseSetOp

  -- Determine all entities ever mentioned to check them later
  let allMentionedEntities = Set.fromList $ concatMap extractOpEntities ops
      finalModel = applyOpsToModel ops Map.empty

  do
    set <- SS.new @TestComponent
    applyOpsToSet set ops
    (setLength, setMap) <- extractFullStateFromSet set allMentionedEntities
    setLength === Map.size finalModel
    setMap === finalModel

  do
    set <- SS.new @TestComponent
    applyOpsToSet set ops
    for_ (Set.toList allMentionedEntities) $ \e -> do
      sutContains <- SS.contains set e
      let modelContains = Map.member e finalModel
      sutContains === modelContains

hprop_clear_removes_all :: Property
hprop_clear_removes_all = property $ do
  ops <- forAll $ Gen.list (Range.linear 1 100) genSparseSetOp
  let allMentionedEntities = Set.fromList $ concatMap extractOpEntities ops

  set <- SS.new @TestComponent
  applyOpsToSet set ops
  SS.clear set
  finalLen <- SS.length set
  found <- or <$> traverse (fmap isJust . SS.lookup set) (toList allMentionedEntities)
  finalLen === 0
  found === False

hprop_compact_preserves_content :: Property
hprop_compact_preserves_content = property $ do
  ops <- forAll $ Gen.list (Range.linear 0 100) genSparseSetOp

  set <- SS.new @TestComponent
  applyOpsToSet set ops

  let model = applyOpsToModel ops Map.empty
  (lenBefore, stateBefore) <- extractFullStateFromSetViaIterator set
  SS.compact set
  (lenAfter, stateAfter) <- extractFullStateFromSetViaIterator set

  stateBefore === model
  lenBefore === lenAfter
  stateBefore === stateAfter

hprop_insert_then_get :: Property
hprop_insert_then_get = property do
  entity <- forAll genEntityId
  component <- forAll genComponent
  set <- SS.new @TestComponent
  SS.insert set entity component
  l <- SS.length set
  v <- SS.lookup set entity
  c <- SS.contains set entity
  l === 1
  v === Just component
  c === True

hprop_insert_update_then_get :: Property
hprop_insert_update_then_get = property do
  entity <- forAll genEntityId
  component1 <- forAll genComponent
  component2 <- forAll genComponent
  set <- SS.new @TestComponent
  SS.insert set entity component1
  SS.insert set entity component2 -- Update
  l <- SS.length set
  v <- SS.lookup set entity
  l === 1
  v === Just component2

hprop_insert_delete_then_get :: Property
hprop_insert_delete_then_get = property do
  entity <- forAll genEntityId
  component <- forAll genComponent
  set <- SS.new @TestComponent
  SS.insert set entity component
  rv <- SS.delete set entity
  l <- SS.length set
  v <- SS.lookup set entity
  c <- SS.contains set entity
  rv === Just component
  l === 0
  v === Nothing
  c === False

hprop_delete_non_existent :: Property
hprop_delete_non_existent = property do
  entity <- forAll genEntityId
  set <- SS.new @TestComponent
  rv <- SS.delete set entity
  l <- SS.length set
  rv === Nothing
  l === 0

hprop_double_delete :: Property
hprop_double_delete = property do
  entity <- forAll genEntityId
  component <- forAll genComponent
  set <- SS.new @TestComponent
  SS.insert set entity component
  r1 <- SS.delete set entity
  r2 <- SS.delete set entity -- Delete again
  l <- SS.length set
  v <- SS.lookup set entity
  r1 === Just component
  r2 === Nothing
  l === 0
  v === Nothing

-- This property specifically stresses the swap logic by creating a denser set.
hprop_dense_delete_integrity :: Property
hprop_dense_delete_integrity = property do
  ops <- forAll $ Gen.list (Range.linear 5 30) genSparseSetOpSmallEntities

  let allMentionedEntities = Set.fromList $ concatMap extractOpEntities ops
      finalModel = applyOpsToModel ops Map.empty

  set <- SS.new @TestComponent
  applyOpsToSet set ops
  (setLength, setMap) <- extractFullStateFromSet set allMentionedEntities

  setLength === Map.size finalModel
  setMap === finalModel

hprop_ifoldIntersectionM_model :: Property
hprop_ifoldIntersectionM_model = property do
  opsA <- forAll $ Gen.list (Range.linear 0 100) genSparseSetOp
  opsB <- forAll $ Gen.list (Range.linear 0 100) genSparseSetOp

  setA <- SS.new @TestComponent
  applyOpsToSet setA opsA
  let modelA = applyOpsToModel opsA Map.empty

  setB <- SS.new @TestComponent
  applyOpsToSet setB opsB
  let modelB = applyOpsToModel opsB Map.empty

  let collectEntities acc entity _ _ = pure (entity : acc)
  sutIntersectedEntities <- SS.ifoldIntersectionM collectEntities [] setA setB

  let modelIntersectedEntities = Set.intersection (Map.keysSet modelA) (Map.keysSet modelB)

  Set.fromList sutIntersectedEntities === modelIntersectedEntities
