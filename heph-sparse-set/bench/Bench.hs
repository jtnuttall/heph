{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant irrefutable pattern" #-}
module Main (main) where

import BenchLib
import Control.DeepSeq
import Control.Monad (replicateM, void)
import Control.Monad.Primitive
import Control.Monad.State.Strict (evalState, state)
import Data.Foldable
import Data.IntMap.Strict qualified as M
import GHC.Generics (Generic)
import System.Random (StdGen, Uniform, mkStdGen, randomR)

import Data.IORef (modifyIORef', newIORef)
import Data.SparseSet.Unboxed.Mutable (MutableSparseSet)
import Data.SparseSet.Unboxed.Mutable qualified as SS

type TestComponent = Int
type TestEntity = Int

-- Common sizes for benchmarks
benchmarkSizes :: [Int]
benchmarkSizes = [10_000, 100_000]

randomSeed :: Int
randomSeed = 7113_1337

main :: IO ()
main = defaultMain [sparseSetBenchmarks, intMapBenchmarks]

-- Main benchmark suite entry point
sparseSetBenchmarks :: Benchmark
sparseSetBenchmarks =
  bgroup
    "Data.SparseSet.Unboxed.Mutable"
    [ makeInsertBenchGroup "Insert_DenseIDs_Sequential_AscendingOrder" False insertSequential
    , makeInsertBenchGroup "Insert_DenseIDs_Sequential_DescendingOrder" True insertSequential
    , makeInsertBenchGroup "Insert_SparseIDs_Step100_AscendingOrder" False insertSparseStep100
    , makeInsertBenchGroup "Insert_SparseIDs_Step100_DescendingOrder" True insertSparseStep100
    , makeUpdateBenchGroup "Update_DenseIDs_Sequential"
    , makeGetBenchGroup "Get_Existing_DenseIDs" True
    , makeGetBenchGroup "Get_NonExisting_DenseIDs" False
    , makeContainsBenchGroup "Contains_Existing_DenseIDs" True
    , makeContainsBenchGroup "Contains_NonExisting_DenseIDs" False
    , makeRemoveBenchGroup "Remove_DenseIDs_AscendingOrder" removeAscending
    , makeRemoveBenchGroup "Remove_DenseIDs_DescendingOrder" removeDescending
    , makeIterationBenchGroup "Iterate_DenseIDs"
    , makeIntersectionBenchGroup "Iterate_Intersection"
    , makeMixedBenchGroup "MixedWorkload_DenseIDs"
    ]
 where
  insertSequential set i = SS.insert set i i
  insertSparseStep100 set i = let entity = i * 100 in SS.insert set entity entity
  removeAscending _ = SS.delete
  removeDescending totalSize set i = SS.delete set (totalSize - 1 - i)

-- Generic benchmark group for N inserts
makeInsertBenchGroup
  :: String
  -> Bool
  -> (MutableSparseSet RealWorld TestComponent -> Int -> IO b)
  -> Benchmark
makeInsertBenchGroup groupName desc insert =
  bgroup
    groupName
    [ bench (show n) $
      perRunEnv
        (SS.new @TestComponent)
        \ ~set -> do
          traverse_ (insert set) (if desc then [n - 1, n - 2 .. 0] else [0 .. n - 1])
    | n <- benchmarkSizes
    ]

-- Generic benchmark group for N updates on existing elements
makeUpdateBenchGroup :: String -> Benchmark
makeUpdateBenchGroup groupName =
  bgroup
    groupName
    [ bench (show n ++ "_updates") $
      perRunEnv
        ( do
            set <- SS.new @TestComponent
            for_ [0 .. n - 1] \i -> SS.insert set i i
            pure set
        )
        \ ~set -> do
          for_ [0 .. n - 1] \i ->
            SS.insert set i (i + 1)
    | n <- benchmarkSizes
    ]

-- Generic benchmark group for N get operations
makeGetBenchGroup :: String -> Bool -> Benchmark
makeGetBenchGroup groupName getExisting =
  bgroup
    groupName
    [ env
      ( do
          -- Setup is run once per benchmark case (e.g., for n=100)
          set <- SS.new @TestComponent
          forM_ [0 .. n - 1] $ \i -> SS.insert set i i
          let idsToAccess =
                if getExisting
                  then [0 .. n - 1] -- Existing IDs
                  else [n .. (2 * n) - 1] -- Non-existing IDs
          pure (set, idsToAccess)
      )
      \ ~(set, idsToAccess) ->
        -- set and idsToAccess are from env
        bench (show n) $ nfIO do
          traverse_ (SS.lookup set) idsToAccess
    | n <- benchmarkSizes
    ]

-- Generic benchmark group for N contains operations
makeContainsBenchGroup :: String -> Bool -> Benchmark
makeContainsBenchGroup groupName checkExisting =
  bgroup
    groupName
    [ env
      ( do
          set <- SS.new @TestComponent
          forM_ [0 .. n - 1] $ \i -> SS.insert set i i
          let idsToAccess =
                if checkExisting
                  then [0 .. n - 1]
                  else [n .. (2 * n) - 1]
          pure (set, idsToAccess)
      )
      $ \ ~(set, idsToAccess) ->
        bench (show n) $ nfIO do
          traverse_ (SS.contains set) idsToAccess
    | n <- benchmarkSizes
    ]

-- Generic benchmark group for N removes
makeRemoveBenchGroup
  :: String -> (Int -> MutableSparseSet RealWorld TestComponent -> Int -> IO b) -> Benchmark
makeRemoveBenchGroup groupName removeAction =
  bgroup
    groupName
    [ bench (show n ++ "_removes") $
      perRunEnv
        ( do
            set <- SS.new @TestComponent
            for_ [0 .. n - 1] $ \i -> SS.insert set i i
            pure set
        )
        \ ~set ->
          traverse_ (removeAction n set) [0 .. n - 1]
    | n <- benchmarkSizes
    ]

makeIterationBenchGroup :: String -> Benchmark
makeIterationBenchGroup groupName =
  bgroup
    groupName
    [ env
      ( do
          -- Setup is run once per benchmark case
          set <- SS.new @TestComponent
          forM_ [0 .. n - 1] $ \i -> SS.insert set i i
          ref <- newIORef 0
          pure (set, ref)
      )
      \ ~(set, ref) ->
        bench (show n) $ nfIO do
          SS.mapM_ (\k -> modifyIORef' ref (+ k)) set
    | n <- benchmarkSizes
    ]

-- Generic benchmark group for N intersection operations
makeIntersectionBenchGroup :: String -> Benchmark
makeIntersectionBenchGroup groupName =
  bgroup
    groupName
    [ env
      ( do
          -- Create two sets with a 50% overlap.
          setA <- SS.new @TestComponent
          forM_ [0 .. n - 1] $ \i -> SS.insert setA i i

          setB <- SS.new @TestComponent
          let offset = n `div` 2
          forM_ [offset .. n + offset - 1] $ \i -> SS.insert setB i i

          ref <- newIORef @Int 0

          pure (setA, setB, ref)
      )
      \ ~(setA, setB, ref) ->
        bench (show n) $ nfIO do
          SS.ifoldIntersectionM (\_ k _ _ -> modifyIORef' ref (+ k)) () setA setB
    | n <- benchmarkSizes
    ]

-- Mixed Workload Benchmarks
data MixedOp = MIns TestEntity TestComponent | MRem TestEntity | MGet TestEntity | MCont TestEntity
  deriving (Show, Generic)

instance Uniform MixedOp

instance NFData MixedOp

generateMixedOps :: Int -> Int -> StdGen -> [MixedOp]
generateMixedOps numOps maxEntity = evalState (replicateM numOps genOp)
 where
  genOp = do
    opType <- state $ randomR (1 :: Int, 4)
    entity <- state $ randomR (0, maxEntity - 1)
    component <- state $ randomR (0, 1000)
    pure case opType of
      1 -> MIns entity component
      2 -> MRem entity
      3 -> MGet entity
      4 -> MCont entity
      _ -> error "Bad operation"

makeMixedBenchGroup :: String -> Benchmark
makeMixedBenchGroup groupName =
  bgroup
    groupName
    [ env
      (pure $ generateMixedOps n n (mkStdGen randomSeed))
      \ ~ops ->
        -- maxEntity = n, fixed seed
        bench (show n ++ "_ops") $ nfIO do
          set <- SS.new
          forM_ ops $ \case
            MIns e c -> SS.insert set e c
            MRem e -> void $ SS.delete set e
            MGet e -> void $ SS.lookup set e
            MCont e -> void $ SS.contains set e
    | n <- benchmarkSizes
    ]

generateMapWithSequentialKeys :: Int -> M.IntMap TestComponent
generateMapWithSequentialKeys n = foldl' (\acc i -> M.insert i i acc) M.empty [0 .. n - 1]

intMapBenchmarks :: Benchmark
intMapBenchmarks =
  bgroup
    "Data.IntMap.Strict"
    [ makeMapInsertBenchGroup "Insert_DenseIDs_Sequential_Map_AscendingOrder" False (\idx _ -> (idx, idx))
    , makeMapInsertBenchGroup "Insert_DenseIDs_Sequential_Map_DesendingOrder" True (\idx _ -> (idx, idx))
    , makeMapInsertBenchGroup
        "Insert_SparseIDs_Step100_Map_AscendingOrder"
        False
        (\idx _ -> (idx * 100, idx * 100))
    , makeMapInsertBenchGroup
        "Insert_SparseIDs_Step100_Map_DescendingOrder"
        True
        (\idx _ -> (idx * 100, idx * 100))
    , makeMapUpdateBenchGroup "Update_DenseIDs_Sequential_Map"
    , makeMapGetBenchGroup "Get_Existing_DenseIDs_Map" True
    , makeMapGetBenchGroup "Get_NonExisting_DenseIDs_Map" False
    , makeMapContainsBenchGroup "Contains_Existing_DenseIDs_Map" True -- Added for completeness
    , makeMapContainsBenchGroup "Contains_NonExisting_DenseIDs_Map" False
    , makeMapRemoveBenchGroup "Remove_DenseIDs_AscendingOrder_Map" (\_ _ entityIdx -> entityIdx)
    , makeMapRemoveBenchGroup
        "Remove_DenseIDs_DescendingOrder_Map"
        (\_ mapSize entityIdx -> mapSize - 1 - entityIdx)
    , makeMapIterationBenchGroup "Iterate_DenseIDs_Map"
    , makeMapIntersectionBenchGroup "Iterate_Intersection_Map"
    , makeMapMixedBenchGroup "MixedWorkload_DenseIDs_Map"
    ]

makeMapInsertBenchGroup
  :: String -> Bool -> (Int -> Int -> (TestEntity, TestComponent)) -> Benchmark
makeMapInsertBenchGroup groupName desc valueGenerator =
  bgroup
    groupName
    [ bench (show n) $
      nf
        ( \size ->
            foldl'
              (\m i -> let (k, v) = valueGenerator i size in M.insert k v m)
              M.empty
              (if desc then [n - 1, n - 2 .. 0] else [0 .. n - 1])
        )
        n
    | n <- benchmarkSizes
    ]

makeMapUpdateBenchGroup :: String -> Benchmark
makeMapUpdateBenchGroup groupName =
  bgroup
    groupName
    [ env (pure (generateMapWithSequentialKeys n)) $ \ ~initialMap ->
      bench (show n ++ "_updates") $
        -- For Map, an "update" is just an insert on an existing key
        nf (\m -> foldl' (\currentMap i -> M.insert i (i + 1) currentMap) m [0 .. n - 1]) initialMap
    | n <- benchmarkSizes
    ]

makeMapGetBenchGroup :: String -> Bool -> Benchmark
makeMapGetBenchGroup groupName getExisting =
  bgroup
    groupName
    [ env
      ( do
          let initialMap = generateMapWithSequentialKeys n
          let idsToAccess =
                if getExisting
                  then [0 .. n - 1] -- Existing IDs
                  else [n .. (2 * n) - 1] -- Non-existing IDs
          pure (initialMap, idsToAccess)
      )
      $ \ ~(m, idsToAccess) ->
        bench (show n) $ nf (map (`M.lookup` m)) idsToAccess
    | n <- benchmarkSizes
    ]

makeMapContainsBenchGroup :: String -> Bool -> Benchmark
makeMapContainsBenchGroup groupName checkExisting =
  bgroup
    groupName
    [ env
      ( do
          let initialMap = generateMapWithSequentialKeys n
          let idsToAccess =
                if checkExisting
                  then [0 .. n - 1]
                  else [n .. (2 * n) - 1]
          pure (initialMap, idsToAccess)
      )
      $ \ ~(m, idsToAccess) ->
        bench (show n) $ nf (map (`M.member` m)) idsToAccess
    | n <- benchmarkSizes
    ]

makeMapIterationBenchGroup :: String -> Benchmark
makeMapIterationBenchGroup groupName =
  bgroup
    groupName
    [ env
      ( do
          let initialMap = generateMapWithSequentialKeys n
          ref <- newIORef 0
          pure (initialMap, ref)
      )
      $ \ ~(initialMap, ref) ->
        bench (show n) $ nfIO do
          traverse_ (\k -> modifyIORef' ref (+ k)) initialMap
    | n <- benchmarkSizes
    ]

makeMapIntersectionBenchGroup :: String -> Benchmark
makeMapIntersectionBenchGroup groupName =
  bgroup
    groupName
    [ env
      ( do
          -- Create two maps with the same 50% overlap.
          let mapA = generateMapWithSequentialKeys n
              offset = n `div` 2
              mapB = foldl' (\acc i -> M.insert i i acc) M.empty [offset .. n + offset - 1]
          ref <- newIORef @Int 0
          pure (mapA, mapB, ref)
      )
      \ ~(mapA, mapB, ref) ->
        bench (show n) $ nfIO do
          let intersectionMap = M.intersectionWithKey (\eid _ _ -> eid) mapA mapB
          traverse_ (\k -> modifyIORef' ref (+ k)) intersectionMap
    | n <- benchmarkSizes
    ]

makeMapRemoveBenchGroup
  :: String -> (M.IntMap TestComponent -> Int -> Int -> TestEntity) -> Benchmark
makeMapRemoveBenchGroup groupName entityToRemoveGenerator =
  bgroup
    groupName
    [ env (pure (generateMapWithSequentialKeys n)) $ \ ~initialMap ->
      bench (show n ++ "_removes") $
        nf
          (\m -> foldl' (\currentMap i -> M.delete (entityToRemoveGenerator m n i) currentMap) m [0 .. n - 1])
          initialMap
    | n <- benchmarkSizes
    ]

makeMapMixedBenchGroup :: String -> Benchmark
makeMapMixedBenchGroup groupName =
  bgroup
    groupName
    [ env (pure $ generateMixedOps n n (mkStdGen randomSeed)) $ \ops ->
      bench (show n ++ "_ops") $ nf (runMapOps M.empty) ops
    | n <- benchmarkSizes
    ]
 where
  runMapOps :: M.IntMap TestComponent -> [MixedOp] -> M.IntMap TestComponent
  runMapOps = foldl' applyMapOp

  applyMapOp :: M.IntMap TestComponent -> MixedOp -> M.IntMap TestComponent
  applyMapOp currentMap op = case op of
    MIns e c -> M.insert e c currentMap
    MRem e -> M.delete e currentMap
    MGet e -> let !_ = M.lookup e currentMap in currentMap -- Force lookup
    MCont e -> let !_ = M.member e currentMap in currentMap -- Force member check
