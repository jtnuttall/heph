{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Description : Structs of GLSL scalars and their oracles
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Structs under test that hold only GLSL scalars, structs of scalars and arrays
-- of them. They need no vector library, so they test the core package alone.
module Foreign.GPU.Storable.Aligned.Test.Sut.Prim (
  -- * Suites
  baseSuite,
  simpleSuite,
  layoutBugSuite,
  largePrimitivesSuite,
  mixedAlignSuite,
  boolTestSuite,
  nestedPrimSuite,
  arrayStructSuite,
  deepNestSuite,

  -- * Structs
  Simple (Simple),
  genSimple,
  LayoutBug (LayoutBug),
  genLayoutBug,
  LargePrimitives (LargePrimitives),
  genLargePrimitives,
  MixedAlign (MixedAlign),
  genMixedAlign,
  BoolTest (BoolTest),
  genBoolTest,
  NestedPrim (NestedPrim),
  genNestedPrim,
  ArrayStruct (ArrayStruct),
  genArrayStruct,
  DeepNest (DeepNest),
  genDeepNest,
)
where

import Data.Int
import Data.Proxy
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Storable.Sized as SSV
import Foreign
import GHC.Generics
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.Aligned.Test.Glsl
import Foreign.GPU.Storable.Aligned.Test.Harness

-- | Sanity checks of the harness on bare scalars.
baseSuite :: TestTree
baseSuite =
  testGroup
    "Base"
    [ testProperty "float is storable" $ property do
        let x = Strided @Std140 (1.0 :: Float)
        let _ = sizeOf x
        success
    , testProperty "int16 round trips" $ property do
        ptrTripping @Int16 @Int16 12 12
    ]

--------------------------------------------------------------------------------
-- Simple
--------------------------------------------------------------------------------

data Simple = Simple
  { simpleA :: Float
  , simpleB :: Int32
  }
  deriving (Generic, Show, Eq)

genSimple :: Gen Simple
genSimple = Simple <$> genFloat <*> genInt

instance AlignedStorable Std140 Simple
instance AlignedStorable Std430 Simple
instance AlignedStorable Scalar Simple

-- GLSL: struct Simple (glsl/Conformance.glsl)
instance Storable (Oracle 'Std140 Simple) where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = Oracle <$> (Simple <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (Oracle Simple{..}) = do
    pokeByteOff ptr 0 simpleA
    pokeByteOff ptr 4 simpleB

-- GLSL: struct Simple (glsl/Conformance.glsl)
instance Storable (Oracle 'Std430 Simple) where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = Oracle <$> (Simple <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (Oracle Simple{..}) = do
    pokeByteOff ptr 0 simpleA
    pokeByteOff ptr 4 simpleB

-- GLSL: struct Simple (glsl/Conformance.glsl)
instance Storable (Oracle 'Scalar Simple) where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = Oracle <$> (Simple <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (Oracle Simple{..}) = do
    pokeByteOff ptr 0 simpleA
    pokeByteOff ptr 4 simpleB

-- | The Simple tests.
simpleSuite :: TestTree
simpleSuite = sutTests "Simple" genSimple [at @'Std140 (Just 8), at @'Std430 (Just 8), at @'Scalar (Just 8)]

--------------------------------------------------------------------------------
-- LayoutBug
--------------------------------------------------------------------------------

data LayoutBug = LayoutBug
  { fieldA :: Float
  , fieldB :: Float
  , fieldC :: Double
  }
  deriving (Generic, Show, Eq)

genLayoutBug :: Gen LayoutBug
genLayoutBug = LayoutBug <$> genFloat <*> genFloat <*> genFloat

instance AlignedStorable Std140 LayoutBug
instance AlignedStorable Std430 LayoutBug
instance AlignedStorable Scalar LayoutBug

-- GLSL: struct LayoutBug (glsl/Conformance.glsl)
instance Storable (Oracle 'Std140 LayoutBug) where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = Oracle <$> (LayoutBug <$> peekByteOff ptr 0 <*> peekByteOff ptr 4 <*> peekByteOff ptr 8)
  poke ptr (Oracle LayoutBug{..}) = do
    pokeByteOff ptr 0 fieldA
    pokeByteOff ptr 4 fieldB
    pokeByteOff ptr 8 fieldC

-- GLSL: struct LayoutBug (glsl/Conformance.glsl)
instance Storable (Oracle 'Std430 LayoutBug) where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = Oracle <$> (LayoutBug <$> peekByteOff ptr 0 <*> peekByteOff ptr 4 <*> peekByteOff ptr 8)
  poke ptr (Oracle LayoutBug{..}) = do
    pokeByteOff ptr 0 fieldA
    pokeByteOff ptr 4 fieldB
    pokeByteOff ptr 8 fieldC

-- GLSL: struct LayoutBug (glsl/Conformance.glsl)
instance Storable (Oracle 'Scalar LayoutBug) where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = Oracle <$> (LayoutBug <$> peekByteOff ptr 0 <*> peekByteOff ptr 4 <*> peekByteOff ptr 8)
  poke ptr (Oracle LayoutBug{..}) = do
    pokeByteOff ptr 0 fieldA
    pokeByteOff ptr 4 fieldB
    pokeByteOff ptr 8 fieldC

-- | The LayoutBug tests.
layoutBugSuite :: TestTree
layoutBugSuite =
  sutTests
    "LayoutBug"
    genLayoutBug
    [at @'Std140 (Just 16), at @'Std430 (Just 16), at @'Scalar (Just 16)]

--------------------------------------------------------------------------------
-- LargePrimitives
--------------------------------------------------------------------------------

data LargePrimitives = LargePrimitives
  { d :: Double
  , l :: Int64
  }
  deriving (Generic, Show, Eq)

genLargePrimitives :: Gen LargePrimitives
genLargePrimitives = LargePrimitives <$> genFloat <*> genInt

instance AlignedStorable Std140 LargePrimitives
instance AlignedStorable Std430 LargePrimitives
instance AlignedStorable Scalar LargePrimitives

-- GLSL: struct LargePrimitives (glsl/Conformance.glsl)
instance Storable (Oracle 'Std140 LargePrimitives) where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = Oracle <$> (LargePrimitives <$> peekByteOff ptr 0 <*> peekByteOff ptr 8)
  poke ptr (Oracle LargePrimitives{..}) = do
    pokeByteOff ptr 0 d
    pokeByteOff ptr 8 l

-- GLSL: struct LargePrimitives (glsl/Conformance.glsl)
instance Storable (Oracle 'Std430 LargePrimitives) where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = Oracle <$> (LargePrimitives <$> peekByteOff ptr 0 <*> peekByteOff ptr 8)
  poke ptr (Oracle LargePrimitives{..}) = do
    pokeByteOff ptr 0 d
    pokeByteOff ptr 8 l

-- GLSL: struct LargePrimitives (glsl/Conformance.glsl)
instance Storable (Oracle 'Scalar LargePrimitives) where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = Oracle <$> (LargePrimitives <$> peekByteOff ptr 0 <*> peekByteOff ptr 8)
  poke ptr (Oracle LargePrimitives{..}) = do
    pokeByteOff ptr 0 d
    pokeByteOff ptr 8 l

-- | The LargePrimitives tests.
largePrimitivesSuite :: TestTree
largePrimitivesSuite =
  sutTests
    "LargePrimitives"
    genLargePrimitives
    [at @'Std140 (Just 16), at @'Std430 (Just 16), at @'Scalar Nothing]

--------------------------------------------------------------------------------
-- MixedAlign (explicitly tests the difference between std140 and std430)
--------------------------------------------------------------------------------

data MixedAlign = MixedAlign
  { a :: Int16
  , b :: Double
  , c :: Int32
  }
  deriving (Generic, Show, Eq)

genMixedAlign :: Gen MixedAlign
genMixedAlign = MixedAlign <$> genInt <*> genFloat <*> genInt

instance AlignedStorable Std140 MixedAlign
instance AlignedStorable Std430 MixedAlign
instance AlignedStorable Scalar MixedAlign

-- GLSL: struct MixedAlign (glsl/Conformance.glsl)
instance Storable (Oracle 'Std140 MixedAlign) where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr =
    Oracle <$> (MixedAlign <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16)
  poke ptr (Oracle MixedAlign{..}) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 8 b
    pokeByteOff ptr 16 c

-- GLSL: struct MixedAlign (glsl/Conformance.glsl)
instance Storable (Oracle 'Std430 MixedAlign) where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr =
    Oracle <$> (MixedAlign <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16)
  poke ptr (Oracle MixedAlign{..}) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 8 b
    pokeByteOff ptr 16 c

-- GLSL: struct MixedAlign (glsl/Conformance.glsl)
instance Storable (Oracle 'Scalar MixedAlign) where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr =
    Oracle <$> (MixedAlign <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16)
  poke ptr (Oracle MixedAlign{..}) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 8 b
    pokeByteOff ptr 16 c

-- | The MixedAlign tests.
mixedAlignSuite :: TestTree
mixedAlignSuite =
  sutTests
    "MixedAlign"
    genMixedAlign
    [at @'Std140 Nothing, at @'Std430 Nothing, at @'Scalar Nothing]

--------------------------------------------------------------------------------
-- BoolTest
--------------------------------------------------------------------------------

data BoolTest = BoolTest
  { isEnabled :: Bool
  , value :: Float
  }
  deriving (Generic, Show, Eq)

genBoolTest :: Gen BoolTest
genBoolTest = BoolTest <$> Gen.bool <*> genFloat

instance AlignedStorable Std140 BoolTest
instance AlignedStorable Std430 BoolTest
instance AlignedStorable Scalar BoolTest

-- GLSL: struct BoolTest (glsl/Conformance.glsl)
instance Storable (Oracle 'Std140 BoolTest) where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = Oracle <$> (BoolTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (Oracle BoolTest{..}) = do
    pokeByteOff ptr 0 isEnabled
    pokeByteOff ptr 4 value

-- GLSL: struct BoolTest (glsl/Conformance.glsl)
instance Storable (Oracle 'Std430 BoolTest) where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = Oracle <$> (BoolTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (Oracle BoolTest{..}) = do
    pokeByteOff ptr 0 isEnabled
    pokeByteOff ptr 4 value

-- GLSL: struct BoolTest (glsl/Conformance.glsl)
instance Storable (Oracle 'Scalar BoolTest) where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = Oracle <$> (BoolTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (Oracle BoolTest{..}) = do
    pokeByteOff ptr 0 isEnabled
    pokeByteOff ptr 4 value

-- | The BoolTest tests.
boolTestSuite :: TestTree
boolTestSuite =
  sutTests
    "BoolTest"
    genBoolTest
    [at @'Std140 Nothing, at @'Std430 Nothing, at @'Scalar Nothing]

--------------------------------------------------------------------------------
-- NestedPrim
--------------------------------------------------------------------------------

data NestedPrim = NestedPrim
  { npHead :: Float
  , npSimple :: Simple
  , npMixed :: MixedAlign
  , npBool :: BoolTest
  }
  deriving (Generic, Show, Eq)

genNestedPrim :: Gen NestedPrim
genNestedPrim = NestedPrim <$> genFloat <*> genSimple <*> genMixedAlign <*> genBoolTest

instance AlignedStorable Std140 NestedPrim
instance AlignedStorable Std430 NestedPrim
instance AlignedStorable Scalar NestedPrim

-- GLSL: struct NestedPrim (glsl/Conformance.glsl)
instance Storable (Oracle 'Std140 NestedPrim) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( NestedPrim
              <$> peekByteOff ptr 0
              <*> peekOracleAt @'Std140 ptr 16
              <*> peekOracleAt @'Std140 ptr 32
              <*> peekOracleAt @'Std140 ptr 64
          )
  poke ptr (Oracle NestedPrim{..}) = do
    pokeByteOff ptr 0 npHead
    pokeOracleAt @'Std140 ptr 16 npSimple
    pokeOracleAt @'Std140 ptr 32 npMixed
    pokeOracleAt @'Std140 ptr 64 npBool

-- GLSL: struct NestedPrim (glsl/Conformance.glsl)
instance Storable (Oracle 'Std430 NestedPrim) where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( NestedPrim
              <$> peekByteOff ptr 0
              <*> peekOracleAt @'Std430 ptr 4
              <*> peekOracleAt @'Std430 ptr 16
              <*> peekOracleAt @'Std430 ptr 40
          )
  poke ptr (Oracle NestedPrim{..}) = do
    pokeByteOff ptr 0 npHead
    pokeOracleAt @'Std430 ptr 4 npSimple
    pokeOracleAt @'Std430 ptr 16 npMixed
    pokeOracleAt @'Std430 ptr 40 npBool

-- GLSL: struct NestedPrim (glsl/Conformance.glsl)
instance Storable (Oracle 'Scalar NestedPrim) where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( NestedPrim
              <$> peekByteOff ptr 0
              <*> peekOracleAt @'Scalar ptr 4
              <*> peekOracleAt @'Scalar ptr 16
              <*> peekOracleAt @'Scalar ptr 36
          )
  poke ptr (Oracle NestedPrim{..}) = do
    pokeByteOff ptr 0 npHead
    pokeOracleAt @'Scalar ptr 4 npSimple
    pokeOracleAt @'Scalar ptr 16 npMixed
    pokeOracleAt @'Scalar ptr 36 npBool

-- | The NestedPrim tests.
nestedPrimSuite :: TestTree
nestedPrimSuite =
  sutTests
    "NestedPrim"
    genNestedPrim
    [at @'Std140 (Just 80), at @'Std430 (Just 48), at @'Scalar (Just 44)]

--------------------------------------------------------------------------------
-- ArrayStruct
--------------------------------------------------------------------------------

data ArrayStruct = ArrayStruct
  { asA :: Float
  , asB :: SSV.Vector 3 Word16
  , asC :: SSV.Vector 2 Double
  , asD :: Float
  , asE :: Double
  }
  deriving (Generic, Show, Eq)

genArrayStruct :: Gen ArrayStruct
genArrayStruct =
  ArrayStruct
    <$> genFloat
    <*> SSV.replicateM genInt
    <*> SSV.replicateM genFloat
    <*> genFloat
    <*> genFloat

instance AlignedStorable Std140 ArrayStruct
instance AlignedStorable Std430 ArrayStruct
instance AlignedStorable Scalar ArrayStruct

-- GLSL: struct ArrayStruct (glsl/Conformance.glsl)
instance Storable (Oracle 'Std140 ArrayStruct) where
  sizeOf _ = 112
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( ArrayStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 16 peekByteOff ptr 16
              <*> peekArrayAt 16 peekByteOff ptr 64
              <*> peekByteOff ptr 96
              <*> peekByteOff ptr 104
          )
  poke ptr (Oracle ArrayStruct{..}) = do
    pokeByteOff ptr 0 asA
    pokeArrayAt 16 pokeByteOff ptr 16 asB
    pokeArrayAt 16 pokeByteOff ptr 64 asC
    pokeByteOff ptr 96 asD
    pokeByteOff ptr 104 asE

-- GLSL: struct ArrayStruct (glsl/Conformance.glsl)
instance Storable (Oracle 'Std430 ArrayStruct) where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( ArrayStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 2 peekByteOff ptr 4
              <*> peekArrayAt 8 peekByteOff ptr 16
              <*> peekByteOff ptr 32
              <*> peekByteOff ptr 40
          )
  poke ptr (Oracle ArrayStruct{..}) = do
    pokeByteOff ptr 0 asA
    pokeArrayAt 2 pokeByteOff ptr 4 asB
    pokeArrayAt 8 pokeByteOff ptr 16 asC
    pokeByteOff ptr 32 asD
    pokeByteOff ptr 40 asE

-- GLSL: struct ArrayStruct (glsl/Conformance.glsl)
instance Storable (Oracle 'Scalar ArrayStruct) where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( ArrayStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 2 peekByteOff ptr 4
              <*> peekArrayAt 8 peekByteOff ptr 16
              <*> peekByteOff ptr 32
              <*> peekByteOff ptr 40
          )
  poke ptr (Oracle ArrayStruct{..}) = do
    pokeByteOff ptr 0 asA
    pokeArrayAt 2 pokeByteOff ptr 4 asB
    pokeArrayAt 8 pokeByteOff ptr 16 asC
    pokeByteOff ptr 32 asD
    pokeByteOff ptr 40 asE

-- | The ArrayStruct tests.
arrayStructSuite :: TestTree
arrayStructSuite =
  sutTests
    "ArrayStruct"
    genArrayStruct
    [at @'Std140 (Just 112), at @'Std430 (Just 48), at @'Scalar (Just 48)]

--------------------------------------------------------------------------------
-- DeepNest
--------------------------------------------------------------------------------

data DeepNest = DeepNest
  { dnA :: Float
  , dnB :: SV.Vector 5 NestedPrim
  , dnC :: Word8
  , dnD :: Word64
  , dnE :: SV.Vector 5 (SV.Vector 2 (SV.Vector 4 NestedPrim))
  , dnF :: Bool
  }
  deriving (Generic, Show, Eq)

genDeepNest :: Gen DeepNest
genDeepNest =
  DeepNest
    <$> genFloat
    <*> SV.replicateM genNestedPrim
    <*> genInt
    <*> genInt
    <*> SV.replicateM (SV.replicateM (SV.replicateM genNestedPrim))
    <*> Gen.bool

instance AlignedStorable Std140 DeepNest
instance AlignedStorable Std430 DeepNest
instance AlignedStorable Scalar DeepNest

-- GLSL: struct DeepNest (glsl/Conformance.glsl)
instance Storable (Oracle 'Std140 DeepNest) where
  sizeOf _ = 3648
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( DeepNest
              <$> peekByteOff ptr 0
              <*> peekSizedVector (Proxy @(Oracle 'Std140 NestedPrim)) ptr 16
              <*> peekByteOff ptr 416
              <*> peekByteOff ptr 424
              <*> peekSized3DVector (Proxy @(Oracle 'Std140 NestedPrim)) ptr 432
              <*> peekByteOff ptr 3632
          )
  poke ptr (Oracle DeepNest{..}) = do
    pokeByteOff ptr 0 dnA
    pokeSizedVector ptr 16 (SV.map (Oracle @'Std140) dnB)
    pokeByteOff ptr 416 dnC
    pokeByteOff ptr 424 dnD
    pokeSized3DVector (Proxy @(Oracle 'Std140 NestedPrim)) ptr 432 dnE
    pokeByteOff ptr 3632 dnF

-- GLSL: struct DeepNest (glsl/Conformance.glsl)
instance Storable (Oracle 'Std430 DeepNest) where
  sizeOf _ = 2192
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( DeepNest
              <$> peekByteOff ptr 0
              <*> peekSizedVector (Proxy @(Oracle 'Std430 NestedPrim)) ptr 8
              <*> peekByteOff ptr 248
              <*> peekByteOff ptr 256
              <*> peekSized3DVector (Proxy @(Oracle 'Std430 NestedPrim)) ptr 264
              <*> peekByteOff ptr 2184
          )
  poke ptr (Oracle DeepNest{..}) = do
    pokeByteOff ptr 0 dnA
    pokeSizedVector ptr 8 (SV.map (Oracle @'Std430) dnB)
    pokeByteOff ptr 248 dnC
    pokeByteOff ptr 256 dnD
    pokeSized3DVector (Proxy @(Oracle 'Std430 NestedPrim)) ptr 264 dnE
    pokeByteOff ptr 2184 dnF

-- GLSL: struct DeepNest (glsl/Conformance.glsl)
instance Storable (Oracle 'Scalar DeepNest) where
  sizeOf _ = 2176
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( DeepNest
              <$> peekByteOff ptr 0
              <*> peekSizedVector (Proxy @(Oracle 'Scalar NestedPrim)) ptr 8
              <*> peekByteOff ptr 244
              <*> peekByteOff ptr 248
              <*> peekSized3DVector (Proxy @(Oracle 'Scalar NestedPrim)) ptr 256
              <*> peekByteOff ptr 2172
          )
  poke ptr (Oracle DeepNest{..}) = do
    pokeByteOff ptr 0 dnA
    pokeSizedVector ptr 8 (SV.map (Oracle @'Scalar) dnB)
    pokeByteOff ptr 244 dnC
    pokeByteOff ptr 248 dnD
    pokeSized3DVector (Proxy @(Oracle 'Scalar NestedPrim)) ptr 256 dnE
    pokeByteOff ptr 2172 dnF

-- | The DeepNest tests.
deepNestSuite :: TestTree
deepNestSuite =
  sutTestsWith
    (withShrinks 10)
    defaultTripOpts
    "DeepNest"
    genDeepNest
    [at @'Std140 (Just 3636), at @'Std430 (Just 2188), at @'Scalar (Just 2176)]
