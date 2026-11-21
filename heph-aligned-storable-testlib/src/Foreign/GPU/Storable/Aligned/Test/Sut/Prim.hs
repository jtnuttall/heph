{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Description : Conformance tests for structs of GLSL scalars
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
module Foreign.GPU.Storable.Aligned.Test.Sut.Prim (
  -- * Base checks
  hprop_base_float_is_storable,
  hprop_base_int16_round_trips,

  -- * Simple
  Simple (..),
  genSimple,
  SimpleStd140 (..),
  SimpleStd430 (..),
  SimpleScalar (..),
  unit_simple_std140_packed_size,
  hprop_simple_std140_round_trip,
  unit_simple_std430_packed_size,
  hprop_simple_std430_round_trip,
  unit_simple_scalar_packed_size,
  hprop_simple_scalar_round_trip,

  -- * LayoutBug
  LayoutBug (..),
  genLayoutBug,
  LayoutBugStd140 (..),
  LayoutBugStd430 (..),
  LayoutBugScalar (..),
  unit_layoutbug_std140_packed_size,
  hprop_layoutbug_std140_round_trip,
  unit_layoutbug_std430_packed_size,
  hprop_layoutbug_std430_round_trip,
  unit_layoutbug_scalar_packed_size,
  hprop_layoutbug_scalar_round_trip,

  -- * LargePrimitives
  LargePrimitives (..),
  genLargePrimitives,
  LargePrimitivesStd140 (..),
  LargePrimitivesStd430 (..),
  LargePrimitivesScalar (..),
  unit_largeprimitives_std140_packed_size,
  hprop_largeprimitives_std140_round_trip,
  unit_largeprimitives_std430_packed_size,
  hprop_largeprimitives_std430_round_trip,
  hprop_largeprimitives_scalar_round_trip,

  -- * MixedAlign
  MixedAlign (..),
  genMixedAlign,
  MixedAlignStd140 (..),
  MixedAlignStd430 (..),
  MixedAlignScalar (..),
  hprop_mixedalign_std140_round_trip,
  hprop_mixedalign_std430_round_trip,
  hprop_mixedalign_scalar_round_trip,

  -- * BoolTest
  BoolTest (..),
  genBoolTest,
  BoolTestStd140 (..),
  BoolTestStd430 (..),
  BoolTestScalar (..),
  hprop_booltest_std140_round_trip,
  hprop_booltest_std430_round_trip,
  hprop_booltest_scalar_round_trip,
)
where

import Data.Int
import Data.Proxy
import Foreign
import GHC.Generics
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Tasty.HUnit

import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.Aligned.Test.Harness

hprop_base_float_is_storable :: Property
hprop_base_float_is_storable = property do
  let x = Strided @Std140 (1.0 :: Float)
  let _ = sizeOf x
  success

hprop_base_int16_round_trips :: Property
hprop_base_int16_round_trips = property do
  ptrTripping @Int16 @Int16 12 12

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

newtype SimpleStd140 = SimpleStd140 Simple
  deriving (Generic, Show, Eq)

instance Storable SimpleStd140 where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = SimpleStd140 <$> (Simple <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (SimpleStd140 Simple{..}) = do
    pokeByteOff ptr 0 simpleA
    pokeByteOff ptr 4 simpleB

unit_simple_std140_packed_size :: Assertion
unit_simple_std140_packed_size =
  packedAlignedSizeOf @Std140 @Simple Proxy Proxy @?= 8

hprop_simple_std140_round_trip :: Property
hprop_simple_std140_round_trip = property do
  simple <- forAll genSimple
  let sut = Strided @Std140 simple
      oracle = SimpleStd140 simple
  ptrTripping sut oracle

newtype SimpleStd430 = SimpleStd430 Simple
  deriving (Generic, Show, Eq)

instance Storable SimpleStd430 where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = SimpleStd430 <$> (Simple <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (SimpleStd430 Simple{..}) = do
    pokeByteOff ptr 0 simpleA
    pokeByteOff ptr 4 simpleB

unit_simple_std430_packed_size :: Assertion
unit_simple_std430_packed_size =
  packedAlignedSizeOf @Std430 @Simple Proxy Proxy @?= 8

hprop_simple_std430_round_trip :: Property
hprop_simple_std430_round_trip = property do
  val <- forAll genSimple
  let sut = Strided @Std430 val
  let oracle = SimpleStd430 val
  ptrTripping sut oracle

newtype SimpleScalar = SimpleScalar Simple
  deriving (Generic, Show, Eq)

instance Storable SimpleScalar where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = SimpleScalar <$> (Simple <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (SimpleScalar Simple{..}) = do
    pokeByteOff ptr 0 simpleA
    pokeByteOff ptr 4 simpleB

unit_simple_scalar_packed_size :: Assertion
unit_simple_scalar_packed_size =
  packedAlignedSizeOf @Scalar @Simple Proxy Proxy @?= 8

hprop_simple_scalar_round_trip :: Property
hprop_simple_scalar_round_trip = property do
  val <- forAll genSimple
  let sut = Strided @Scalar val
  let oracle = SimpleScalar val
  ptrTripping sut oracle

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

newtype LayoutBugStd140 = LayoutBugStd140 LayoutBug
  deriving (Generic, Show, Eq)

instance Storable LayoutBugStd140 where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = LayoutBugStd140 <$> (LayoutBug <$> peekByteOff ptr 0 <*> peekByteOff ptr 4 <*> peekByteOff ptr 8)
  poke ptr (LayoutBugStd140 LayoutBug{..}) = do
    pokeByteOff ptr 0 fieldA
    pokeByteOff ptr 4 fieldB
    pokeByteOff ptr 8 fieldC

unit_layoutbug_std140_packed_size :: Assertion
unit_layoutbug_std140_packed_size =
  packedAlignedSizeOf @Std140 @LayoutBug Proxy Proxy @?= 16

hprop_layoutbug_std140_round_trip :: Property
hprop_layoutbug_std140_round_trip = property do
  val <- forAll genLayoutBug
  let sut = Strided @Std140 val
      oracle = LayoutBugStd140 val
  ptrTripping sut oracle

newtype LayoutBugStd430 = LayoutBugStd430 LayoutBug
  deriving (Generic, Show, Eq)

instance Storable LayoutBugStd430 where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = LayoutBugStd430 <$> (LayoutBug <$> peekByteOff ptr 0 <*> peekByteOff ptr 4 <*> peekByteOff ptr 8)
  poke ptr (LayoutBugStd430 LayoutBug{..}) = do
    pokeByteOff ptr 0 fieldA
    pokeByteOff ptr 4 fieldB
    pokeByteOff ptr 8 fieldC

unit_layoutbug_std430_packed_size :: Assertion
unit_layoutbug_std430_packed_size =
  packedAlignedSizeOf @Std430 @LayoutBug Proxy Proxy @?= 16

hprop_layoutbug_std430_round_trip :: Property
hprop_layoutbug_std430_round_trip = property do
  val <- forAll genLayoutBug
  let sut = Strided @Std430 val
  let oracle = LayoutBugStd430 val
  ptrTripping sut oracle

newtype LayoutBugScalar = LayoutBugScalar LayoutBug
  deriving (Generic, Show, Eq)

instance Storable LayoutBugScalar where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = LayoutBugScalar <$> (LayoutBug <$> peekByteOff ptr 0 <*> peekByteOff ptr 4 <*> peekByteOff ptr 8)
  poke ptr (LayoutBugScalar LayoutBug{..}) = do
    pokeByteOff ptr 0 fieldA
    pokeByteOff ptr 4 fieldB
    pokeByteOff ptr 8 fieldC

unit_layoutbug_scalar_packed_size :: Assertion
unit_layoutbug_scalar_packed_size =
  packedAlignedSizeOf @Scalar @LayoutBug Proxy Proxy @?= 16

hprop_layoutbug_scalar_round_trip :: Property
hprop_layoutbug_scalar_round_trip = property do
  val <- forAll genLayoutBug
  let sut = Strided @Scalar val
  let oracle = LayoutBugScalar val
  ptrTripping sut oracle

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

newtype LargePrimitivesStd140 = LargePrimitivesStd140 LargePrimitives
  deriving (Generic, Show, Eq)

instance Storable LargePrimitivesStd140 where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = LargePrimitivesStd140 <$> (LargePrimitives <$> peekByteOff ptr 0 <*> peekByteOff ptr 8)
  poke ptr (LargePrimitivesStd140 LargePrimitives{..}) = do
    pokeByteOff ptr 0 d
    pokeByteOff ptr 8 l

unit_largeprimitives_std140_packed_size :: Assertion
unit_largeprimitives_std140_packed_size =
  packedAlignedSizeOf @Std140 @LargePrimitives Proxy Proxy @?= 16

hprop_largeprimitives_std140_round_trip :: Property
hprop_largeprimitives_std140_round_trip = property do
  val <- forAll genLargePrimitives
  let sut = Strided @Std140 val
  let oracle = LargePrimitivesStd140 val
  ptrTripping sut oracle

newtype LargePrimitivesStd430 = LargePrimitivesStd430 LargePrimitives
  deriving (Generic, Show, Eq)

instance Storable LargePrimitivesStd430 where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = LargePrimitivesStd430 <$> (LargePrimitives <$> peekByteOff ptr 0 <*> peekByteOff ptr 8)
  poke ptr (LargePrimitivesStd430 LargePrimitives{..}) = do
    pokeByteOff ptr 0 d
    pokeByteOff ptr 8 l

unit_largeprimitives_std430_packed_size :: Assertion
unit_largeprimitives_std430_packed_size =
  packedAlignedSizeOf @Std430 @LargePrimitives Proxy Proxy @?= 16

hprop_largeprimitives_std430_round_trip :: Property
hprop_largeprimitives_std430_round_trip = property do
  val <- forAll genLargePrimitives
  let sut = Strided @Std430 val
  let oracle = LargePrimitivesStd430 val
  ptrTripping sut oracle

newtype LargePrimitivesScalar = LargePrimitivesScalar LargePrimitives
  deriving (Generic, Show, Eq)

instance Storable LargePrimitivesScalar where
  sizeOf _ = 16
  alignment _ = 8
  peek ptr = LargePrimitivesScalar <$> (LargePrimitives <$> peekByteOff ptr 0 <*> peekByteOff ptr 8)
  poke ptr (LargePrimitivesScalar LargePrimitives{..}) = do
    pokeByteOff ptr 0 d
    pokeByteOff ptr 8 l

hprop_largeprimitives_scalar_round_trip :: Property
hprop_largeprimitives_scalar_round_trip = property do
  val <- forAll genLargePrimitives
  let sut = Strided @Scalar val
  let oracle = LargePrimitivesScalar val
  ptrTripping sut oracle

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

newtype MixedAlignStd140 = MixedAlignStd140 MixedAlign
  deriving (Generic, Show, Eq)

instance Storable MixedAlignStd140 where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr =
    MixedAlignStd140 <$> (MixedAlign <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16)
  poke ptr (MixedAlignStd140 MixedAlign{..}) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 8 b
    pokeByteOff ptr 16 c

hprop_mixedalign_std140_round_trip :: Property
hprop_mixedalign_std140_round_trip = property do
  val <- forAll genMixedAlign
  let sut = Strided @Std140 val
  let oracle = MixedAlignStd140 val
  ptrTripping sut oracle

newtype MixedAlignStd430 = MixedAlignStd430 MixedAlign
  deriving (Generic, Show, Eq)

instance Storable MixedAlignStd430 where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr =
    MixedAlignStd430 <$> (MixedAlign <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16)
  poke ptr (MixedAlignStd430 MixedAlign{..}) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 8 b
    pokeByteOff ptr 16 c

hprop_mixedalign_std430_round_trip :: Property
hprop_mixedalign_std430_round_trip = property do
  val <- forAll genMixedAlign
  let sut = Strided @Std430 val
  let oracle = MixedAlignStd430 val
  ptrTripping sut oracle

newtype MixedAlignScalar = MixedAlignScalar MixedAlign
  deriving (Generic, Show, Eq)

instance Storable MixedAlignScalar where
  sizeOf _ = 24
  alignment _ = 8
  peek ptr =
    MixedAlignScalar <$> (MixedAlign <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16)
  poke ptr (MixedAlignScalar MixedAlign{..}) = do
    pokeByteOff ptr 0 a
    pokeByteOff ptr 8 b
    pokeByteOff ptr 16 c

hprop_mixedalign_scalar_round_trip :: Property
hprop_mixedalign_scalar_round_trip = property do
  val <- forAll genMixedAlign
  let sut = Strided @Scalar val
  let oracle = MixedAlignScalar val
  ptrTripping sut oracle

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

newtype BoolTestStd140 = BoolTestStd140 BoolTest
  deriving (Generic, Show, Eq)

instance Storable BoolTestStd140 where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = BoolTestStd140 <$> (BoolTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (BoolTestStd140 BoolTest{..}) = do
    pokeByteOff ptr 0 isEnabled
    pokeByteOff ptr 4 value

hprop_booltest_std140_round_trip :: Property
hprop_booltest_std140_round_trip = property do
  val <- forAll genBoolTest
  let sut = Strided @Std140 val
      oracle = BoolTestStd140 val
  ptrTripping sut oracle

newtype BoolTestStd430 = BoolTestStd430 BoolTest
  deriving (Generic, Show, Eq)

instance Storable BoolTestStd430 where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = BoolTestStd430 <$> (BoolTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (BoolTestStd430 BoolTest{..}) = do
    pokeByteOff ptr 0 isEnabled
    pokeByteOff ptr 4 value

hprop_booltest_std430_round_trip :: Property
hprop_booltest_std430_round_trip = property do
  val <- forAll genBoolTest
  let sut = Strided @Std430 val
      oracle = BoolTestStd430 val
  ptrTripping sut oracle

newtype BoolTestScalar = BoolTestScalar BoolTest
  deriving (Generic, Show, Eq)

instance Storable BoolTestScalar where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = BoolTestScalar <$> (BoolTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (BoolTestScalar BoolTest{..}) = do
    pokeByteOff ptr 0 isEnabled
    pokeByteOff ptr 4 value

hprop_booltest_scalar_round_trip :: Property
hprop_booltest_scalar_round_trip = property do
  val <- forAll genBoolTest
  let sut = Strided @Scalar val
      oracle = BoolTestScalar val
  ptrTripping sut oracle
