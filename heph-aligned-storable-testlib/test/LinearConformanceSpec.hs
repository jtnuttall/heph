{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Alignments verified by running glslang against AlignedSpec.glsl and manually
-- inspecting the SPIR-V.
--
-- You can use https://godbolt.org/z/njnvM8q4o to check for yourself.
module LinearConformanceSpec where

import Data.Coerce
import Data.Int
import Data.Proxy
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Storable.Sized as SSV
import Data.Word
import Foreign
import GHC.Generics
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Linear hiding (outer)
import Numeric.Half
import Test.Tasty.HUnit

import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.Aligned.Test

--------------------------------------------------------------------------------
-- Nonsquare
--------------------------------------------------------------------------------

-- | Needs to be documented, but Mnm -> matnxm
data TestNonSquare = TestNonSquare
  { matA :: M32 Half
  , matB :: M43 Double
  }
  deriving (Generic, Show, Eq)

genTestNonSquare :: Gen TestNonSquare
genTestNonSquare = TestNonSquare <$> genMat genFloat <*> genMat genFloat

instance AlignedStorable Std140 TestNonSquare
instance AlignedStorable Std430 TestNonSquare
instance AlignedStorable Scalar TestNonSquare

newtype TestNonSquareStd140 = TestNonSquareStd140 TestNonSquare
  deriving (Generic, Show, Eq)

instance Storable TestNonSquareStd140 where
  sizeOf _ = 192
  alignment _ = 32
  peek ptr =
    TestNonSquareStd140
      <$> (TestNonSquare <$> peekMat3Oracle Std140 ptr 0 <*> peekMat4Oracle Std140 ptr 64)
  poke ptr (TestNonSquareStd140 TestNonSquare{..}) = do
    pokeMat3Oracle Std140 ptr 0 matA
    pokeMat4Oracle Std140 ptr 64 matB

hprop_testnonsquare_std140_round_trip :: Property
hprop_testnonsquare_std140_round_trip = property do
  value <- forAll genTestNonSquare
  let sut = Strided @Std140 value
      oracle = TestNonSquareStd140 value
  ptrTripping sut oracle

newtype TestNonSquareStd430 = TestNonSquareStd430 TestNonSquare
  deriving (Generic, Show, Eq)

instance Storable TestNonSquareStd430 where
  sizeOf _ = 160
  alignment _ = 32
  peek ptr =
    TestNonSquareStd430
      <$> (TestNonSquare <$> peekMat3Oracle Std430 ptr 0 <*> peekMat4Oracle Std430 ptr 32)
  poke ptr (TestNonSquareStd430 TestNonSquare{..}) = do
    pokeMat3Oracle Std430 ptr 0 matA
    pokeMat4Oracle Std430 ptr 32 matB

hprop_testnonsquare_std430_round_trip :: Property
hprop_testnonsquare_std430_round_trip = property do
  value <- forAll genTestNonSquare
  let sut = Strided @Std430 value
      oracle = TestNonSquareStd430 value
  ptrTripping sut oracle

newtype TestNonSquareScalar = TestNonSquareScalar TestNonSquare
  deriving (Generic, Show, Eq)

instance Storable TestNonSquareScalar where
  sizeOf _ = 112
  alignment _ = 8
  peek ptr = TestNonSquareScalar <$> (TestNonSquare <$> peekByteOff ptr 0 <*> peekByteOff ptr 16)
  poke ptr (TestNonSquareScalar TestNonSquare{..}) = do
    pokeByteOff ptr 0 matA
    pokeByteOff ptr 16 matB

hprop_testnonsquare_scalar_round_trip :: Property
hprop_testnonsquare_scalar_round_trip = property do
  value <- forAll genTestNonSquare
  let sut = Strided @Scalar value
      oracle = TestNonSquareScalar value
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- D2
--------------------------------------------------------------------------------

data D2 = D2
  { d2A :: Half
  , d2B :: V2 Float
  , d2C :: Half
  , d2D :: M22 Float
  , d2E :: Half
  }
  deriving (Generic, Show, Eq)

genD2 :: Gen D2
genD2 = D2 <$> genFloat <*> genV genFloat <*> genFloat <*> genMat genFloat <*> genFloat

instance AlignedStorable Std140 D2
instance AlignedStorable Std430 D2
instance AlignedStorable Scalar D2

newtype D2Std140 = D2Std140 D2
  deriving (Generic, Show, Eq)

instance Storable D2Std140 where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    D2Std140
      <$> ( D2
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 8
              <*> peekByteOff ptr 16
              <*> peekMat2Oracle Std140 ptr 32
              <*> peekByteOff ptr 64
          )
  poke ptr (D2Std140 D2{..}) = do
    pokeByteOff ptr 0 d2A
    pokeByteOff ptr 8 d2B
    pokeByteOff ptr 16 d2C
    pokeMat2Oracle Std140 ptr 32 d2D
    pokeByteOff ptr 64 d2E

unit_d2_std140_packed_size :: Assertion
unit_d2_std140_packed_size =
  packedAlignedSizeOf @Std140 @D2 Proxy Proxy @?= 66

hprop_d2_std140_round_trip :: Property
hprop_d2_std140_round_trip = property do
  d2 <- forAll genD2
  let sut = Strided @Std140 d2
      oracle = D2Std140 d2
  ptrTripping sut oracle

newtype D2Std430 = D2Std430 D2
  deriving (Generic, Show, Eq)

instance Storable D2Std430 where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr =
    D2Std430
      <$> ( D2
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 8
              <*> peekByteOff ptr 16
              <*> peekMat2Oracle Std430 ptr 24
              <*> peekByteOff ptr 40
          )
  poke ptr (D2Std430 D2{..}) = do
    pokeByteOff ptr 0 d2A
    pokeByteOff ptr 8 d2B
    pokeByteOff ptr 16 d2C
    pokeMat2Oracle Std430 ptr 24 d2D
    pokeByteOff ptr 40 d2E

unit_d2_std430_packed_size :: Assertion
unit_d2_std430_packed_size =
  packedAlignedSizeOf @Std430 @D2 Proxy Proxy @?= 42

hprop_d2_std430_round_trip :: Property
hprop_d2_std430_round_trip = property do
  val <- forAll genD2
  let sut = Strided @Std430 val
      oracle = D2Std430 val
  ptrTripping sut oracle

newtype D2Scalar = D2Scalar D2
  deriving (Generic, Show, Eq)

instance Storable D2Scalar where
  sizeOf _ = 36
  alignment _ = 4
  peek ptr =
    D2Scalar
      <$> ( D2
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 4
              <*> peekByteOff ptr 12
              <*> peekByteOff ptr 16
              <*> peekByteOff ptr 32
          )
  poke ptr (D2Scalar D2{..}) = do
    pokeByteOff ptr 0 d2A
    pokeByteOff ptr 4 d2B
    pokeByteOff ptr 12 d2C
    pokeByteOff ptr 16 d2D
    pokeByteOff ptr 32 d2E

unit_d2_scalar_packed_size :: Assertion
unit_d2_scalar_packed_size =
  packedAlignedSizeOf @Scalar @D2 Proxy Proxy @?= 34

hprop_d2_scalar_round_trip :: Property
hprop_d2_scalar_round_trip = property do
  val <- forAll genD2
  let sut = Strided @Scalar val
      oracle = D2Scalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- Kitchen sink
--------------------------------------------------------------------------------

-- | Every shader primitive, ordered to be pretty close to worst case for padding
-- rules
data KitchenSink = KitchenSink
  { ksA :: Half
  , ksB :: V3 Double
  , ksC :: Float
  , ksD :: M33 Float
  , ksE :: Int32
  , ksF :: M33 Float
  , ksG :: Word8
  , ksH :: M32 Half
  , ksI :: Word16
  , ksJ :: M23 Half
  , ksK :: Word16
  , ksL :: M43 Float
  , ksM :: Int16
  , ksN :: M34 Double
  , ksO :: Bool
  , ksP :: Int8
  , ksQ :: M44 Half
  , ksR :: Int64
  , ksS :: M42 Double
  , ksT :: Word64
  , ksU :: M24 Half
  , ksV :: Half
  }
  deriving (Generic, Show, Eq)

genKitchenSink :: Gen KitchenSink
genKitchenSink =
  KitchenSink
    <$> genFloat
    <*> genV genFloat
    <*> genFloat
    <*> genMat genFloat
    <*> genInt
    <*> genMat genFloat
    <*> genInt
    <*> genMat genFloat
    <*> genInt
    <*> genMat genFloat
    <*> genInt
    <*> genMat genFloat
    <*> genInt
    <*> genMat genFloat
    <*> Gen.bool
    <*> genInt
    <*> genMat genFloat
    <*> genInt
    <*> genMat genFloat
    <*> genInt
    <*> genMat genFloat
    <*> genFloat

instance AlignedStorable Std140 KitchenSink
instance AlignedStorable Std430 KitchenSink
instance AlignedStorable Scalar KitchenSink

newtype KitchenSinkStd140 = KitchenSinkStd140 KitchenSink
  deriving (Generic, Show, Eq)

instance Storable KitchenSinkStd140 where
  sizeOf _ = 704
  alignment _ = 32
  peek ptr =
    KitchenSinkStd140
      <$> ( KitchenSink
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 32
              <*> peekByteOff ptr 56
              <*> peekMat3Oracle Std140 ptr 64
              <*> peekByteOff ptr 112
              <*> peekMat3Oracle Std140 ptr 128
              <*> peekByteOff ptr 176
              <*> peekMat3Oracle Std140 ptr 192
              <*> peekByteOff ptr 240
              <*> peekMat2Oracle Std140 ptr 256
              <*> peekByteOff ptr 288
              <*> peekMat4Oracle Std140 ptr 304
              <*> peekByteOff ptr 368
              <*> peekMat3Oracle Std140 ptr 384
              <*> peekByteOff ptr 480
              <*> peekByteOff ptr 484
              <*> peekMat4Oracle Std140 ptr 496
              <*> peekByteOff ptr 560
              <*> peekMat4Oracle Std140 ptr 576
              <*> peekByteOff ptr 640
              <*> peekMat2Oracle Std140 ptr 656
              <*> peekByteOff ptr 688
          )
  poke ptr (KitchenSinkStd140 KitchenSink{..}) = do
    pokeByteOff ptr 0 ksA
    pokeByteOff ptr 32 ksB
    pokeByteOff ptr 56 ksC
    pokeMat3Oracle Std140 ptr 64 ksD
    pokeByteOff ptr 112 ksE
    pokeMat3Oracle Std140 ptr 128 ksF
    pokeByteOff ptr 176 ksG
    pokeMat3Oracle Std140 ptr 192 ksH
    pokeByteOff ptr 240 ksI
    pokeMat2Oracle Std140 ptr 256 ksJ
    pokeByteOff ptr 288 ksK
    pokeMat4Oracle Std140 ptr 304 ksL
    pokeByteOff ptr 368 ksM
    pokeMat3Oracle Std140 ptr 384 ksN
    pokeByteOff ptr 480 ksO
    pokeByteOff ptr 484 ksP
    pokeMat4Oracle Std140 ptr 496 ksQ
    pokeByteOff ptr 560 ksR
    pokeMat4Oracle Std140 ptr 576 ksS
    pokeByteOff ptr 640 ksT
    pokeMat2Oracle Std140 ptr 656 ksU
    pokeByteOff ptr 688 ksV

hprop_kitchensink_std140_round_trip :: Property
hprop_kitchensink_std140_round_trip = property do
  val <- forAll genKitchenSink
  let sut = Strided @Std140 val
      oracle = KitchenSinkStd140 val
  ptrTripping sut oracle

newtype KitchenSinkStd430 = KitchenSinkStd430 KitchenSink
  deriving (Generic, Show, Eq)

instance Storable KitchenSinkStd430 where
  sizeOf _ = 576
  alignment _ = 32
  peek ptr =
    KitchenSinkStd430
      <$> ( KitchenSink
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 32
              <*> peekByteOff ptr 56
              <*> peekMat3Oracle Std430 ptr 64
              <*> peekByteOff ptr 112
              <*> peekMat3Oracle Std430 ptr 128
              <*> peekByteOff ptr 176
              <*> peekMat3Oracle Std430 ptr 180
              <*> peekByteOff ptr 192
              <*> peekMat2Oracle Std430 ptr 200
              <*> peekByteOff ptr 216
              <*> peekMat4Oracle Std430 ptr 224
              <*> peekByteOff ptr 288
              <*> peekMat3Oracle Std430 ptr 320
              <*> peekByteOff ptr 416
              <*> peekByteOff ptr 420
              <*> peekMat4Oracle Std430 ptr 424
              <*> peekByteOff ptr 456
              <*> peekMat4Oracle Std430 ptr 464
              <*> peekByteOff ptr 528
              <*> peekMat2Oracle Std430 ptr 536
              <*> peekByteOff ptr 552
          )
  poke ptr (KitchenSinkStd430 KitchenSink{..}) = do
    pokeByteOff ptr 0 ksA
    pokeByteOff ptr 32 ksB
    pokeByteOff ptr 56 ksC
    pokeMat3Oracle Std430 ptr 64 ksD
    pokeByteOff ptr 112 ksE
    pokeMat3Oracle Std430 ptr 128 ksF
    pokeByteOff ptr 176 ksG
    pokeMat3Oracle Std430 ptr 180 ksH
    pokeByteOff ptr 192 ksI
    pokeMat2Oracle Std430 ptr 200 ksJ
    pokeByteOff ptr 216 ksK
    pokeMat4Oracle Std430 ptr 224 ksL
    pokeByteOff ptr 288 ksM
    pokeMat3Oracle Std430 ptr 320 ksN
    pokeByteOff ptr 416 ksO
    pokeByteOff ptr 420 ksP
    pokeMat4Oracle Std430 ptr 424 ksQ
    pokeByteOff ptr 456 ksR
    pokeMat4Oracle Std430 ptr 464 ksS
    pokeByteOff ptr 528 ksT
    pokeMat2Oracle Std430 ptr 536 ksU
    pokeByteOff ptr 552 ksV

hprop_kitchensink_std430_round_trip :: Property
hprop_kitchensink_std430_round_trip = property do
  val <- forAll genKitchenSink
  let sut = Strided @Std430 val
      oracle = KitchenSinkStd430 val
  ptrTripping sut oracle

-- Oracle for scalar layout
newtype KitchenSinkScalar = KitchenSinkScalar KitchenSink
  deriving (Generic, Show, Eq)

instance Storable KitchenSinkScalar where
  sizeOf _ = 440
  alignment _ = 8 -- Based on alignment of dvec3
  peek ptr =
    KitchenSinkScalar
      <$> ( KitchenSink
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 8
              <*> peekByteOff ptr 32
              <*> peekByteOff ptr 36
              <*> peekByteOff ptr 72
              <*> peekByteOff ptr 76
              <*> peekByteOff ptr 112
              <*> peekByteOff ptr 114
              <*> peekByteOff ptr 126
              <*> peekByteOff ptr 128
              <*> peekByteOff ptr 140
              <*> peekByteOff ptr 144
              <*> peekByteOff ptr 192
              <*> peekByteOff ptr 200
              <*> peekByteOff ptr 296
              <*> peekByteOff ptr 300
              <*> peekByteOff ptr 302
              <*> peekByteOff ptr 336
              <*> peekByteOff ptr 344
              <*> peekByteOff ptr 408
              <*> peekByteOff ptr 416
              <*> peekByteOff ptr 432
          )
  poke ptr (KitchenSinkScalar KitchenSink{..}) = do
    pokeByteOff ptr 0 ksA
    pokeByteOff ptr 8 ksB
    pokeByteOff ptr 32 ksC
    pokeByteOff ptr 36 ksD
    pokeByteOff ptr 72 ksE
    pokeByteOff ptr 76 ksF
    pokeByteOff ptr 112 ksG
    pokeByteOff ptr 114 ksH
    pokeByteOff ptr 126 ksI
    pokeByteOff ptr 128 ksJ
    pokeByteOff ptr 140 ksK
    pokeByteOff ptr 144 ksL
    pokeByteOff ptr 192 ksM
    pokeByteOff ptr 200 ksN
    pokeByteOff ptr 296 ksO
    pokeByteOff ptr 300 ksP
    pokeByteOff ptr 302 ksQ
    pokeByteOff ptr 336 ksR
    pokeByteOff ptr 344 ksS
    pokeByteOff ptr 408 ksT
    pokeByteOff ptr 416 ksU
    pokeByteOff ptr 432 ksV

hprop_kitchensink_scalar_round_trip :: Property
hprop_kitchensink_scalar_round_trip = property do
  val <- forAll genKitchenSink
  let sut = Strided @Scalar val
      oracle = KitchenSinkScalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- PaddingTest
--------------------------------------------------------------------------------

data PaddingTest = PaddingTest
  { f :: Float
  , v :: V4 Float
  }
  deriving (Generic, Show, Eq)

genPaddingTest :: Gen PaddingTest
genPaddingTest = PaddingTest <$> genFloat <*> genV genFloat

instance AlignedStorable Std140 PaddingTest
instance AlignedStorable Std430 PaddingTest
instance AlignedStorable Scalar PaddingTest

newtype PaddingTestStd140 = PaddingTestStd140 PaddingTest
  deriving (Generic, Show, Eq)

instance Storable PaddingTestStd140 where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = PaddingTestStd140 <$> (PaddingTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 16)
  poke ptr (PaddingTestStd140 PaddingTest{..}) = do
    pokeByteOff ptr 0 f
    pokeByteOff ptr 16 v

unit_paddingtest_std140_packed_size :: Assertion
unit_paddingtest_std140_packed_size =
  packedAlignedSizeOf @Std140 @PaddingTest Proxy Proxy @?= 32

hprop_paddingtest_std140_round_trip :: Property
hprop_paddingtest_std140_round_trip = property do
  val <- forAll genPaddingTest
  let sut = Strided @Std140 val
  let oracle = PaddingTestStd140 val
  ptrTripping sut oracle

-- Oracle for Std430 layout.
-- Layout is identical to Std140 for this struct, but we define a separate
-- oracle to maintain a 1-to-1 mapping between tests and layouts.
newtype PaddingTestStd430 = PaddingTestStd430 PaddingTest
  deriving (Generic, Show, Eq)

instance Storable PaddingTestStd430 where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = PaddingTestStd430 <$> (PaddingTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 16)
  poke ptr (PaddingTestStd430 PaddingTest{..}) = do
    pokeByteOff ptr 0 f
    pokeByteOff ptr 16 v

unit_paddingtest_std430_packed_size :: Assertion
unit_paddingtest_std430_packed_size =
  packedAlignedSizeOf @Std430 @PaddingTest Proxy Proxy @?= 32

hprop_paddingtest_std430_round_trip :: Property
hprop_paddingtest_std430_round_trip = property do
  val <- forAll genPaddingTest
  let sut = Strided @Std430 val
  let oracle = PaddingTestStd430 val
  ptrTripping sut oracle

newtype PaddingTestScalar = PaddingTestScalar PaddingTest
  deriving (Generic, Show, Eq)

instance Storable PaddingTestScalar where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr = PaddingTestScalar <$> (PaddingTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 4)
  poke ptr (PaddingTestScalar PaddingTest{..}) = do
    pokeByteOff ptr 0 f
    pokeByteOff ptr 4 v

unit_paddingtest_scalar_packed_size :: Assertion
unit_paddingtest_scalar_packed_size =
  packedAlignedSizeOf @Scalar @PaddingTest Proxy Proxy @?= 20

hprop_paddingtest_scalar_round_trip :: Property
hprop_paddingtest_scalar_round_trip = property do
  val <- forAll genPaddingTest
  let sut = Strided @Scalar val
  let oracle = PaddingTestScalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- Std140Test
--------------------------------------------------------------------------------

data Std140Test = Std140Test
  { v3 :: V3 Float
  , f1 :: Float
  }
  deriving (Generic, Show, Eq)

genStd140Test :: Gen Std140Test
genStd140Test =
  Std140Test
    <$> (V3 <$> genFloat <*> genFloat <*> genFloat)
    <*> genFloat

instance AlignedStorable Std140 Std140Test
instance AlignedStorable Std430 Std140Test
instance AlignedStorable Scalar Std140Test

newtype Std140TestStd140 = Std140TestStd140 Std140Test
  deriving (Generic, Show, Eq)

instance Storable Std140TestStd140 where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = Std140TestStd140 <$> (Std140Test <$> peekByteOff ptr 0 <*> peekByteOff ptr 12)
  poke ptr (Std140TestStd140 Std140Test{..}) = do
    pokeByteOff ptr 0 v3
    pokeByteOff ptr 12 f1

unit_std140test_std140_packed_size :: Assertion
unit_std140test_std140_packed_size =
  packedAlignedSizeOf @Std140 @Std140Test Proxy Proxy @?= 16

hprop_std140test_std140_round_trip :: Property
hprop_std140test_std140_round_trip = property do
  val <- forAll genStd140Test
  let sut = Strided @Std140 val
      oracle = Std140TestStd140 val
  ptrTripping sut oracle

-- Oracle for Std430 layout. For this type, it is identical to std140.
newtype Std140TestStd430 = Std140TestStd430 Std140Test
  deriving (Generic, Show, Eq)

instance Storable Std140TestStd430 where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = Std140TestStd430 <$> (Std140Test <$> peekByteOff ptr 0 <*> peekByteOff ptr 12)
  poke ptr (Std140TestStd430 Std140Test{..}) = do
    pokeByteOff ptr 0 v3
    pokeByteOff ptr 12 f1

unit_std140test_std430_packed_size :: Assertion
unit_std140test_std430_packed_size =
  packedAlignedSizeOf @Std430 @Std140Test Proxy Proxy @?= 16

hprop_std140test_std430_round_trip :: Property
hprop_std140test_std430_round_trip = property do
  val <- forAll genStd140Test
  let sut = Strided @Std430 val
  let oracle = Std140TestStd430 val
  ptrTripping sut oracle

newtype Std140TestScalar = Std140TestScalar Std140Test
  deriving (Generic, Show, Eq)

instance Storable Std140TestScalar where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = Std140TestScalar <$> (Std140Test <$> peekByteOff ptr 0 <*> peekByteOff ptr 12)
  poke ptr (Std140TestScalar Std140Test{..}) = do
    pokeByteOff ptr 0 v3
    pokeByteOff ptr 12 f1

unit_std140test_scalar_packed_size :: Assertion
unit_std140test_scalar_packed_size =
  packedAlignedSizeOf @Scalar @Std140Test Proxy Proxy @?= 16

hprop_std140test_scalar_round_trip :: Property
hprop_std140test_scalar_round_trip = property do
  val <- forAll genStd140Test
  let sut = Strided @Scalar val
  let oracle = Std140TestScalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- ComplexPadding
--------------------------------------------------------------------------------

-- |
--   complexpaddingA : float (size=4, align=4); tSize=4
--   complexpaddingB : vec3 (size=12, align=16); tSize=16+12=28; align to 16, start at 16
--   complexpaddingC : mat3 (size=3*16=48, align=16); tSize=32+48=80; align to 16, start at 32
--   complexpaddingD : float (size=4, align=4); tSize=80+4=4; align to 4, start at 80
--   complexpaddingE : mat4 (size=4*4=64, align=16); tSize=96+64=160; align to 16, start at 96
--   complexpaddingF : float (size=4, align=4); tSize=160+4=164; align to 4, start at 160
--
--   overall size: alignTo 16 164
--               = 176
data ComplexPadding = ComplexPadding
  { complexpaddingA :: Float
  , complexpaddingB :: V3 Float
  , complexpaddingC :: M33 Float
  , complexpaddingD :: Float
  , complexpaddingE :: M44 Float
  , complexpaddingF :: Float
  }
  deriving (Generic, Show, Eq)

-- >>> alignedSizeOf (Data.Proxy.Proxy @Std430) (Data.Proxy.Proxy @ComplexPadding)
-- 176

genComplexPadding :: Gen ComplexPadding
genComplexPadding =
  ComplexPadding
    <$> genFloat
    <*> genV genFloat
    <*> genMat genFloat
    <*> genFloat
    <*> genMat genFloat
    <*> genFloat

instance AlignedStorable Std140 ComplexPadding
instance AlignedStorable Std430 ComplexPadding
instance AlignedStorable Scalar ComplexPadding

newtype ComplexPaddingStd140 = ComplexPaddingStd140 ComplexPadding
  deriving (Generic, Show, Eq)

instance Storable ComplexPaddingStd140 where
  sizeOf _ = 176
  alignment _ = 16
  peek ptr =
    ComplexPaddingStd140
      <$> ( ComplexPadding
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 16
              <*> peekMat3Oracle Std140 ptr 32
              <*> peekByteOff ptr 80
              <*> peekByteOff ptr 96
              <*> peekByteOff ptr 160
          )
  poke ptr (ComplexPaddingStd140 ComplexPadding{..}) = do
    pokeByteOff ptr 0 complexpaddingA
    pokeByteOff ptr 16 complexpaddingB
    pokeMat3Oracle Std140 ptr 32 complexpaddingC
    pokeByteOff ptr 80 complexpaddingD
    pokeByteOff ptr 96 complexpaddingE
    pokeByteOff ptr 160 complexpaddingF

unit_complexpadding_std140_packed_size :: Assertion
unit_complexpadding_std140_packed_size =
  packedAlignedSizeOf @Std140 @ComplexPadding Proxy Proxy @?= 164

hprop_complexpadding_std140_round_trip :: Property
hprop_complexpadding_std140_round_trip = property do
  complexpadding <- forAll genComplexPadding
  let sut = Strided @Std140 complexpadding
      oracle = ComplexPaddingStd140 complexpadding
  ptrTripping sut oracle

newtype ComplexPaddingStd430 = ComplexPaddingStd430 ComplexPadding
  deriving (Generic, Show, Eq)

instance Storable ComplexPaddingStd430 where
  sizeOf _ = 176
  alignment _ = 16
  peek ptr =
    ComplexPaddingStd430
      <$> ( ComplexPadding
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 16
              <*> peekMat3Oracle Std430 ptr 32
              <*> peekByteOff ptr 80
              <*> peekByteOff ptr 96
              <*> peekByteOff ptr 160
          )
  poke ptr (ComplexPaddingStd430 ComplexPadding{..}) = do
    pokeByteOff ptr 0 complexpaddingA
    pokeByteOff ptr 16 complexpaddingB
    pokeMat3Oracle Std430 ptr 32 complexpaddingC
    pokeByteOff ptr 80 complexpaddingD
    pokeByteOff ptr 96 complexpaddingE
    pokeByteOff ptr 160 complexpaddingF

unit_complexpadding_std430_packed_size :: Assertion
unit_complexpadding_std430_packed_size =
  packedAlignedSizeOf @Std430 @ComplexPadding Proxy Proxy @?= 164

hprop_complexpadding_std430_round_trip :: Property
hprop_complexpadding_std430_round_trip = property do
  val <- forAll genComplexPadding
  let sut = Strided @Std430 val
  let oracle = ComplexPaddingStd430 val
  ptrTripping sut oracle

newtype ComplexPaddingScalar = ComplexPaddingScalar ComplexPadding
  deriving (Generic, Show, Eq)

instance Storable ComplexPaddingScalar where
  sizeOf _ = 124
  alignment _ = 4
  peek ptr =
    ComplexPaddingScalar
      <$> ( ComplexPadding
              <$> peekByteOff ptr 0
              <*> peekByteOff ptr 4
              <*> peekByteOff ptr 16
              <*> peekByteOff ptr 52
              <*> peekByteOff ptr 56
              <*> peekByteOff ptr 120
          )
  poke ptr (ComplexPaddingScalar ComplexPadding{..}) = do
    pokeByteOff ptr 0 complexpaddingA
    pokeByteOff ptr 4 complexpaddingB
    pokeByteOff ptr 16 complexpaddingC
    pokeByteOff ptr 52 complexpaddingD
    pokeByteOff ptr 56 complexpaddingE
    pokeByteOff ptr 120 complexpaddingF

unit_complexpadding_scalar_packed_size :: Assertion
unit_complexpadding_scalar_packed_size =
  packedAlignedSizeOf @Scalar @ComplexPadding Proxy Proxy @?= 124

hprop_complexpadding_scalar_round_trip :: Property
hprop_complexpadding_scalar_round_trip = property do
  val <- forAll genComplexPadding
  let sut = Strided @Scalar val
  let oracle = ComplexPaddingScalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- Vertex
--------------------------------------------------------------------------------

data Vertex = Vertex
  { position :: V3 Float
  , normal :: V3 Float
  , uv :: V2 Float
  }
  deriving (Generic, Show, Eq)

genVertex :: Gen Vertex
genVertex = Vertex <$> genV genFloat <*> genV genFloat <*> genV genFloat

instance AlignedStorable Std140 Vertex
instance AlignedStorable Std430 Vertex
instance AlignedStorable Scalar Vertex

newtype VertexStd140 = VertexStd140 Vertex
  deriving (Generic, Show, Eq)

instance Storable VertexStd140 where
  sizeOf _ = 48
  alignment _ = 16
  peek ptr = VertexStd140 <$> (Vertex <$> peekByteOff ptr 0 <*> peekByteOff ptr 16 <*> peekByteOff ptr 32)
  poke ptr (VertexStd140 Vertex{..}) = do
    pokeByteOff ptr 0 position
    pokeByteOff ptr 16 normal
    pokeByteOff ptr 32 uv

unit_vertex_std140_packed_size :: Assertion
unit_vertex_std140_packed_size =
  packedAlignedSizeOf @Std140 @Vertex Proxy Proxy @?= 40

hprop_vertex_std140_round_trip :: Property
hprop_vertex_std140_round_trip = property do
  val <- forAll genVertex
  let sut = Strided @Std140 val
      oracle = VertexStd140 val
  ptrTripping sut oracle

newtype VertexStd430 = VertexStd430 Vertex
  deriving (Generic, Show, Eq)

instance Storable VertexStd430 where
  sizeOf _ = 48
  alignment _ = 16
  peek ptr = VertexStd430 <$> (Vertex <$> peekByteOff ptr 0 <*> peekByteOff ptr 16 <*> peekByteOff ptr 32)
  poke ptr (VertexStd430 Vertex{..}) = do
    pokeByteOff ptr 0 position
    pokeByteOff ptr 16 normal
    pokeByteOff ptr 32 uv

unit_vertex_std430_packed_size :: Assertion
unit_vertex_std430_packed_size =
  packedAlignedSizeOf @Std430 @Vertex Proxy Proxy @?= 40

hprop_vertex_std430_round_trip :: Property
hprop_vertex_std430_round_trip = property do
  val <- forAll genVertex
  let sut = Strided @Std430 val
      oracle = VertexStd430 val
  ptrTripping sut oracle

newtype VertexScalar = VertexScalar Vertex
  deriving (Generic, Show, Eq)

instance Storable VertexScalar where
  sizeOf _ = 32
  alignment _ = 4
  peek ptr = VertexScalar <$> (Vertex <$> peekByteOff ptr 0 <*> peekByteOff ptr 12 <*> peekByteOff ptr 24)
  poke ptr (VertexScalar Vertex{..}) = do
    pokeByteOff ptr 0 position
    pokeByteOff ptr 12 normal
    pokeByteOff ptr 24 uv

unit_vertex_scalar_packed_size :: Assertion
unit_vertex_scalar_packed_size =
  packedAlignedSizeOf @Scalar @Vertex Proxy Proxy @?= 32

hprop_vertex_scalar_round_trip :: Property
hprop_vertex_scalar_round_trip = property do
  val <- forAll genVertex
  let sut = Strided @Scalar val
      oracle = VertexScalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- Uniforms
--------------------------------------------------------------------------------

data Uniforms = Uniforms
  { modelView :: V4 (V4 Float)
  , cameraPos :: V4 Float
  }
  deriving (Generic, Show, Eq)

genUniforms :: Gen Uniforms
genUniforms = Uniforms <$> genMat genFloat <*> genV genFloat

instance AlignedStorable Std140 Uniforms
instance AlignedStorable Std430 Uniforms
instance AlignedStorable Scalar Uniforms

newtype UniformsStd140 = UniformsStd140 Uniforms
  deriving (Generic, Show, Eq)

instance Storable UniformsStd140 where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr = UniformsStd140 <$> (Uniforms <$> peekByteOff ptr 0 <*> peekByteOff ptr 64)
  poke ptr (UniformsStd140 Uniforms{..}) = do
    pokeByteOff ptr 0 modelView
    pokeByteOff ptr 64 cameraPos

hprop_uniforms_std140_round_trip :: Property
hprop_uniforms_std140_round_trip = property do
  val <- forAll genUniforms
  let sut = Strided @Std140 val
  let oracle = UniformsStd140 val
  ptrTripping sut oracle

newtype UniformsStd430 = UniformsStd430 Uniforms
  deriving (Generic, Show, Eq)

instance Storable UniformsStd430 where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr = UniformsStd430 <$> (Uniforms <$> peekByteOff ptr 0 <*> peekByteOff ptr 64)
  poke ptr (UniformsStd430 Uniforms{..}) = do
    pokeByteOff ptr 0 modelView
    pokeByteOff ptr 64 cameraPos

hprop_uniforms_std430_round_trip :: Property
hprop_uniforms_std430_round_trip = property do
  val <- forAll genUniforms
  let sut = Strided @Std430 val
  let oracle = UniformsStd430 val
  ptrTripping sut oracle

newtype UniformsScalar = UniformsScalar Uniforms
  deriving (Generic, Show, Eq)

instance Storable UniformsScalar where
  sizeOf _ = 80
  alignment _ = 4
  peek ptr = UniformsScalar <$> (Uniforms <$> peekByteOff ptr 0 <*> peekByteOff ptr 64)
  poke ptr (UniformsScalar Uniforms{..}) = do
    pokeByteOff ptr 0 modelView
    pokeByteOff ptr 64 cameraPos

hprop_uniforms_scalar_round_trip :: Property
hprop_uniforms_scalar_round_trip = property do
  val <- forAll genUniforms
  let sut = Strided @Scalar val
  let oracle = UniformsScalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- FinalPaddingTest
--------------------------------------------------------------------------------

data FinalPaddingTest = FinalPaddingTest
  { fptA :: V4 Float
  , fptB :: Int32
  }
  deriving (Generic, Show, Eq)

genFinalPaddingTest :: Gen FinalPaddingTest
genFinalPaddingTest = FinalPaddingTest <$> genV genFloat <*> genInt

instance AlignedStorable Std140 FinalPaddingTest
instance AlignedStorable Std430 FinalPaddingTest
instance AlignedStorable Scalar FinalPaddingTest

newtype FinalPaddingTestStd140 = FinalPaddingTestStd140 FinalPaddingTest
  deriving (Generic, Show, Eq)

instance Storable FinalPaddingTestStd140 where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = FinalPaddingTestStd140 <$> (FinalPaddingTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 16)
  poke ptr (FinalPaddingTestStd140 FinalPaddingTest{..}) = do
    pokeByteOff ptr 0 fptA
    pokeByteOff ptr 16 fptB

hprop_finalpaddingtest_std140_round_trip :: Property
hprop_finalpaddingtest_std140_round_trip = property do
  val <- forAll genFinalPaddingTest
  let sut = Strided @Std140 val
  let oracle = FinalPaddingTestStd140 val
  ptrTripping sut oracle

newtype FinalPaddingTestStd430 = FinalPaddingTestStd430 FinalPaddingTest
  deriving (Generic, Show, Eq)

instance Storable FinalPaddingTestStd430 where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = FinalPaddingTestStd430 <$> (FinalPaddingTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 16)
  poke ptr (FinalPaddingTestStd430 FinalPaddingTest{..}) = do
    pokeByteOff ptr 0 fptA
    pokeByteOff ptr 16 fptB

hprop_finalpaddingtest_std430_round_trip :: Property
hprop_finalpaddingtest_std430_round_trip = property do
  val <- forAll genFinalPaddingTest
  let sut = Strided @Std430 val
  let oracle = FinalPaddingTestStd430 val
  ptrTripping sut oracle

newtype FinalPaddingTestScalar = FinalPaddingTestScalar FinalPaddingTest
  deriving (Generic, Show, Eq)

instance Storable FinalPaddingTestScalar where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr = FinalPaddingTestScalar <$> (FinalPaddingTest <$> peekByteOff ptr 0 <*> peekByteOff ptr 16)
  poke ptr (FinalPaddingTestScalar FinalPaddingTest{..}) = do
    pokeByteOff ptr 0 fptA
    pokeByteOff ptr 16 fptB

hprop_finalpaddingtest_scalar_round_trip :: Property
hprop_finalpaddingtest_scalar_round_trip = property do
  val <- forAll genFinalPaddingTest
  let sut = Strided @Scalar val
  let oracle = FinalPaddingTestScalar val
  ptrTripping sut oracle

data Nested = Nested
  { nestedV3 :: V3 Float
  , nestedSimple :: Simple
  , nestedMixed :: MixedAlign
  , nestedBoolTest :: BoolTest
  }
  deriving (Generic, Show, Eq)

genNested :: Gen Nested
genNested =
  Nested
    <$> genV genFloat
    <*> genSimple
    <*> genMixedAlign
    <*> genBoolTest

instance AlignedStorable Std140 Nested
instance AlignedStorable Std430 Nested
instance AlignedStorable Scalar Nested

newtype NestedStd140 = NestedStd140 Nested
  deriving (Generic, Show, Eq)

instance Storable NestedStd140 where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    NestedStd140
      <$> ( Nested
              <$> peekByteOff ptr 0
              <*> (coerce <$> peekByteOff @(Strided Std140 Simple) ptr 16)
              <*> (coerce <$> peekByteOff @(Strided Std140 MixedAlign) ptr 32)
              <*> (coerce <$> peekByteOff @(Strided Std140 BoolTest) ptr 64)
          )
  poke ptr (NestedStd140 Nested{..}) = do
    pokeByteOff ptr 0 nestedV3
    pokeByteOff ptr 16 (Strided @Std140 nestedSimple)
    pokeByteOff ptr 32 (Strided @Std140 nestedMixed)
    pokeByteOff ptr 64 (Strided @Std140 nestedBoolTest)

hprop_nested_std140_round_trip :: Property
hprop_nested_std140_round_trip = property do
  val <- forAll genNested
  let sut = Strided @Std140 val
      oracle = NestedStd140 val
  ptrTripping sut oracle

newtype NestedStd430 = NestedStd430 Nested
  deriving (Generic, Show, Eq)

instance Storable NestedStd430 where
  sizeOf _ = 64
  alignment _ = 16
  peek ptr =
    NestedStd430
      <$> ( Nested
              <$> peekByteOff ptr 0
              <*> (coerce <$> peekByteOff @(Strided Std430 Simple) ptr 12)
              <*> (coerce <$> peekByteOff @(Strided Std430 MixedAlign) ptr 24)
              <*> (coerce <$> peekByteOff @(Strided Std430 BoolTest) ptr 48)
          )
  poke ptr (NestedStd430 Nested{..}) = do
    pokeByteOff ptr 0 nestedV3
    pokeByteOff ptr 12 (Strided @Std430 nestedSimple)
    pokeByteOff ptr 24 (Strided @Std430 nestedMixed)
    pokeByteOff ptr 48 (Strided @Std430 nestedBoolTest)

hprop_nested_std430_round_trip :: Property
hprop_nested_std430_round_trip = property do
  val <- forAll genNested
  let sut = Strided @Std430 val
      oracle = NestedStd430 val
  ptrTripping sut oracle

newtype NestedScalar = NestedScalar Nested
  deriving (Generic, Show, Eq)

instance Storable NestedScalar where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr =
    NestedScalar
      <$> ( Nested
              <$> peekByteOff ptr 0
              <*> (coerce <$> peekByteOff @(Strided Scalar Simple) ptr 12)
              <*> (coerce <$> peekByteOff @(Strided Scalar MixedAlign) ptr 24)
              <*> (coerce <$> peekByteOff @(Strided Scalar BoolTest) ptr 44)
          )
  poke ptr (NestedScalar Nested{..}) = do
    pokeByteOff ptr 0 nestedV3
    pokeByteOff ptr 12 (Strided @Scalar nestedSimple)
    pokeByteOff ptr 24 (Strided @Scalar nestedMixed)
    pokeByteOff ptr 44 (Strided @Scalar nestedBoolTest)

hprop_nested_scalar_round_trip :: Property
hprop_nested_scalar_round_trip = property do
  val <- forAll genNested
  let sut = Strided @Scalar val
      oracle = NestedScalar val
  ptrTripping sut oracle

newtype NestedNewtype = NestedNewtype Nested
  deriving (Generic, Show, Eq)

genNestedNewtype :: Gen NestedNewtype
genNestedNewtype = NestedNewtype <$> genNested

instance AlignedStorable Std140 NestedNewtype
instance AlignedStorable Std430 NestedNewtype
instance AlignedStorable Scalar NestedNewtype

newtype NestedNewtypeStd140 = NestedNewtypeStd140 NestedNewtype
  deriving (Generic, Show, Eq)

instance Storable NestedNewtypeStd140 where
  sizeOf _ = sizeOf @(Strided Std140 Nested) undefined
  alignment _ = alignment @(Strided Std140 Nested) undefined
  peek ptr = NestedNewtypeStd140 . coerce <$> peek @(Strided Std140 Nested) (castPtr ptr)
  poke ptr (NestedNewtypeStd140 nested) = poke @(Strided Std140 Nested) (castPtr ptr) (coerce nested)

hprop_nested_newtype_std140_round_trip :: Property
hprop_nested_newtype_std140_round_trip = property do
  val <- forAll genNestedNewtype
  let sut = Strided @Std140 val
      oracle = NestedNewtypeStd140 val
  ptrTripping sut oracle

newtype NestedNewtypeStd430 = NestedNewtypeStd430 NestedNewtype
  deriving (Generic, Show, Eq)

instance Storable NestedNewtypeStd430 where
  sizeOf _ = sizeOf @(Strided Std430 Nested) undefined
  alignment _ = alignment @(Strided Std430 Nested) undefined
  peek ptr = NestedNewtypeStd430 . coerce <$> peek @(Strided Std430 Nested) (castPtr ptr)
  poke ptr (NestedNewtypeStd430 nested) = poke @(Strided Std430 Nested) (castPtr ptr) (coerce nested)

hprop_nested_newtype_std430_round_trip :: Property
hprop_nested_newtype_std430_round_trip = property do
  val <- forAll genNestedNewtype
  let sut = Strided @Std430 val
      oracle = NestedNewtypeStd430 val
  ptrTripping sut oracle

newtype NestedNewtypeScalar = NestedNewtypeScalar NestedNewtype
  deriving (Generic, Show, Eq)

instance Storable NestedNewtypeScalar where
  sizeOf _ = sizeOf @(Strided Scalar Nested) undefined
  alignment _ = alignment @(Strided Scalar Nested) undefined
  peek ptr =
    NestedNewtypeScalar . coerce <$> peek @(Strided Scalar Nested) (castPtr ptr)
  poke ptr (NestedNewtypeScalar nested) = poke @(Strided Scalar Nested) (castPtr ptr) (coerce nested)

hprop_nested_newtype_scalar_round_trip :: Property
hprop_nested_newtype_scalar_round_trip = property do
  val <- forAll genNestedNewtype
  let sut = Strided @Scalar val
      oracle = NestedNewtypeScalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- DoubleTrouble
--------------------------------------------------------------------------------

data DoubleTrouble = DoubleTrouble
  { dtD0 :: Double
  , dtM0 :: M33 Double
  , dtF0 :: Float
  , dtV0 :: V3 Double
  , dtF1 :: Float
  , dtV1 :: V4 Double
  , dtF2 :: Float
  , dtM1 :: M44 Double
  }
  deriving (Generic, Show, Eq)

genDoubleTrouble :: Gen DoubleTrouble
genDoubleTrouble =
  DoubleTrouble
    <$> genFloat
    <*> genMat genFloat
    <*> genFloat
    <*> genV genFloat
    <*> genFloat
    <*> genV genFloat
    <*> genFloat
    <*> genMat genFloat

instance AlignedStorable Std140 DoubleTrouble
instance AlignedStorable Std430 DoubleTrouble
instance AlignedStorable Scalar DoubleTrouble

newtype DoubleTroubleStd140 = DoubleTroubleStd140 DoubleTrouble
  deriving (Generic, Show, Eq)

instance Storable DoubleTroubleStd140 where
  sizeOf _ = 384
  alignment _ = 32
  peek ptr =
    DoubleTroubleStd140
      <$> ( DoubleTrouble
              <$> peekByteOff ptr 0
              <*> peekMat3Oracle Std140 ptr 32
              <*> peekByteOff ptr 128
              <*> peekByteOff ptr 160
              <*> peekByteOff ptr 184
              <*> peekByteOff ptr 192
              <*> peekByteOff ptr 224
              <*> peekByteOff ptr 256
          )
  poke ptr (DoubleTroubleStd140 DoubleTrouble{..}) = do
    pokeByteOff ptr 0 dtD0
    pokeMat3Oracle Std140 ptr 32 dtM0
    pokeByteOff ptr 128 dtF0
    pokeByteOff ptr 160 dtV0
    pokeByteOff ptr 184 dtF1
    pokeByteOff ptr 192 dtV1
    pokeByteOff ptr 224 dtF2
    pokeByteOff ptr 256 dtM1

hprop_doubletrouble_std140_round_trip :: Property
hprop_doubletrouble_std140_round_trip = property do
  val <- forAll genDoubleTrouble
  let sut = Strided @Std140 val
      oracle = DoubleTroubleStd140 val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- VectorStruct
--------------------------------------------------------------------------------

data VectorStruct = VectorStruct
  { vsA :: Float
  , vsB :: SSV.Vector 2 (V3 Float)
  , vsC :: Int32
  }
  deriving (Generic, Show, Eq)

genVectorStruct :: Gen VectorStruct
genVectorStruct =
  VectorStruct
    <$> genFloat
    <*> SSV.replicateM (genV genFloat)
    <*> genInt

instance AlignedStorable Std140 VectorStruct
instance AlignedStorable Std430 VectorStruct
instance AlignedStorable Scalar VectorStruct

newtype VectorStructStd140 = VectorStructStd140 VectorStruct
  deriving (Generic, Show, Eq)

instance Storable VectorStructStd140 where
  sizeOf _ = 64
  alignment _ = 16
  peek ptr =
    VectorStructStd140
      <$> ( VectorStruct
              <$> peekByteOff ptr 0
              <*> (coerce <$> peekByteOff @(Packed Std140 (SSV.Vector 2 (V3 Float))) ptr 16)
              <*> peekByteOff ptr 48
          )
  poke ptr (VectorStructStd140 VectorStruct{..}) = do
    pokeByteOff ptr 0 vsA
    pokeByteOff ptr 16 (Packed @Std140 vsB)
    pokeByteOff ptr 48 vsC

hprop_vectorstruct_std140_round_trip :: Property
hprop_vectorstruct_std140_round_trip = property do
  val <- forAll genVectorStruct
  let sut = Strided @Std140 val
      oracle = VectorStructStd140 val
  ptrTripping sut oracle

newtype VectorStructStd430 = VectorStructStd430 VectorStruct
  deriving (Generic, Show, Eq)

instance Storable VectorStructStd430 where
  sizeOf _ = 64
  alignment _ = 16
  peek ptr =
    VectorStructStd430
      <$> ( VectorStruct
              <$> peekByteOff ptr 0
              <*> (coerce <$> peekByteOff @(Packed Std430 (SSV.Vector 2 (V3 Float))) ptr 16)
              <*> peekByteOff ptr 48
          )
  poke ptr (VectorStructStd430 VectorStruct{..}) = do
    pokeByteOff ptr 0 vsA
    pokeByteOff ptr 16 (Packed @Std430 vsB)
    pokeByteOff ptr 48 vsC

hprop_vectorstruct_std430_round_trip :: Property
hprop_vectorstruct_std430_round_trip = property do
  val <- forAll genVectorStruct
  let sut = Strided @Std430 val
      oracle = VectorStructStd430 val
  ptrTripping sut oracle

newtype VectorStructScalar = VectorStructScalar VectorStruct
  deriving (Generic, Show, Eq)

instance Storable VectorStructScalar where
  sizeOf _ = 32
  alignment _ = 4
  peek ptr =
    VectorStructScalar
      <$> ( VectorStruct
              <$> peekByteOff ptr 0
              <*> (coerce <$> peekByteOff @(Packed Scalar (SSV.Vector 2 (V3 Float))) ptr 4)
              <*> peekByteOff ptr 28
          )
  poke ptr (VectorStructScalar VectorStruct{..}) = do
    pokeByteOff ptr 0 vsA
    pokeByteOff ptr 4 (Packed @Scalar vsB)
    pokeByteOff ptr 28 vsC

hprop_vectorstruct_scalar_round_trip :: Property
hprop_vectorstruct_scalar_round_trip = property do
  val <- forAll genVectorStruct
  let sut = Strided @Scalar val
      oracle = VectorStructScalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- ComplexVectorStruct
--------------------------------------------------------------------------------

data ComplexVectorStruct = ComplexVectorStruct
  { cvsA :: Float
  , cvsB :: SSV.Vector 3 (V3 Word16)
  , cvsC :: SSV.Vector 2 (V2 Double)
  , cvsD :: Float
  , cvsE :: Double
  }
  deriving (Generic, Show, Eq)

genComplexVectorStruct :: Gen ComplexVectorStruct
genComplexVectorStruct =
  ComplexVectorStruct
    <$> genFloat
    <*> SSV.replicateM (genV genInt)
    <*> SSV.replicateM (genV genFloat)
    <*> genFloat
    <*> genFloat

instance AlignedStorable Std140 ComplexVectorStruct
instance AlignedStorable Std430 ComplexVectorStruct
instance AlignedStorable Scalar ComplexVectorStruct

newtype ComplexVectorStructStd140 = ComplexVectorStructStd140 ComplexVectorStruct
  deriving (Generic, Show, Eq)

instance Storable ComplexVectorStructStd140 where
  sizeOf _ = 112
  alignment _ = 16
  peek ptr =
    ComplexVectorStructStd140
      <$> ( ComplexVectorStruct
              <$> peekByteOff ptr 0
              <*> (coerce <$> peekByteOff @(Packed Std140 (SSV.Vector 3 (V3 Word16))) ptr 16)
              <*> (coerce <$> peekByteOff @(Packed Std140 (SSV.Vector 2 (V2 Double))) ptr 64)
              <*> peekByteOff ptr 96
              <*> peekByteOff ptr 104
          )
  poke ptr (ComplexVectorStructStd140 ComplexVectorStruct{..}) = do
    pokeByteOff ptr 0 cvsA
    pokeByteOff ptr 16 (Packed @Std140 cvsB)
    pokeByteOff ptr 64 (Packed @Std140 cvsC)
    pokeByteOff ptr 96 cvsD
    pokeByteOff ptr 104 cvsE

hprop_complexvectorstruct_std140_round_trip :: Property
hprop_complexvectorstruct_std140_round_trip = property do
  val <- forAll genComplexVectorStruct
  let sut = Strided @Std140 val
      oracle = ComplexVectorStructStd140 val
  ptrTripping sut oracle

newtype ComplexVectorStructStd430 = ComplexVectorStructStd430 ComplexVectorStruct
  deriving (Generic, Show, Eq)

instance Storable ComplexVectorStructStd430 where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    ComplexVectorStructStd430
      <$> ( ComplexVectorStruct
              <$> peekByteOff ptr 0
              <*> (coerce <$> peekByteOff @(Packed Std430 (SSV.Vector 3 (V3 Word16))) ptr 8)
              <*> (coerce <$> peekByteOff @(Packed Std430 (SSV.Vector 2 (V2 Double))) ptr 32)
              <*> peekByteOff ptr 64
              <*> peekByteOff ptr 72
          )
  poke ptr (ComplexVectorStructStd430 ComplexVectorStruct{..}) = do
    pokeByteOff ptr 0 cvsA
    pokeByteOff ptr 8 (Packed @Std430 cvsB)
    pokeByteOff ptr 32 (Packed @Std430 cvsC)
    pokeByteOff ptr 64 cvsD
    pokeByteOff ptr 72 cvsE

hprop_complexvectorstruct_std430_round_trip :: Property
hprop_complexvectorstruct_std430_round_trip = property do
  val <- forAll genComplexVectorStruct
  let sut = Strided @Std430 val
      oracle = ComplexVectorStructStd430 val
  ptrTripping sut oracle

newtype ComplexVectorStructScalar = ComplexVectorStructScalar ComplexVectorStruct
  deriving (Generic, Show, Eq)

instance Storable ComplexVectorStructScalar where
  sizeOf _ = 72
  alignment _ = 8
  peek ptr =
    ComplexVectorStructScalar
      <$> ( ComplexVectorStruct
              <$> peekByteOff ptr 0
              <*> (coerce <$> peekByteOff @(Packed Scalar (SSV.Vector 3 (V3 Word16))) ptr 4)
              <*> (coerce <$> peekByteOff @(Packed Scalar (SSV.Vector 2 (V2 Double))) ptr 24)
              <*> peekByteOff ptr 56
              <*> peekByteOff ptr 64
          )
  poke ptr (ComplexVectorStructScalar ComplexVectorStruct{..}) = do
    pokeByteOff ptr 0 cvsA
    pokeByteOff ptr 4 (Packed @Scalar cvsB)
    pokeByteOff ptr 24 (Packed @Scalar cvsC)
    pokeByteOff ptr 56 (Packed @Scalar cvsD)
    pokeByteOff ptr 64 (Packed @Scalar cvsE)

hprop_complexvectorstruct_scalar_round_trip :: Property
hprop_complexvectorstruct_scalar_round_trip = property do
  val <- forAll genComplexVectorStruct
  let sut = Strided @Scalar val
      oracle = ComplexVectorStructScalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- Insanity
--------------------------------------------------------------------------------

data Insanity = Insanity
  { ina :: Float
  , inb :: SV.Vector 7 ComplexVectorStruct
  , inc :: Word8
  , ind :: Word64
  , ine :: Word16
  , inf :: V3 Bool
  , ing :: SV.Vector 5 Nested
  , inh :: V3 Word16
  , ini :: SV.Vector 5 (SV.Vector 2 (SV.Vector 4 Nested))
  , inj :: Bool
  , ink :: V3 Double
  }
  deriving (Generic, Show, Eq)

instance AlignedStorable Std140 Insanity
instance AlignedStorable Std430 Insanity
instance AlignedStorable Scalar Insanity

genInsanity :: Gen Insanity
genInsanity =
  Insanity
    <$> genFloat
    <*> SV.replicateM genComplexVectorStruct
    <*> genInt
    <*> genInt
    <*> genInt
    <*> genV Gen.bool
    <*> SV.replicateM genNested
    <*> genV genInt
    <*> SV.replicateM (SV.replicateM (SV.replicateM genNested))
    <*> Gen.bool
    <*> genV genFloat

newtype InsanityStd140 = InsanityStd140 Insanity
  deriving (Generic, Show, Eq)

instance Storable InsanityStd140 where
  sizeOf _ = 4512
  alignment _ = 32

  peek ptr =
    InsanityStd140
      <$> ( Insanity
              <$> peekByteOff ptr 0
              <*> peekSizedVector (Proxy @ComplexVectorStructStd140) ptr 16
              <*> peekByteOff ptr 800
              <*> peekByteOff ptr 808
              <*> peekByteOff ptr 816
              <*> peekByteOff ptr 832
              <*> peekSizedVector (Proxy @NestedStd140) ptr 848
              <*> peekStd140U16Vec3OracleByteOff ptr 1248
              <*> peekSized3DVector (Proxy @NestedStd140) ptr 1264
              <*> peekByteOff ptr 4464
              <*> peekByteOff ptr 4480
          )

  poke ptr (InsanityStd140 Insanity{..}) = do
    pokeByteOff ptr 0 ina
    pokeSizedVector ptr 16 (SV.map ComplexVectorStructStd140 inb)
    pokeByteOff ptr 800 inc
    pokeByteOff ptr 808 ind
    pokeByteOff ptr 816 ine
    pokeByteOff ptr 832 inf
    pokeSizedVector ptr 848 (SV.map NestedStd140 ing)
    pokeStd140U16Vec3OracleByteOff ptr 1248 inh
    pokeSized3DVector (Proxy @NestedStd140) ptr 1264 ini
    pokeByteOff ptr 4464 inj
    pokeByteOff ptr 4480 ink

hprop_insanity_std140_round_trips :: Property
hprop_insanity_std140_round_trips = withShrinks 10 $ property do
  val <- forAll genInsanity
  let sut = Strided @Std140 val
      oracle = InsanityStd140 val
  ptrTripping sut oracle

newtype InsanityStd430 = InsanityStd430 Insanity
  deriving (Generic, Show, Eq)

instance Storable InsanityStd430 where
  sizeOf _ = 3584
  alignment _ = 32

  peek ptr =
    InsanityStd430
      <$> ( Insanity
              <$> peekByteOff ptr 0
              <*> peekSizedVector (Proxy @ComplexVectorStructStd430) ptr 16
              <*> peekByteOff ptr 576
              <*> peekByteOff ptr 584
              <*> peekByteOff ptr 592
              <*> peekByteOff ptr 608
              <*> peekSizedVector (Proxy @NestedStd430) ptr 624
              <*> peekStd430U16Vec3OracleByteOff ptr 944
              <*> peekSized3DVector (Proxy @NestedStd430) ptr 960
              <*> peekByteOff ptr 3520
              <*> peekByteOff ptr 3552
          )

  poke ptr (InsanityStd430 Insanity{..}) = do
    pokeByteOff ptr 0 ina
    pokeSizedVector ptr 16 (SV.map ComplexVectorStructStd430 inb)
    pokeByteOff ptr 576 inc
    pokeByteOff ptr 584 ind
    pokeByteOff ptr 592 ine
    pokeByteOff ptr 608 inf
    pokeSizedVector ptr 624 (SV.map NestedStd430 ing)
    pokeStd430U16Vec3OracleByteOff ptr 944 inh
    pokeSized3DVector (Proxy @NestedStd430) ptr 960 ini
    pokeByteOff ptr 3520 inj
    pokeByteOff ptr 3552 ink

hprop_insanity_std430_round_trips :: Property
hprop_insanity_std430_round_trips = withShrinks 10 $ property do
  val <- forAll genInsanity
  let sut = Strided @Std430 val
      oracle = InsanityStd430 val
  ptrTripping sut oracle

newtype InsanityScalar = InsanityScalar Insanity
  deriving (Generic, Show, Eq)

instance Storable InsanityScalar where
  sizeOf _ = 3096
  alignment _ = 8

  peek ptr =
    InsanityScalar
      <$> ( Insanity
              <$> peekByteOff ptr 0
              <*> peekSizedVector (Proxy @ComplexVectorStructScalar) ptr 8
              <*> peekByteOff ptr 512
              <*> peekByteOff ptr 520
              <*> peekByteOff ptr 528
              <*> peekByteOff ptr 532
              <*> peekSizedVector (Proxy @NestedScalar) ptr 544
              <*> peekStd430U16Vec3OracleByteOff ptr 820
              <*> peekSized3DVector (Proxy @NestedScalar) ptr 832
              <*> peekByteOff ptr 3068
              <*> peekByteOff ptr 3072
          )

  poke ptr (InsanityScalar Insanity{..}) = do
    pokeByteOff ptr 0 ina
    pokeSizedVector ptr 8 (SV.map ComplexVectorStructScalar inb)
    pokeByteOff ptr 512 inc
    pokeByteOff ptr 520 ind
    pokeByteOff ptr 528 ine
    pokeByteOff ptr 532 inf
    pokeSizedVector ptr 544 (SV.map NestedScalar ing)
    pokeStd430U16Vec3OracleByteOff ptr 820 inh
    pokeSized3DVector (Proxy @NestedScalar) ptr 832 ini
    pokeByteOff ptr 3068 inj
    pokeByteOff ptr 3072 ink

hprop_insanity_scalar_round_trips :: Property
hprop_insanity_scalar_round_trips = withShrinks 10 $ property do
  val <- forAll genInsanity
  let sut = Strided @Scalar val
      oracle = InsanityScalar val
  ptrTripping sut oracle

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
class MatrixStride v a where
  matStride :: Proxy (v a) -> MemoryLayout -> Int

instance MatrixStride V2 Half where
  matStride _ = \case
    Std140 -> 16
    Std430 -> 4
    Scalar -> 4

instance MatrixStride V2 Float where
  matStride _ = \case
    Std140 -> 16
    Std430 -> 8
    Scalar -> 8

instance MatrixStride V2 Double where
  matStride _ = \case
    Std140 -> 16
    Std430 -> 16
    Scalar -> 8

instance MatrixStride V3 Half where
  matStride _ = \case
    Std140 -> 16
    Std430 -> 8
    Scalar -> 6

instance MatrixStride V3 Float where
  matStride _ = \case
    Std140 -> 16
    Std430 -> 16
    Scalar -> 12

instance MatrixStride V3 Double where
  matStride _ = \case
    Std140 -> 32
    Std430 -> 32
    Scalar -> 24

instance MatrixStride V4 Half where
  matStride _ = \case
    Std140 -> 16
    Std430 -> 8
    Scalar -> 8

instance MatrixStride V4 Float where
  matStride _ _ = 16

instance MatrixStride V4 Double where
  matStride _ _ = 32

peekMat2Oracle
  :: forall a b v. (MatrixStride v a, Storable (v a)) => MemoryLayout -> Ptr b -> Int -> IO (V2 (v a))
peekMat2Oracle layout ptr off =
  let stride = matStride (Proxy @(v a)) layout
   in V2 <$> peekByteOff ptr off <*> peekByteOff ptr (off + stride)

pokeMat2Oracle
  :: forall a b v
   . (MatrixStride v a, Storable (v a))
  => MemoryLayout
  -> Ptr b
  -> Int
  -> V2 (v a)
  -> IO ()
pokeMat2Oracle layout ptr off (V2 r1 r2) = do
  let stride = matStride (Proxy @(v a)) layout
  pokeByteOff ptr off r1
  pokeByteOff ptr (off + stride) r2

peekMat3Oracle
  :: forall a b v. (MatrixStride v a, Storable (v a)) => MemoryLayout -> Ptr b -> Int -> IO (V3 (v a))
peekMat3Oracle layout ptr off =
  let stride = matStride (Proxy @(v a)) layout
   in V3
        <$> peekByteOff ptr off
        <*> peekByteOff ptr (off + stride)
        <*> peekByteOff ptr (off + stride * 2)

pokeMat3Oracle
  :: forall a b v
   . (MatrixStride v a, Storable (v a))
  => MemoryLayout
  -> Ptr b
  -> Int
  -> V3 (v a)
  -> IO ()
pokeMat3Oracle layout ptr off (V3 r1 r2 r3) = do
  let stride = matStride (Proxy @(v a)) layout
  pokeByteOff ptr off r1
  pokeByteOff ptr (off + stride) r2
  pokeByteOff ptr (off + stride * 2) r3

peekMat4Oracle
  :: forall a b v. (MatrixStride v a, Storable (v a)) => MemoryLayout -> Ptr b -> Int -> IO (V4 (v a))
peekMat4Oracle layout ptr off =
  let stride = matStride (Proxy @(v a)) layout
   in V4
        <$> peekByteOff ptr off
        <*> peekByteOff ptr (off + stride)
        <*> peekByteOff ptr (off + stride * 2)
        <*> peekByteOff ptr (off + stride * 3)

pokeMat4Oracle
  :: forall a b v
   . (MatrixStride v a, Storable (v a))
  => MemoryLayout
  -> Ptr b
  -> Int
  -> V4 (v a)
  -> IO ()
pokeMat4Oracle layout ptr off (V4 r1 r2 r3 r4) = do
  let stride = matStride (Proxy @(v a)) layout
  pokeByteOff ptr off r1
  pokeByteOff ptr (off + stride) r2
  pokeByteOff ptr (off + stride * 2) r3
  pokeByteOff ptr (off + stride * 3) r4

peekStd140U16Vec3OracleByteOff :: (Storable a) => Ptr b -> Int -> IO (V3 a)
peekStd140U16Vec3OracleByteOff ptr off =
  V3
    <$> peekByteOff ptr off
    <*> peekByteOff ptr (off + 4)
    <*> peekByteOff ptr (off + 8)

peekStd140U16Mat3OracleByteOff :: Ptr b -> Int -> IO (M33 Word16)
peekStd140U16Mat3OracleByteOff ptr off =
  V3
    <$> peekStd140U16Vec3OracleByteOff ptr off
    <*> peekStd140U16Vec3OracleByteOff ptr (off + 16)
    <*> peekStd140U16Vec3OracleByteOff ptr (off + 32)

peekStd430U16Vec3OracleByteOff :: (Storable a) => Ptr b -> Int -> IO (V3 a)
peekStd430U16Vec3OracleByteOff ptr off =
  V3
    <$> peekByteOff ptr off
    <*> peekByteOff ptr (off + 2)
    <*> peekByteOff ptr (off + 4)

pokeStd140U16Vec3OracleByteOff :: Ptr b -> Int -> V3 Word16 -> IO ()
pokeStd140U16Vec3OracleByteOff ptr off (V3 x y z) = do
  pokeByteOff ptr off x
  pokeByteOff ptr (off + 4) y
  pokeByteOff ptr (off + 8) z

pokeStd140U16Mat3OracleByteOff :: Ptr b -> Int -> M33 Word16 -> IO ()
pokeStd140U16Mat3OracleByteOff ptr off (V3 r1 r2 r3) = do
  pokeStd140U16Vec3OracleByteOff ptr off r1
  pokeStd140U16Vec3OracleByteOff ptr (off + 16) r2
  pokeStd140U16Vec3OracleByteOff ptr (off + 32) r3

pokeStd430U16Vec3OracleByteOff :: Ptr b -> Int -> V3 Word16 -> IO ()
pokeStd430U16Vec3OracleByteOff ptr off (V3 x y z) = do
  pokeByteOff ptr off x
  pokeByteOff ptr (off + 2) y
  pokeByteOff ptr (off + 4) z
