{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Structs with matrices and their oracles
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Structs under test with GLSL matrices. t'Uniforms' and t'DUniforms' need only
-- @mat4@/@dmat4@; the others need the full 'FloatMats', 'DoubleMats' and
-- 'HalfMats' groups.
module Foreign.GPU.Storable.Aligned.Test.Sut.Matrix (
  Uniforms (Uniforms),
  genUniforms,
  uniformsSuite,
  ComplexPadding (ComplexPadding),
  genComplexPadding,
  complexPaddingSuite,
  D2 (D2),
  genD2,
  d2Suite,
  DoubleTrouble (DoubleTrouble),
  genDoubleTrouble,
  doubleTroubleSuite,
  DUniforms (DUniforms),
  genDUniforms,
  dUniformsSuite,
  TestNonSquare (TestNonSquare),
  genTestNonSquare,
  testNonSquareSuite,
  KitchenSink (KitchenSink),
  genKitchenSink,
  kitchenSinkSuite,
)
where

import Data.Int
import Data.Proxy
import Foreign
import Foreign.GPU.Storable.Aligned
import GHC.Generics
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Numeric.Half (Half)
import Test.Tasty (TestTree)

import Foreign.GPU.Storable.Aligned.Test.Glsl
import Foreign.GPU.Storable.Aligned.Test.Harness
import Foreign.GPU.Storable.Aligned.Test.Vocab

--------------------------------------------------------------------------------
-- Nonsquare
--------------------------------------------------------------------------------

-- | Needs to be documented, but Mnm -> matnxm
data TestNonSquare lib = TestNonSquare
  { matA :: F16Mat2x3 lib
  , matB :: DMat3x4 lib
  }
  deriving (Generic)

deriving instance (HalfMats lib, DoubleMats lib) => Show (TestNonSquare lib)

deriving instance (HalfMats lib, DoubleMats lib) => Eq (TestNonSquare lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (TestNonSquare lib)))
  => AlignedStorable l (TestNonSquare lib)

genTestNonSquare :: (HalfMats lib, DoubleMats lib) => Gen (TestNonSquare lib)
genTestNonSquare = TestNonSquare <$> genMat <*> genMat

-- GLSL: struct TestNonSquare (glsl/Conformance.glsl)
instance (HalfMats lib, DoubleMats lib) => Storable (Oracle Std140 (TestNonSquare lib)) where
  sizeOf _ = 192
  alignment _ = 32
  peek ptr =
    Oracle
      <$> (TestNonSquare <$> peekMatAt Std140 ptr 0 <*> peekMatAt Std140 ptr 64)
  poke ptr (Oracle TestNonSquare{..}) = do
    pokeMatAt Std140 ptr 0 matA
    pokeMatAt Std140 ptr 64 matB

-- GLSL: struct TestNonSquare (glsl/Conformance.glsl)
instance (HalfMats lib, DoubleMats lib) => Storable (Oracle Std430 (TestNonSquare lib)) where
  sizeOf _ = 160
  alignment _ = 32
  peek ptr =
    Oracle
      <$> (TestNonSquare <$> peekMatAt Std430 ptr 0 <*> peekMatAt Std430 ptr 32)
  poke ptr (Oracle TestNonSquare{..}) = do
    pokeMatAt Std430 ptr 0 matA
    pokeMatAt Std430 ptr 32 matB

-- GLSL: struct TestNonSquare (glsl/Conformance.glsl)
instance (HalfMats lib, DoubleMats lib) => Storable (Oracle Scalar (TestNonSquare lib)) where
  sizeOf _ = 112
  alignment _ = 8
  peek ptr = Oracle <$> (TestNonSquare <$> peekMatAt Scalar ptr 0 <*> peekMatAt Scalar ptr 16)
  poke ptr (Oracle TestNonSquare{..}) = do
    pokeMatAt Scalar ptr 0 matA
    pokeMatAt Scalar ptr 16 matB

testNonSquareSuite :: forall lib. (HalfMats lib, DoubleMats lib) => Proxy lib -> TestTree
testNonSquareSuite _ =
  sutTests
    "TestNonSquare"
    (genTestNonSquare @lib)
    [at @Std140 Nothing, at @Std430 Nothing, at @Scalar Nothing]

--------------------------------------------------------------------------------
-- D2
--------------------------------------------------------------------------------

data D2 lib = D2
  { d2A :: Half
  , d2B :: Vec2 lib
  , d2C :: Half
  , d2D :: Mat2 lib
  , d2E :: Half
  }
  deriving (Generic)

deriving instance (FloatVecs lib, FloatMats lib) => Show (D2 lib)

deriving instance (FloatVecs lib, FloatMats lib) => Eq (D2 lib)

instance (MemoryLayoutRules l, GAlignedStorable l (Rep (D2 lib))) => AlignedStorable l (D2 lib)

genD2 :: (FloatVecs lib, FloatMats lib) => Gen (D2 lib)
genD2 = D2 <$> genFloat <*> genVec <*> genFloat <*> genMat <*> genFloat

-- GLSL: struct D2 (glsl/Conformance.glsl)
instance (FloatVecs lib, FloatMats lib) => Storable (Oracle Std140 (D2 lib)) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( D2
              <$> peekByteOff ptr 0
              <*> peekVecAt Std140 ptr 8
              <*> peekByteOff ptr 16
              <*> peekMatAt Std140 ptr 32
              <*> peekByteOff ptr 64
          )
  poke ptr (Oracle D2{..}) = do
    pokeByteOff ptr 0 d2A
    pokeVecAt Std140 ptr 8 d2B
    pokeByteOff ptr 16 d2C
    pokeMatAt Std140 ptr 32 d2D
    pokeByteOff ptr 64 d2E

-- GLSL: struct D2 (glsl/Conformance.glsl)
instance (FloatVecs lib, FloatMats lib) => Storable (Oracle Std430 (D2 lib)) where
  sizeOf _ = 48
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( D2
              <$> peekByteOff ptr 0
              <*> peekVecAt Std430 ptr 8
              <*> peekByteOff ptr 16
              <*> peekMatAt Std430 ptr 24
              <*> peekByteOff ptr 40
          )
  poke ptr (Oracle D2{..}) = do
    pokeByteOff ptr 0 d2A
    pokeVecAt Std430 ptr 8 d2B
    pokeByteOff ptr 16 d2C
    pokeMatAt Std430 ptr 24 d2D
    pokeByteOff ptr 40 d2E

-- GLSL: struct D2 (glsl/Conformance.glsl)
instance (FloatVecs lib, FloatMats lib) => Storable (Oracle Scalar (D2 lib)) where
  sizeOf _ = 36
  alignment _ = 4
  peek ptr =
    Oracle
      <$> ( D2
              <$> peekByteOff ptr 0
              <*> peekVecAt Scalar ptr 4
              <*> peekByteOff ptr 12
              <*> peekMatAt Scalar ptr 16
              <*> peekByteOff ptr 32
          )
  poke ptr (Oracle D2{..}) = do
    pokeByteOff ptr 0 d2A
    pokeVecAt Scalar ptr 4 d2B
    pokeByteOff ptr 12 d2C
    pokeMatAt Scalar ptr 16 d2D
    pokeByteOff ptr 32 d2E

d2Suite :: forall lib. (FloatVecs lib, FloatMats lib) => Proxy lib -> TestTree
d2Suite _ = sutTests "D2" (genD2 @lib) [at @Std140 (Just 66), at @Std430 (Just 42), at @Scalar (Just 34)]

--------------------------------------------------------------------------------
-- Kitchen sink
--------------------------------------------------------------------------------

-- | Every shader primitive, ordered to be pretty close to worst case for padding
-- rules
data KitchenSink lib = KitchenSink
  { ksA :: Half
  , ksB :: DVec3 lib
  , ksC :: Float
  , ksD :: Mat3 lib
  , ksE :: Int32
  , ksF :: Mat3 lib
  , ksG :: Word8
  , ksH :: F16Mat2x3 lib
  , ksI :: Word16
  , ksJ :: F16Mat3x2 lib
  , ksK :: Word16
  , ksL :: Mat3x4 lib
  , ksM :: Int16
  , ksN :: DMat4x3 lib
  , ksO :: Bool
  , ksP :: Int8
  , ksQ :: F16Mat4 lib
  , ksR :: Int64
  , ksS :: DMat2x4 lib
  , ksT :: Word64
  , ksU :: F16Mat4x2 lib
  , ksV :: Half
  }
  deriving (Generic)

deriving instance
  (DoubleVecs lib, FloatMats lib, HalfMats lib, DoubleMats lib) => Show (KitchenSink lib)

deriving instance
  (DoubleVecs lib, FloatMats lib, HalfMats lib, DoubleMats lib) => Eq (KitchenSink lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (KitchenSink lib)))
  => AlignedStorable l (KitchenSink lib)

genKitchenSink
  :: (DoubleVecs lib, FloatMats lib, HalfMats lib, DoubleMats lib) => Gen (KitchenSink lib)
genKitchenSink =
  KitchenSink
    <$> genFloat
    <*> genVec
    <*> genFloat
    <*> genMat
    <*> genInt
    <*> genMat
    <*> genInt
    <*> genMat
    <*> genInt
    <*> genMat
    <*> genInt
    <*> genMat
    <*> genInt
    <*> genMat
    <*> Gen.bool
    <*> genInt
    <*> genMat
    <*> genInt
    <*> genMat
    <*> genInt
    <*> genMat
    <*> genFloat

-- GLSL: struct KitchenSink (glsl/Conformance.glsl)
instance
  (DoubleVecs lib, FloatMats lib, HalfMats lib, DoubleMats lib)
  => Storable (Oracle Std140 (KitchenSink lib))
  where
  sizeOf _ = 704
  alignment _ = 32
  peek ptr =
    Oracle
      <$> ( KitchenSink
              <$> peekByteOff ptr 0
              <*> peekVecAt Std140 ptr 32
              <*> peekByteOff ptr 56
              <*> peekMatAt Std140 ptr 64
              <*> peekByteOff ptr 112
              <*> peekMatAt Std140 ptr 128
              <*> peekByteOff ptr 176
              <*> peekMatAt Std140 ptr 192
              <*> peekByteOff ptr 240
              <*> peekMatAt Std140 ptr 256
              <*> peekByteOff ptr 288
              <*> peekMatAt Std140 ptr 304
              <*> peekByteOff ptr 368
              <*> peekMatAt Std140 ptr 384
              <*> peekByteOff ptr 480
              <*> peekByteOff ptr 484
              <*> peekMatAt Std140 ptr 496
              <*> peekByteOff ptr 560
              <*> peekMatAt Std140 ptr 576
              <*> peekByteOff ptr 640
              <*> peekMatAt Std140 ptr 656
              <*> peekByteOff ptr 688
          )
  poke ptr (Oracle KitchenSink{..}) = do
    pokeByteOff ptr 0 ksA
    pokeVecAt Std140 ptr 32 ksB
    pokeByteOff ptr 56 ksC
    pokeMatAt Std140 ptr 64 ksD
    pokeByteOff ptr 112 ksE
    pokeMatAt Std140 ptr 128 ksF
    pokeByteOff ptr 176 ksG
    pokeMatAt Std140 ptr 192 ksH
    pokeByteOff ptr 240 ksI
    pokeMatAt Std140 ptr 256 ksJ
    pokeByteOff ptr 288 ksK
    pokeMatAt Std140 ptr 304 ksL
    pokeByteOff ptr 368 ksM
    pokeMatAt Std140 ptr 384 ksN
    pokeByteOff ptr 480 ksO
    pokeByteOff ptr 484 ksP
    pokeMatAt Std140 ptr 496 ksQ
    pokeByteOff ptr 560 ksR
    pokeMatAt Std140 ptr 576 ksS
    pokeByteOff ptr 640 ksT
    pokeMatAt Std140 ptr 656 ksU
    pokeByteOff ptr 688 ksV

-- GLSL: struct KitchenSink (glsl/Conformance.glsl)
instance
  (DoubleVecs lib, FloatMats lib, HalfMats lib, DoubleMats lib)
  => Storable (Oracle Std430 (KitchenSink lib))
  where
  sizeOf _ = 576
  alignment _ = 32
  peek ptr =
    Oracle
      <$> ( KitchenSink
              <$> peekByteOff ptr 0
              <*> peekVecAt Std430 ptr 32
              <*> peekByteOff ptr 56
              <*> peekMatAt Std430 ptr 64
              <*> peekByteOff ptr 112
              <*> peekMatAt Std430 ptr 128
              <*> peekByteOff ptr 176
              <*> peekMatAt Std430 ptr 180
              <*> peekByteOff ptr 192
              <*> peekMatAt Std430 ptr 200
              <*> peekByteOff ptr 216
              <*> peekMatAt Std430 ptr 224
              <*> peekByteOff ptr 288
              <*> peekMatAt Std430 ptr 320
              <*> peekByteOff ptr 416
              <*> peekByteOff ptr 420
              <*> peekMatAt Std430 ptr 424
              <*> peekByteOff ptr 456
              <*> peekMatAt Std430 ptr 464
              <*> peekByteOff ptr 528
              <*> peekMatAt Std430 ptr 536
              <*> peekByteOff ptr 552
          )
  poke ptr (Oracle KitchenSink{..}) = do
    pokeByteOff ptr 0 ksA
    pokeVecAt Std430 ptr 32 ksB
    pokeByteOff ptr 56 ksC
    pokeMatAt Std430 ptr 64 ksD
    pokeByteOff ptr 112 ksE
    pokeMatAt Std430 ptr 128 ksF
    pokeByteOff ptr 176 ksG
    pokeMatAt Std430 ptr 180 ksH
    pokeByteOff ptr 192 ksI
    pokeMatAt Std430 ptr 200 ksJ
    pokeByteOff ptr 216 ksK
    pokeMatAt Std430 ptr 224 ksL
    pokeByteOff ptr 288 ksM
    pokeMatAt Std430 ptr 320 ksN
    pokeByteOff ptr 416 ksO
    pokeByteOff ptr 420 ksP
    pokeMatAt Std430 ptr 424 ksQ
    pokeByteOff ptr 456 ksR
    pokeMatAt Std430 ptr 464 ksS
    pokeByteOff ptr 528 ksT
    pokeMatAt Std430 ptr 536 ksU
    pokeByteOff ptr 552 ksV

-- GLSL: struct KitchenSink (glsl/Conformance.glsl)
instance
  (DoubleVecs lib, FloatMats lib, HalfMats lib, DoubleMats lib)
  => Storable (Oracle Scalar (KitchenSink lib))
  where
  sizeOf _ = 440
  alignment _ = 8 -- Based on alignment of dvec3
  peek ptr =
    Oracle
      <$> ( KitchenSink
              <$> peekByteOff ptr 0
              <*> peekVecAt Scalar ptr 8
              <*> peekByteOff ptr 32
              <*> peekMatAt Scalar ptr 36
              <*> peekByteOff ptr 72
              <*> peekMatAt Scalar ptr 76
              <*> peekByteOff ptr 112
              <*> peekMatAt Scalar ptr 114
              <*> peekByteOff ptr 126
              <*> peekMatAt Scalar ptr 128
              <*> peekByteOff ptr 140
              <*> peekMatAt Scalar ptr 144
              <*> peekByteOff ptr 192
              <*> peekMatAt Scalar ptr 200
              <*> peekByteOff ptr 296
              <*> peekByteOff ptr 300
              <*> peekMatAt Scalar ptr 302
              <*> peekByteOff ptr 336
              <*> peekMatAt Scalar ptr 344
              <*> peekByteOff ptr 408
              <*> peekMatAt Scalar ptr 416
              <*> peekByteOff ptr 432
          )
  poke ptr (Oracle KitchenSink{..}) = do
    pokeByteOff ptr 0 ksA
    pokeVecAt Scalar ptr 8 ksB
    pokeByteOff ptr 32 ksC
    pokeMatAt Scalar ptr 36 ksD
    pokeByteOff ptr 72 ksE
    pokeMatAt Scalar ptr 76 ksF
    pokeByteOff ptr 112 ksG
    pokeMatAt Scalar ptr 114 ksH
    pokeByteOff ptr 126 ksI
    pokeMatAt Scalar ptr 128 ksJ
    pokeByteOff ptr 140 ksK
    pokeMatAt Scalar ptr 144 ksL
    pokeByteOff ptr 192 ksM
    pokeMatAt Scalar ptr 200 ksN
    pokeByteOff ptr 296 ksO
    pokeByteOff ptr 300 ksP
    pokeMatAt Scalar ptr 302 ksQ
    pokeByteOff ptr 336 ksR
    pokeMatAt Scalar ptr 344 ksS
    pokeByteOff ptr 408 ksT
    pokeMatAt Scalar ptr 416 ksU
    pokeByteOff ptr 432 ksV

kitchenSinkSuite
  :: forall lib. (DoubleVecs lib, FloatMats lib, HalfMats lib, DoubleMats lib) => Proxy lib -> TestTree
kitchenSinkSuite _ =
  sutTests
    "KitchenSink"
    (genKitchenSink @lib)
    [at @Std140 Nothing, at @Std430 Nothing, at @Scalar Nothing]

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
data ComplexPadding lib = ComplexPadding
  { complexpaddingA :: Float
  , complexpaddingB :: Vec3 lib
  , complexpaddingC :: Mat3 lib
  , complexpaddingD :: Float
  , complexpaddingE :: Mat4 lib
  , complexpaddingF :: Float
  }
  deriving (Generic)

deriving instance (FloatVecs lib, FloatMats lib) => Show (ComplexPadding lib)

deriving instance (FloatVecs lib, FloatMats lib) => Eq (ComplexPadding lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (ComplexPadding lib)))
  => AlignedStorable l (ComplexPadding lib)

genComplexPadding :: (FloatVecs lib, FloatMats lib) => Gen (ComplexPadding lib)
genComplexPadding =
  ComplexPadding
    <$> genFloat
    <*> genVec
    <*> genMat
    <*> genFloat
    <*> genMat
    <*> genFloat

-- GLSL: struct ComplexPadding (glsl/Conformance.glsl)
instance (FloatVecs lib, FloatMats lib) => Storable (Oracle Std140 (ComplexPadding lib)) where
  sizeOf _ = 176
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( ComplexPadding
              <$> peekByteOff ptr 0
              <*> peekVecAt Std140 ptr 16
              <*> peekMatAt Std140 ptr 32
              <*> peekByteOff ptr 80
              <*> peekMatAt Std140 ptr 96
              <*> peekByteOff ptr 160
          )
  poke ptr (Oracle ComplexPadding{..}) = do
    pokeByteOff ptr 0 complexpaddingA
    pokeVecAt Std140 ptr 16 complexpaddingB
    pokeMatAt Std140 ptr 32 complexpaddingC
    pokeByteOff ptr 80 complexpaddingD
    pokeMatAt Std140 ptr 96 complexpaddingE
    pokeByteOff ptr 160 complexpaddingF

-- GLSL: struct ComplexPadding (glsl/Conformance.glsl)
instance (FloatVecs lib, FloatMats lib) => Storable (Oracle Std430 (ComplexPadding lib)) where
  sizeOf _ = 176
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( ComplexPadding
              <$> peekByteOff ptr 0
              <*> peekVecAt Std430 ptr 16
              <*> peekMatAt Std430 ptr 32
              <*> peekByteOff ptr 80
              <*> peekMatAt Std430 ptr 96
              <*> peekByteOff ptr 160
          )
  poke ptr (Oracle ComplexPadding{..}) = do
    pokeByteOff ptr 0 complexpaddingA
    pokeVecAt Std430 ptr 16 complexpaddingB
    pokeMatAt Std430 ptr 32 complexpaddingC
    pokeByteOff ptr 80 complexpaddingD
    pokeMatAt Std430 ptr 96 complexpaddingE
    pokeByteOff ptr 160 complexpaddingF

-- GLSL: struct ComplexPadding (glsl/Conformance.glsl)
instance (FloatVecs lib, FloatMats lib) => Storable (Oracle Scalar (ComplexPadding lib)) where
  sizeOf _ = 124
  alignment _ = 4
  peek ptr =
    Oracle
      <$> ( ComplexPadding
              <$> peekByteOff ptr 0
              <*> peekVecAt Scalar ptr 4
              <*> peekMatAt Scalar ptr 16
              <*> peekByteOff ptr 52
              <*> peekMatAt Scalar ptr 56
              <*> peekByteOff ptr 120
          )
  poke ptr (Oracle ComplexPadding{..}) = do
    pokeByteOff ptr 0 complexpaddingA
    pokeVecAt Scalar ptr 4 complexpaddingB
    pokeMatAt Scalar ptr 16 complexpaddingC
    pokeByteOff ptr 52 complexpaddingD
    pokeMatAt Scalar ptr 56 complexpaddingE
    pokeByteOff ptr 120 complexpaddingF

complexPaddingSuite :: forall lib. (FloatVecs lib, FloatMats lib) => Proxy lib -> TestTree
complexPaddingSuite _ =
  sutTests
    "ComplexPadding"
    (genComplexPadding @lib)
    [at @Std140 (Just 164), at @Std430 (Just 164), at @Scalar (Just 124)]

--------------------------------------------------------------------------------
-- Uniforms
--------------------------------------------------------------------------------

data Uniforms lib = Uniforms
  { modelView :: Mat4 lib
  , cameraPos :: Vec4 lib
  }
  deriving (Generic)

deriving instance (FloatVecs lib, FloatMat4 lib) => Show (Uniforms lib)

deriving instance (FloatVecs lib, FloatMat4 lib) => Eq (Uniforms lib)

instance (MemoryLayoutRules l, GAlignedStorable l (Rep (Uniforms lib))) => AlignedStorable l (Uniforms lib)

genUniforms :: (FloatVecs lib, FloatMat4 lib) => Gen (Uniforms lib)
genUniforms = Uniforms <$> genMat <*> genVec

-- GLSL: struct Uniforms (glsl/Conformance.glsl)
instance (FloatVecs lib, FloatMat4 lib) => Storable (Oracle Std140 (Uniforms lib)) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr = Oracle <$> (Uniforms <$> peekMatAt Std140 ptr 0 <*> peekVecAt Std140 ptr 64)
  poke ptr (Oracle Uniforms{..}) = do
    pokeMatAt Std140 ptr 0 modelView
    pokeVecAt Std140 ptr 64 cameraPos

-- GLSL: struct Uniforms (glsl/Conformance.glsl)
instance (FloatVecs lib, FloatMat4 lib) => Storable (Oracle Std430 (Uniforms lib)) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr = Oracle <$> (Uniforms <$> peekMatAt Std430 ptr 0 <*> peekVecAt Std430 ptr 64)
  poke ptr (Oracle Uniforms{..}) = do
    pokeMatAt Std430 ptr 0 modelView
    pokeVecAt Std430 ptr 64 cameraPos

-- GLSL: struct Uniforms (glsl/Conformance.glsl)
instance (FloatVecs lib, FloatMat4 lib) => Storable (Oracle Scalar (Uniforms lib)) where
  sizeOf _ = 80
  alignment _ = 4
  peek ptr = Oracle <$> (Uniforms <$> peekMatAt Scalar ptr 0 <*> peekVecAt Scalar ptr 64)
  poke ptr (Oracle Uniforms{..}) = do
    pokeMatAt Scalar ptr 0 modelView
    pokeVecAt Scalar ptr 64 cameraPos

uniformsSuite :: forall lib. (FloatVecs lib, FloatMat4 lib) => Proxy lib -> TestTree
uniformsSuite _ =
  sutTests
    "Uniforms"
    (genUniforms @lib)
    [at @Std140 Nothing, at @Std430 Nothing, at @Scalar Nothing]

--------------------------------------------------------------------------------
-- DoubleTrouble
--------------------------------------------------------------------------------

data DoubleTrouble lib = DoubleTrouble
  { dtD0 :: Double
  , dtM0 :: DMat3 lib
  , dtF0 :: Float
  , dtV0 :: DVec3 lib
  , dtF1 :: Float
  , dtV1 :: DVec4 lib
  , dtF2 :: Float
  , dtM1 :: DMat4 lib
  }
  deriving (Generic)

deriving instance (DoubleVecs lib, DoubleMats lib) => Show (DoubleTrouble lib)

deriving instance (DoubleVecs lib, DoubleMats lib) => Eq (DoubleTrouble lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (DoubleTrouble lib)))
  => AlignedStorable l (DoubleTrouble lib)

genDoubleTrouble :: (DoubleVecs lib, DoubleMats lib) => Gen (DoubleTrouble lib)
genDoubleTrouble =
  DoubleTrouble
    <$> genFloat
    <*> genMat
    <*> genFloat
    <*> genVec
    <*> genFloat
    <*> genVec
    <*> genFloat
    <*> genMat

-- GLSL: struct DoubleTrouble (glsl/Conformance.glsl)
instance (DoubleVecs lib, DoubleMats lib) => Storable (Oracle Std140 (DoubleTrouble lib)) where
  sizeOf _ = 384
  alignment _ = 32
  peek ptr =
    Oracle
      <$> ( DoubleTrouble
              <$> peekByteOff ptr 0
              <*> peekMatAt Std140 ptr 32
              <*> peekByteOff ptr 128
              <*> peekVecAt Std140 ptr 160
              <*> peekByteOff ptr 184
              <*> peekVecAt Std140 ptr 192
              <*> peekByteOff ptr 224
              <*> peekMatAt Std140 ptr 256
          )
  poke ptr (Oracle DoubleTrouble{..}) = do
    pokeByteOff ptr 0 dtD0
    pokeMatAt Std140 ptr 32 dtM0
    pokeByteOff ptr 128 dtF0
    pokeVecAt Std140 ptr 160 dtV0
    pokeByteOff ptr 184 dtF1
    pokeVecAt Std140 ptr 192 dtV1
    pokeByteOff ptr 224 dtF2
    pokeMatAt Std140 ptr 256 dtM1

doubleTroubleSuite :: forall lib. (DoubleVecs lib, DoubleMats lib) => Proxy lib -> TestTree
doubleTroubleSuite _ = sutTests "DoubleTrouble" (genDoubleTrouble @lib) [at @Std140 Nothing]

--------------------------------------------------------------------------------
-- DUniforms
--------------------------------------------------------------------------------

data DUniforms lib = DUniforms
  { duM :: DMat4 lib
  , duV :: DVec4 lib
  }
  deriving (Generic)

deriving instance (DoubleVecs lib, DoubleMat4 lib) => Show (DUniforms lib)

deriving instance (DoubleVecs lib, DoubleMat4 lib) => Eq (DUniforms lib)

instance (MemoryLayoutRules l, GAlignedStorable l (Rep (DUniforms lib))) => AlignedStorable l (DUniforms lib)

genDUniforms :: (DoubleVecs lib, DoubleMat4 lib) => Gen (DUniforms lib)
genDUniforms = DUniforms <$> genMat <*> genVec

-- GLSL: struct DUniforms (glsl/Conformance.glsl)
instance (DoubleVecs lib, DoubleMat4 lib) => Storable (Oracle Std140 (DUniforms lib)) where
  sizeOf _ = 160
  alignment _ = 32
  peek ptr = Oracle <$> (DUniforms <$> peekMatAt Std140 ptr 0 <*> peekVecAt Std140 ptr 128)
  poke ptr (Oracle DUniforms{..}) = do
    pokeMatAt Std140 ptr 0 duM
    pokeVecAt Std140 ptr 128 duV

-- GLSL: struct DUniforms (glsl/Conformance.glsl)
instance (DoubleVecs lib, DoubleMat4 lib) => Storable (Oracle Std430 (DUniforms lib)) where
  sizeOf _ = 160
  alignment _ = 32
  peek ptr = Oracle <$> (DUniforms <$> peekMatAt Std430 ptr 0 <*> peekVecAt Std430 ptr 128)
  poke ptr (Oracle DUniforms{..}) = do
    pokeMatAt Std430 ptr 0 duM
    pokeVecAt Std430 ptr 128 duV

-- GLSL: struct DUniforms (glsl/Conformance.glsl)
instance (DoubleVecs lib, DoubleMat4 lib) => Storable (Oracle Scalar (DUniforms lib)) where
  sizeOf _ = 160
  alignment _ = 8
  peek ptr = Oracle <$> (DUniforms <$> peekMatAt Scalar ptr 0 <*> peekVecAt Scalar ptr 128)
  poke ptr (Oracle DUniforms{..}) = do
    pokeMatAt Scalar ptr 0 duM
    pokeVecAt Scalar ptr 128 duV

dUniformsSuite :: forall lib. (DoubleVecs lib, DoubleMat4 lib) => Proxy lib -> TestTree
dUniformsSuite _ =
  sutTests
    "DUniforms"
    (genDUniforms @lib)
    [at @Std140 (Just 160), at @Std430 (Just 160), at @Scalar (Just 160)]
