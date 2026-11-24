{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Structs under test for geomancy that don't exist in the shared conformance suite.
module GeomancySpec where

import Data.Int
import Data.Word
import Foreign
import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.Aligned.Test
import GHC.Generics (Generic)
import qualified Geomancy.DQuaternion as DQuaternion
import qualified Geomancy.DTransform as DTransform
import qualified Geomancy.DVec3 as DVec3
import qualified Geomancy.I64Vec3 as I64Vec3
import qualified Geomancy.IVec3 as IVec3
import qualified Geomancy.Mat4 as Mat4
import Geomancy.Point (Point (..))
import qualified Geomancy.Quaternion as Quaternion
import qualified Geomancy.Transform as Transform
import qualified Geomancy.U64Vec4 as U64Vec4
import qualified Geomancy.UVec3 as UVec3
import qualified Geomancy.Vec3 as Vec3
import Hedgehog (Gen)
import Test.Tasty

import Foreign.GPU.Storable.Aligned.Geomancy ()
import GeomancyConformanceSpec ()

instance GlslVec Quaternion.Quaternion where
  type VecElem Quaternion.Quaternion = Float
  type VecDim Quaternion.Quaternion = 4
  toComponents q = Quaternion.withQuaternion q \x y z w -> [x, y, z, w]
  fromComponents [x, y, z, w] = Quaternion.quaternion x y z w
  fromComponents xs = componentArity 4 xs

instance GlslVec DQuaternion.DQuaternion where
  type VecElem DQuaternion.DQuaternion = Double
  type VecDim DQuaternion.DQuaternion = 4
  toComponents q = DQuaternion.withDQuaternion q \x y z w -> [x, y, z, w]
  fromComponents [x, y, z, w] = DQuaternion.dquaternion x y z w
  fromComponents xs = componentArity 4 xs

unPoint :: Point v -> v
unPoint (Point v) = v

--------------------------------------------------------------------------------
-- TransformTail
--------------------------------------------------------------------------------

data TransformTail = TransformTail
  { ttM :: Transform.Transform
  , ttF :: Float
  }
  deriving (Eq, Generic, Show)

genTransformTail :: Gen TransformTail
genTransformTail = TransformTail . Transform.Transform <$> genMat <*> genFloat

instance AlignedStorable Std140 TransformTail
instance AlignedStorable Std430 TransformTail
instance AlignedStorable Scalar TransformTail

-- GLSL: struct TransformTail (glsl/Geomancy.glsl)
instance Storable (Oracle Std140 TransformTail) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    Oracle
      <$> (TransformTail . Transform.Transform <$> peekMatAt Std140 ptr 0 <*> peekByteOff ptr 64)
  poke ptr (Oracle TransformTail{..}) = do
    pokeMatAt Std140 ptr 0 (Transform.unTransform ttM)
    pokeByteOff ptr 64 ttF

-- GLSL: struct TransformTail (glsl/Geomancy.glsl)
instance Storable (Oracle Std430 TransformTail) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    Oracle
      <$> (TransformTail . Transform.Transform <$> peekMatAt Std430 ptr 0 <*> peekByteOff ptr 64)
  poke ptr (Oracle TransformTail{..}) = do
    pokeMatAt Std430 ptr 0 (Transform.unTransform ttM)
    pokeByteOff ptr 64 ttF

-- GLSL: struct TransformTail (glsl/Geomancy.glsl)
instance Storable (Oracle Scalar TransformTail) where
  sizeOf _ = 68
  alignment _ = 4
  peek ptr =
    Oracle
      <$> (TransformTail . Transform.Transform <$> peekMatAt Scalar ptr 0 <*> peekByteOff ptr 64)
  poke ptr (Oracle TransformTail{..}) = do
    pokeMatAt Scalar ptr 0 (Transform.unTransform ttM)
    pokeByteOff ptr 64 ttF

transformTailSuite :: TestTree
transformTailSuite =
  sutTests
    "TransformTail"
    genTransformTail
    [at @Std140 (Just 68), at @Std430 (Just 68), at @Scalar (Just 68)]

--------------------------------------------------------------------------------
-- OffsetMat4
--------------------------------------------------------------------------------

data OffsetMat4 = OffsetMat4
  { omA :: Float
  , omM :: Mat4.Mat4
  }
  deriving (Eq, Generic, Show)

genOffsetMat4 :: Gen OffsetMat4
genOffsetMat4 = OffsetMat4 <$> genFloat <*> genMat

instance AlignedStorable Std140 OffsetMat4
instance AlignedStorable Std430 OffsetMat4
instance AlignedStorable Scalar OffsetMat4

-- GLSL: struct OffsetMat4 (glsl/Geomancy.glsl)
instance Storable (Oracle Std140 OffsetMat4) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr = Oracle <$> (OffsetMat4 <$> peekByteOff ptr 0 <*> peekMatAt Std140 ptr 16)
  poke ptr (Oracle OffsetMat4{..}) = do
    pokeByteOff ptr 0 omA
    pokeMatAt Std140 ptr 16 omM

-- GLSL: struct OffsetMat4 (glsl/Geomancy.glsl)
instance Storable (Oracle Std430 OffsetMat4) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr = Oracle <$> (OffsetMat4 <$> peekByteOff ptr 0 <*> peekMatAt Std430 ptr 16)
  poke ptr (Oracle OffsetMat4{..}) = do
    pokeByteOff ptr 0 omA
    pokeMatAt Std430 ptr 16 omM

-- GLSL: struct OffsetMat4 (glsl/Geomancy.glsl)
instance Storable (Oracle Scalar OffsetMat4) where
  sizeOf _ = 68
  alignment _ = 4
  peek ptr = Oracle <$> (OffsetMat4 <$> peekByteOff ptr 0 <*> peekMatAt Scalar ptr 4)
  poke ptr (Oracle OffsetMat4{..}) = do
    pokeByteOff ptr 0 omA
    pokeMatAt Scalar ptr 4 omM

offsetMat4Suite :: TestTree
offsetMat4Suite =
  sutTests
    "OffsetMat4"
    genOffsetMat4
    [at @Std140 (Just 80), at @Std430 (Just 80), at @Scalar (Just 68)]

--------------------------------------------------------------------------------
-- QuatStruct
--------------------------------------------------------------------------------

data QuatStruct = QuatStruct
  { qsA :: Float
  , qsQ :: Quaternion.Quaternion
  }
  deriving (Eq, Generic, Show)

genQuatStruct :: Gen QuatStruct
genQuatStruct = QuatStruct <$> genFloat <*> genVec

instance AlignedStorable Std140 QuatStruct
instance AlignedStorable Std430 QuatStruct
instance AlignedStorable Scalar QuatStruct

-- GLSL: struct QuatStruct (glsl/Geomancy.glsl)
instance Storable (Oracle Std140 QuatStruct) where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = Oracle <$> (QuatStruct <$> peekByteOff ptr 0 <*> peekVecAt Std140 ptr 16)
  poke ptr (Oracle QuatStruct{..}) = do
    pokeByteOff ptr 0 qsA
    pokeVecAt Std140 ptr 16 qsQ

-- GLSL: struct QuatStruct (glsl/Geomancy.glsl)
instance Storable (Oracle Std430 QuatStruct) where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = Oracle <$> (QuatStruct <$> peekByteOff ptr 0 <*> peekVecAt Std430 ptr 16)
  poke ptr (Oracle QuatStruct{..}) = do
    pokeByteOff ptr 0 qsA
    pokeVecAt Std430 ptr 16 qsQ

-- GLSL: struct QuatStruct (glsl/Geomancy.glsl)
instance Storable (Oracle Scalar QuatStruct) where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr = Oracle <$> (QuatStruct <$> peekByteOff ptr 0 <*> peekVecAt Scalar ptr 4)
  poke ptr (Oracle QuatStruct{..}) = do
    pokeByteOff ptr 0 qsA
    pokeVecAt Scalar ptr 4 qsQ

quatStructSuite :: TestTree
quatStructSuite =
  sutTests
    "QuatStruct"
    genQuatStruct
    [at @Std140 (Just 32), at @Std430 (Just 32), at @Scalar (Just 20)]

--------------------------------------------------------------------------------
-- Vec3Mix
--------------------------------------------------------------------------------

data Vec3Mix = Vec3Mix
  { vmA :: Float
  , vmP :: Point Vec3.Vec3
  , vmB :: Float
  , vmQ :: Vec3.Packed
  }
  deriving (Eq, Generic, Show)

genVec3Mix :: Gen Vec3Mix
genVec3Mix = Vec3Mix <$> genFloat <*> (Point <$> genVec) <*> genFloat <*> (Vec3.Packed <$> genVec)

instance AlignedStorable Std140 Vec3Mix
instance AlignedStorable Std430 Vec3Mix
instance AlignedStorable Scalar Vec3Mix

-- GLSL: struct Vec3Mix (glsl/Geomancy.glsl)
instance Storable (Oracle Std140 Vec3Mix) where
  sizeOf _ = 48
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( Vec3Mix
              <$> peekByteOff ptr 0
              <*> (Point <$> peekVecAt Std140 ptr 16)
              <*> peekByteOff ptr 28
              <*> (Vec3.Packed <$> peekVecAt Std140 ptr 32)
          )
  poke ptr (Oracle Vec3Mix{..}) = do
    pokeByteOff ptr 0 vmA
    pokeVecAt Std140 ptr 16 (unPoint vmP)
    pokeByteOff ptr 28 vmB
    pokeVecAt Std140 ptr 32 (Vec3.unPacked vmQ)

-- GLSL: struct Vec3Mix (glsl/Geomancy.glsl)
instance Storable (Oracle Std430 Vec3Mix) where
  sizeOf _ = 48
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( Vec3Mix
              <$> peekByteOff ptr 0
              <*> (Point <$> peekVecAt Std430 ptr 16)
              <*> peekByteOff ptr 28
              <*> (Vec3.Packed <$> peekVecAt Std430 ptr 32)
          )
  poke ptr (Oracle Vec3Mix{..}) = do
    pokeByteOff ptr 0 vmA
    pokeVecAt Std430 ptr 16 (unPoint vmP)
    pokeByteOff ptr 28 vmB
    pokeVecAt Std430 ptr 32 (Vec3.unPacked vmQ)

-- GLSL: struct Vec3Mix (glsl/Geomancy.glsl)
instance Storable (Oracle Scalar Vec3Mix) where
  sizeOf _ = 32
  alignment _ = 4
  peek ptr =
    Oracle
      <$> ( Vec3Mix
              <$> peekByteOff ptr 0
              <*> (Point <$> peekVecAt Scalar ptr 4)
              <*> peekByteOff ptr 16
              <*> (Vec3.Packed <$> peekVecAt Scalar ptr 20)
          )
  poke ptr (Oracle Vec3Mix{..}) = do
    pokeByteOff ptr 0 vmA
    pokeVecAt Scalar ptr 4 (unPoint vmP)
    pokeByteOff ptr 16 vmB
    pokeVecAt Scalar ptr 20 (Vec3.unPacked vmQ)

vec3MixSuite :: TestTree
vec3MixSuite =
  sutTests
    "Vec3Mix"
    genVec3Mix
    [at @Std140 (Just 44), at @Std430 (Just 44), at @Scalar (Just 32)]

--------------------------------------------------------------------------------
-- DoubleMix
--------------------------------------------------------------------------------

data DoubleMix = DoubleMix
  { dmA :: Float
  , dmT :: DTransform.DTransform
  , dmQ :: DQuaternion.DQuaternion
  , dmP :: Point DVec3.DVec3
  , dmB :: Float
  , dmR :: DVec3.Packed
  }
  deriving (Eq, Generic, Show)

genDoubleMix :: Gen DoubleMix
genDoubleMix =
  DoubleMix
    <$> genFloat
    <*> (DTransform.DTransform <$> genMat)
    <*> genVec
    <*> (Point <$> genVec)
    <*> genFloat
    <*> (DVec3.Packed <$> genVec)

instance AlignedStorable Std140 DoubleMix
instance AlignedStorable Std430 DoubleMix
instance AlignedStorable Scalar DoubleMix

-- GLSL: struct DoubleMix (glsl/Geomancy.glsl)
instance Storable (Oracle Std140 DoubleMix) where
  sizeOf _ = 256
  alignment _ = 32
  peek ptr =
    Oracle
      <$> ( DoubleMix
              <$> peekByteOff ptr 0
              <*> (DTransform.DTransform <$> peekMatAt Std140 ptr 32)
              <*> peekVecAt Std140 ptr 160
              <*> (Point <$> peekVecAt Std140 ptr 192)
              <*> peekByteOff ptr 216
              <*> (DVec3.Packed <$> peekVecAt Std140 ptr 224)
          )
  poke ptr (Oracle DoubleMix{..}) = do
    pokeByteOff ptr 0 dmA
    pokeMatAt Std140 ptr 32 (DTransform.unDTransform dmT)
    pokeVecAt Std140 ptr 160 dmQ
    pokeVecAt Std140 ptr 192 (unPoint dmP)
    pokeByteOff ptr 216 dmB
    pokeVecAt Std140 ptr 224 (DVec3.unPacked dmR)

-- GLSL: struct DoubleMix (glsl/Geomancy.glsl)
instance Storable (Oracle Std430 DoubleMix) where
  sizeOf _ = 256
  alignment _ = 32
  peek ptr =
    Oracle
      <$> ( DoubleMix
              <$> peekByteOff ptr 0
              <*> (DTransform.DTransform <$> peekMatAt Std430 ptr 32)
              <*> peekVecAt Std430 ptr 160
              <*> (Point <$> peekVecAt Std430 ptr 192)
              <*> peekByteOff ptr 216
              <*> (DVec3.Packed <$> peekVecAt Std430 ptr 224)
          )
  poke ptr (Oracle DoubleMix{..}) = do
    pokeByteOff ptr 0 dmA
    pokeMatAt Std430 ptr 32 (DTransform.unDTransform dmT)
    pokeVecAt Std430 ptr 160 dmQ
    pokeVecAt Std430 ptr 192 (unPoint dmP)
    pokeByteOff ptr 216 dmB
    pokeVecAt Std430 ptr 224 (DVec3.unPacked dmR)

-- GLSL: struct DoubleMix (glsl/Geomancy.glsl)
instance Storable (Oracle Scalar DoubleMix) where
  sizeOf _ = 224
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( DoubleMix
              <$> peekByteOff ptr 0
              <*> (DTransform.DTransform <$> peekMatAt Scalar ptr 8)
              <*> peekVecAt Scalar ptr 136
              <*> (Point <$> peekVecAt Scalar ptr 168)
              <*> peekByteOff ptr 192
              <*> (DVec3.Packed <$> peekVecAt Scalar ptr 200)
          )
  poke ptr (Oracle DoubleMix{..}) = do
    pokeByteOff ptr 0 dmA
    pokeMatAt Scalar ptr 8 (DTransform.unDTransform dmT)
    pokeVecAt Scalar ptr 136 dmQ
    pokeVecAt Scalar ptr 168 (unPoint dmP)
    pokeByteOff ptr 192 dmB
    pokeVecAt Scalar ptr 200 (DVec3.unPacked dmR)

doubleMixSuite :: TestTree
doubleMixSuite =
  sutTests
    "DoubleMix"
    genDoubleMix
    [at @Std140 (Just 248), at @Std430 (Just 248), at @Scalar (Just 224)]

--------------------------------------------------------------------------------
-- IntMix
--------------------------------------------------------------------------------

data IntMix = IntMix
  { imA :: Int32
  , imP :: Point IVec3.IVec3
  , imB :: Int32
  , imQ :: UVec3.Packed
  , imC :: Word32
  , imR :: I64Vec3.Packed
  , imD :: Word32
  , imS :: U64Vec4.U64Vec4
  }
  deriving (Eq, Generic, Show)

genIntMix :: Gen IntMix
genIntMix =
  IntMix
    <$> genInt
    <*> (Point <$> genVec)
    <*> genInt
    <*> (UVec3.Packed <$> genVec)
    <*> genInt
    <*> (I64Vec3.Packed <$> genVec)
    <*> genInt
    <*> genVec

instance AlignedStorable Std140 IntMix
instance AlignedStorable Std430 IntMix
instance AlignedStorable Scalar IntMix

-- GLSL: struct IntMix (glsl/Geomancy.glsl)
instance Storable (Oracle Std140 IntMix) where
  sizeOf _ = 128
  alignment _ = 32
  peek ptr =
    Oracle
      <$> ( IntMix
              <$> peekByteOff ptr 0
              <*> (Point <$> peekVecAt Std140 ptr 16)
              <*> peekByteOff ptr 28
              <*> (UVec3.Packed <$> peekVecAt Std140 ptr 32)
              <*> peekByteOff ptr 44
              <*> (I64Vec3.Packed <$> peekVecAt Std140 ptr 64)
              <*> peekByteOff ptr 88
              <*> peekVecAt Std140 ptr 96
          )
  poke ptr (Oracle IntMix{..}) = do
    pokeByteOff ptr 0 imA
    pokeVecAt Std140 ptr 16 (unPoint imP)
    pokeByteOff ptr 28 imB
    pokeVecAt Std140 ptr 32 (UVec3.unPacked imQ)
    pokeByteOff ptr 44 imC
    pokeVecAt Std140 ptr 64 (I64Vec3.unPacked imR)
    pokeByteOff ptr 88 imD
    pokeVecAt Std140 ptr 96 imS

-- GLSL: struct IntMix (glsl/Geomancy.glsl)
instance Storable (Oracle Std430 IntMix) where
  sizeOf _ = 128
  alignment _ = 32
  peek ptr =
    Oracle
      <$> ( IntMix
              <$> peekByteOff ptr 0
              <*> (Point <$> peekVecAt Std430 ptr 16)
              <*> peekByteOff ptr 28
              <*> (UVec3.Packed <$> peekVecAt Std430 ptr 32)
              <*> peekByteOff ptr 44
              <*> (I64Vec3.Packed <$> peekVecAt Std430 ptr 64)
              <*> peekByteOff ptr 88
              <*> peekVecAt Std430 ptr 96
          )
  poke ptr (Oracle IntMix{..}) = do
    pokeByteOff ptr 0 imA
    pokeVecAt Std430 ptr 16 (unPoint imP)
    pokeByteOff ptr 28 imB
    pokeVecAt Std430 ptr 32 (UVec3.unPacked imQ)
    pokeByteOff ptr 44 imC
    pokeVecAt Std430 ptr 64 (I64Vec3.unPacked imR)
    pokeByteOff ptr 88 imD
    pokeVecAt Std430 ptr 96 imS

-- GLSL: struct IntMix (glsl/Geomancy.glsl)
instance Storable (Oracle Scalar IntMix) where
  sizeOf _ = 104
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( IntMix
              <$> peekByteOff ptr 0
              <*> (Point <$> peekVecAt Scalar ptr 4)
              <*> peekByteOff ptr 16
              <*> (UVec3.Packed <$> peekVecAt Scalar ptr 20)
              <*> peekByteOff ptr 32
              <*> (I64Vec3.Packed <$> peekVecAt Scalar ptr 40)
              <*> peekByteOff ptr 64
              <*> peekVecAt Scalar ptr 72
          )
  poke ptr (Oracle IntMix{..}) = do
    pokeByteOff ptr 0 imA
    pokeVecAt Scalar ptr 4 (unPoint imP)
    pokeByteOff ptr 16 imB
    pokeVecAt Scalar ptr 20 (UVec3.unPacked imQ)
    pokeByteOff ptr 32 imC
    pokeVecAt Scalar ptr 40 (I64Vec3.unPacked imR)
    pokeByteOff ptr 64 imD
    pokeVecAt Scalar ptr 72 imS

intMixSuite :: TestTree
intMixSuite =
  sutTests
    "IntMix"
    genIntMix
    [at @Std140 (Just 128), at @Std430 (Just 128), at @Scalar (Just 104)]

test_geomancy :: TestTree
test_geomancy =
  testGroup
    "geomancy"
    [ transformTailSuite
    , offsetMat4Suite
    , quatStructSuite
    , vec3MixSuite
    , doubleMixSuite
    , intMixSuite
    ]
