{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Structs of float vectors and their oracles
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Structs under test whose vector fields are @vec2@, @vec3@ and @vec4@
-- ('FloatVecs'): the shapes every vector library has.
module Foreign.GPU.Storable.Aligned.Test.Sut.FloatVec (
  Std140Test (Std140Test),
  genStd140Test,
  std140TestSuite,
  PaddingTest (PaddingTest),
  genPaddingTest,
  paddingTestSuite,
  FinalPaddingTest (FinalPaddingTest),
  genFinalPaddingTest,
  finalPaddingTestSuite,
  Vertex (Vertex),
  genVertex,
  vertexSuite,
  Nested (Nested),
  genNested,
  nestedSuite,
  NestedNewtype (NestedNewtype),
  genNestedNewtype,
  nestedNewtypeSuite,
  VectorStruct (VectorStruct),
  genVectorStruct,
  vectorStructSuite,
  TailVec3 (TailVec3),
  genTailVec3,
  tailVec3Suite,
)
where

import Data.Proxy
import Data.Vector.Storable.Sized qualified as SSV
import Foreign
import Foreign.GPU.Storable.Aligned
import GHC.Generics
import Hedgehog
import Test.Tasty (TestTree)

import Foreign.GPU.Storable.Aligned.Test.Glsl
import Foreign.GPU.Storable.Aligned.Test.Harness
import Foreign.GPU.Storable.Aligned.Test.Sut.Prim
import Foreign.GPU.Storable.Aligned.Test.Vocab

--------------------------------------------------------------------------------
-- PaddingTest
--------------------------------------------------------------------------------

data PaddingTest lib = PaddingTest
  { f :: Float
  , v :: Vec4 lib
  }
  deriving (Generic)

deriving instance (FloatVecs lib) => Show (PaddingTest lib)

deriving instance (FloatVecs lib) => Eq (PaddingTest lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (PaddingTest lib)))
  => AlignedStorable l (PaddingTest lib)

genPaddingTest :: (FloatVecs lib) => Gen (PaddingTest lib)
genPaddingTest = PaddingTest <$> genFloat <*> genVec

-- GLSL: struct PaddingTest (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std140 (PaddingTest lib)) where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = Oracle <$> (PaddingTest <$> peekByteOff ptr 0 <*> peekVecAt Std140 ptr 16)
  poke ptr (Oracle PaddingTest{..}) = do
    pokeByteOff ptr 0 f
    pokeVecAt Std140 ptr 16 v

-- GLSL: struct PaddingTest (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std430 (PaddingTest lib)) where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = Oracle <$> (PaddingTest <$> peekByteOff ptr 0 <*> peekVecAt Std430 ptr 16)
  poke ptr (Oracle PaddingTest{..}) = do
    pokeByteOff ptr 0 f
    pokeVecAt Std430 ptr 16 v

-- GLSL: struct PaddingTest (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Scalar (PaddingTest lib)) where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr = Oracle <$> (PaddingTest <$> peekByteOff ptr 0 <*> peekVecAt Scalar ptr 4)
  poke ptr (Oracle PaddingTest{..}) = do
    pokeByteOff ptr 0 f
    pokeVecAt Scalar ptr 4 v

paddingTestSuite :: forall lib. (FloatVecs lib) => Proxy lib -> TestTree
paddingTestSuite _ =
  sutTests
    "PaddingTest"
    (genPaddingTest @lib)
    [at @Std140 (Just 32), at @Std430 (Just 32), at @Scalar (Just 20)]

--------------------------------------------------------------------------------
-- Std140Test
--------------------------------------------------------------------------------

data Std140Test lib = Std140Test
  { v3 :: Vec3 lib
  , f1 :: Float
  }
  deriving (Generic)

deriving instance (FloatVecs lib) => Show (Std140Test lib)

deriving instance (FloatVecs lib) => Eq (Std140Test lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (Std140Test lib)))
  => AlignedStorable l (Std140Test lib)

genStd140Test :: (FloatVecs lib) => Gen (Std140Test lib)
genStd140Test = Std140Test <$> genVec <*> genFloat

-- GLSL: struct Std140Test (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std140 (Std140Test lib)) where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = Oracle <$> (Std140Test <$> peekVecAt Std140 ptr 0 <*> peekByteOff ptr 12)
  poke ptr (Oracle Std140Test{..}) = do
    pokeVecAt Std140 ptr 0 v3
    pokeByteOff ptr 12 f1

-- GLSL: struct Std140Test (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std430 (Std140Test lib)) where
  sizeOf _ = 16
  alignment _ = 16
  peek ptr = Oracle <$> (Std140Test <$> peekVecAt Std430 ptr 0 <*> peekByteOff ptr 12)
  poke ptr (Oracle Std140Test{..}) = do
    pokeVecAt Std430 ptr 0 v3
    pokeByteOff ptr 12 f1

-- GLSL: struct Std140Test (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Scalar (Std140Test lib)) where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = Oracle <$> (Std140Test <$> peekVecAt Scalar ptr 0 <*> peekByteOff ptr 12)
  poke ptr (Oracle Std140Test{..}) = do
    pokeVecAt Scalar ptr 0 v3
    pokeByteOff ptr 12 f1

std140TestSuite :: forall lib. (FloatVecs lib) => Proxy lib -> TestTree
std140TestSuite _ =
  sutTests
    "Std140Test"
    (genStd140Test @lib)
    [at @Std140 (Just 16), at @Std430 (Just 16), at @Scalar (Just 16)]

--------------------------------------------------------------------------------
-- Vertex
--------------------------------------------------------------------------------

data Vertex lib = Vertex
  { position :: Vec3 lib
  , normal :: Vec3 lib
  , uv :: Vec2 lib
  }
  deriving (Generic)

deriving instance (FloatVecs lib) => Show (Vertex lib)

deriving instance (FloatVecs lib) => Eq (Vertex lib)

instance (MemoryLayoutRules l, GAlignedStorable l (Rep (Vertex lib))) => AlignedStorable l (Vertex lib)

genVertex :: (FloatVecs lib) => Gen (Vertex lib)
genVertex = Vertex <$> genVec <*> genVec <*> genVec

-- GLSL: struct Vertex (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std140 (Vertex lib)) where
  sizeOf _ = 48
  alignment _ = 16
  peek ptr =
    Oracle
      <$> (Vertex <$> peekVecAt Std140 ptr 0 <*> peekVecAt Std140 ptr 16 <*> peekVecAt Std140 ptr 32)
  poke ptr (Oracle Vertex{..}) = do
    pokeVecAt Std140 ptr 0 position
    pokeVecAt Std140 ptr 16 normal
    pokeVecAt Std140 ptr 32 uv

-- GLSL: struct Vertex (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std430 (Vertex lib)) where
  sizeOf _ = 48
  alignment _ = 16
  peek ptr =
    Oracle
      <$> (Vertex <$> peekVecAt Std430 ptr 0 <*> peekVecAt Std430 ptr 16 <*> peekVecAt Std430 ptr 32)
  poke ptr (Oracle Vertex{..}) = do
    pokeVecAt Std430 ptr 0 position
    pokeVecAt Std430 ptr 16 normal
    pokeVecAt Std430 ptr 32 uv

-- GLSL: struct Vertex (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Scalar (Vertex lib)) where
  sizeOf _ = 32
  alignment _ = 4
  peek ptr =
    Oracle
      <$> (Vertex <$> peekVecAt Scalar ptr 0 <*> peekVecAt Scalar ptr 12 <*> peekVecAt Scalar ptr 24)
  poke ptr (Oracle Vertex{..}) = do
    pokeVecAt Scalar ptr 0 position
    pokeVecAt Scalar ptr 12 normal
    pokeVecAt Scalar ptr 24 uv

vertexSuite :: forall lib. (FloatVecs lib) => Proxy lib -> TestTree
vertexSuite _ =
  sutTests
    "Vertex"
    (genVertex @lib)
    [at @Std140 (Just 40), at @Std430 (Just 40), at @Scalar (Just 32)]

--------------------------------------------------------------------------------
-- FinalPaddingTest
--------------------------------------------------------------------------------

data FinalPaddingTest lib = FinalPaddingTest
  { fptA :: Vec4 lib
  , fptB :: Int32
  }
  deriving (Generic)

deriving instance (FloatVecs lib) => Show (FinalPaddingTest lib)

deriving instance (FloatVecs lib) => Eq (FinalPaddingTest lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (FinalPaddingTest lib)))
  => AlignedStorable l (FinalPaddingTest lib)

genFinalPaddingTest :: (FloatVecs lib) => Gen (FinalPaddingTest lib)
genFinalPaddingTest = FinalPaddingTest <$> genVec <*> genInt

-- GLSL: struct FinalPaddingTest (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std140 (FinalPaddingTest lib)) where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = Oracle <$> (FinalPaddingTest <$> peekVecAt Std140 ptr 0 <*> peekByteOff ptr 16)
  poke ptr (Oracle FinalPaddingTest{..}) = do
    pokeVecAt Std140 ptr 0 fptA
    pokeByteOff ptr 16 fptB

-- GLSL: struct FinalPaddingTest (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std430 (FinalPaddingTest lib)) where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = Oracle <$> (FinalPaddingTest <$> peekVecAt Std430 ptr 0 <*> peekByteOff ptr 16)
  poke ptr (Oracle FinalPaddingTest{..}) = do
    pokeVecAt Std430 ptr 0 fptA
    pokeByteOff ptr 16 fptB

-- GLSL: struct FinalPaddingTest (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Scalar (FinalPaddingTest lib)) where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr = Oracle <$> (FinalPaddingTest <$> peekVecAt Scalar ptr 0 <*> peekByteOff ptr 16)
  poke ptr (Oracle FinalPaddingTest{..}) = do
    pokeVecAt Scalar ptr 0 fptA
    pokeByteOff ptr 16 fptB

finalPaddingTestSuite :: forall lib. (FloatVecs lib) => Proxy lib -> TestTree
finalPaddingTestSuite _ =
  sutTests
    "FinalPaddingTest"
    (genFinalPaddingTest @lib)
    [at @Std140 Nothing, at @Std430 Nothing, at @Scalar Nothing]

--------------------------------------------------------------------------------
-- Nested
--------------------------------------------------------------------------------

data Nested lib = Nested
  { nestedV3 :: Vec3 lib
  , nestedSimple :: Simple
  , nestedMixed :: MixedAlign
  , nestedBoolTest :: BoolTest
  }
  deriving (Generic)

deriving instance (FloatVecs lib) => Show (Nested lib)

deriving instance (FloatVecs lib) => Eq (Nested lib)

instance (MemoryLayoutRules l, GAlignedStorable l (Rep (Nested lib))) => AlignedStorable l (Nested lib)

genNested :: (FloatVecs lib) => Gen (Nested lib)
genNested = Nested <$> genVec <*> genSimple <*> genMixedAlign <*> genBoolTest

-- GLSL: struct Nested (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std140 (Nested lib)) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( Nested
              <$> peekVecAt Std140 ptr 0
              <*> peekOracleAt @Std140 ptr 16
              <*> peekOracleAt @Std140 ptr 32
              <*> peekOracleAt @Std140 ptr 64
          )
  poke ptr (Oracle Nested{..}) = do
    pokeVecAt Std140 ptr 0 nestedV3
    pokeOracleAt @Std140 ptr 16 nestedSimple
    pokeOracleAt @Std140 ptr 32 nestedMixed
    pokeOracleAt @Std140 ptr 64 nestedBoolTest

-- GLSL: struct Nested (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std430 (Nested lib)) where
  sizeOf _ = 64
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( Nested
              <$> peekVecAt Std430 ptr 0
              <*> peekOracleAt @Std430 ptr 12
              <*> peekOracleAt @Std430 ptr 24
              <*> peekOracleAt @Std430 ptr 48
          )
  poke ptr (Oracle Nested{..}) = do
    pokeVecAt Std430 ptr 0 nestedV3
    pokeOracleAt @Std430 ptr 12 nestedSimple
    pokeOracleAt @Std430 ptr 24 nestedMixed
    pokeOracleAt @Std430 ptr 48 nestedBoolTest

-- GLSL: struct Nested (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Scalar (Nested lib)) where
  sizeOf _ = 56
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( Nested
              <$> peekVecAt Scalar ptr 0
              <*> peekOracleAt @Scalar ptr 12
              <*> peekOracleAt @Scalar ptr 24
              <*> peekOracleAt @Scalar ptr 44
          )
  poke ptr (Oracle Nested{..}) = do
    pokeVecAt Scalar ptr 0 nestedV3
    pokeOracleAt @Scalar ptr 12 nestedSimple
    pokeOracleAt @Scalar ptr 24 nestedMixed
    pokeOracleAt @Scalar ptr 44 nestedBoolTest

nestedSuite :: forall lib. (FloatVecs lib) => Proxy lib -> TestTree
nestedSuite _ =
  sutTests
    "Nested"
    (genNested @lib)
    [at @Std140 Nothing, at @Std430 Nothing, at @Scalar Nothing]

--------------------------------------------------------------------------------
-- NestedNewtype
--------------------------------------------------------------------------------

newtype NestedNewtype lib = NestedNewtype (Nested lib)
  deriving (Generic)

deriving instance (FloatVecs lib) => Show (NestedNewtype lib)

deriving instance (FloatVecs lib) => Eq (NestedNewtype lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (NestedNewtype lib)))
  => AlignedStorable l (NestedNewtype lib)

genNestedNewtype :: (FloatVecs lib) => Gen (NestedNewtype lib)
genNestedNewtype = NestedNewtype <$> genNested

deriving via
  Oracle l (Nested lib)
  instance
    (Storable (Oracle l (Nested lib))) => Storable (Oracle l (NestedNewtype lib))

nestedNewtypeSuite :: forall lib. (FloatVecs lib) => Proxy lib -> TestTree
nestedNewtypeSuite _ =
  sutTests
    "NestedNewtype"
    (genNestedNewtype @lib)
    [at @Std140 Nothing, at @Std430 Nothing, at @Scalar Nothing]

--------------------------------------------------------------------------------
-- VectorStruct
--------------------------------------------------------------------------------

data VectorStruct lib = VectorStruct
  { vsA :: Float
  , vsB :: SSV.Vector 2 (Vec3 lib)
  , vsC :: Int32
  }
  deriving (Generic)

deriving instance (FloatVecs lib) => Show (VectorStruct lib)

deriving instance (FloatVecs lib) => Eq (VectorStruct lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (VectorStruct lib)))
  => AlignedStorable l (VectorStruct lib)

genVectorStruct :: (FloatVecs lib) => Gen (VectorStruct lib)
genVectorStruct = VectorStruct <$> genFloat <*> SSV.replicateM genVec <*> genInt

-- GLSL: struct VectorStruct (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std140 (VectorStruct lib)) where
  sizeOf _ = 64
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( VectorStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 16 (peekVecAt Std140) ptr 16
              <*> peekByteOff ptr 48
          )
  poke ptr (Oracle VectorStruct{..}) = do
    pokeByteOff ptr 0 vsA
    pokeArrayAt 16 (pokeVecAt Std140) ptr 16 vsB
    pokeByteOff ptr 48 vsC

-- GLSL: struct VectorStruct (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std430 (VectorStruct lib)) where
  sizeOf _ = 64
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( VectorStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 16 (peekVecAt Std430) ptr 16
              <*> peekByteOff ptr 48
          )
  poke ptr (Oracle VectorStruct{..}) = do
    pokeByteOff ptr 0 vsA
    pokeArrayAt 16 (pokeVecAt Std430) ptr 16 vsB
    pokeByteOff ptr 48 vsC

-- GLSL: struct VectorStruct (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Scalar (VectorStruct lib)) where
  sizeOf _ = 32
  alignment _ = 4
  peek ptr =
    Oracle
      <$> ( VectorStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 12 (peekVecAt Scalar) ptr 4
              <*> peekByteOff ptr 28
          )
  poke ptr (Oracle VectorStruct{..}) = do
    pokeByteOff ptr 0 vsA
    pokeArrayAt 12 (pokeVecAt Scalar) ptr 4 vsB
    pokeByteOff ptr 28 vsC

vectorStructSuite :: forall lib. (FloatVecs lib) => Proxy lib -> TestTree
vectorStructSuite _ =
  sutTests
    "VectorStruct"
    (genVectorStruct @lib)
    [at @Std140 Nothing, at @Std430 Nothing, at @Scalar Nothing]

--------------------------------------------------------------------------------
-- TailVec3
--------------------------------------------------------------------------------

data TailVec3 lib = TailVec3
  { tvA :: Float
  , tvB :: Vec3 lib
  }
  deriving (Generic)

deriving instance (FloatVecs lib) => Show (TailVec3 lib)

deriving instance (FloatVecs lib) => Eq (TailVec3 lib)

instance (MemoryLayoutRules l, GAlignedStorable l (Rep (TailVec3 lib))) => AlignedStorable l (TailVec3 lib)

genTailVec3 :: (FloatVecs lib) => Gen (TailVec3 lib)
genTailVec3 = TailVec3 <$> genFloat <*> genVec

-- GLSL: struct TailVec3 (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std140 (TailVec3 lib)) where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = Oracle <$> (TailVec3 <$> peekByteOff ptr 0 <*> peekVecAt Std140 ptr 16)
  poke ptr (Oracle TailVec3{..}) = do
    pokeByteOff ptr 0 tvA
    pokeVecAt Std140 ptr 16 tvB

-- GLSL: struct TailVec3 (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Std430 (TailVec3 lib)) where
  sizeOf _ = 32
  alignment _ = 16
  peek ptr = Oracle <$> (TailVec3 <$> peekByteOff ptr 0 <*> peekVecAt Std430 ptr 16)
  poke ptr (Oracle TailVec3{..}) = do
    pokeByteOff ptr 0 tvA
    pokeVecAt Std430 ptr 16 tvB

-- GLSL: struct TailVec3 (glsl/Conformance.glsl)
instance (FloatVecs lib) => Storable (Oracle Scalar (TailVec3 lib)) where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = Oracle <$> (TailVec3 <$> peekByteOff ptr 0 <*> peekVecAt Scalar ptr 4)
  poke ptr (Oracle TailVec3{..}) = do
    pokeByteOff ptr 0 tvA
    pokeVecAt Scalar ptr 4 tvB

tailVec3Suite :: forall lib. (FloatVecs lib) => Proxy lib -> TestTree
tailVec3Suite _ =
  sutTests
    "TailVec3"
    (genTailVec3 @lib)
    [at @Std140 (Just 28), at @Std430 (Just 28), at @Scalar (Just 16)]
