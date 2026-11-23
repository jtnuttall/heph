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
-- Description : Structs of arrays and nested structs, and their oracles
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Structs under test that combine arrays of vectors, nested structs and arrays
-- of nested structs.
module Foreign.GPU.Storable.Aligned.Test.Sut.Composite (
  ComplexVectorStruct (ComplexVectorStruct),
  genComplexVectorStruct,
  complexVectorStructSuite,
  Insanity (Insanity),
  genInsanity,
  insanitySuite,
  IntVecStruct (IntVecStruct),
  genIntVecStruct,
  intVecStructSuite,
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
import Test.Tasty (TestTree)

import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.Aligned.Test.Glsl
import Foreign.GPU.Storable.Aligned.Test.Harness
import Foreign.GPU.Storable.Aligned.Test.Sut.FloatVec
import Foreign.GPU.Storable.Aligned.Test.Vocab

--------------------------------------------------------------------------------
-- ComplexVectorStruct
--------------------------------------------------------------------------------

data ComplexVectorStruct lib = ComplexVectorStruct
  { cvsA :: Float
  , cvsB :: SSV.Vector 3 (U16Vec3 lib)
  , cvsC :: SSV.Vector 2 (DVec2 lib)
  , cvsD :: Float
  , cvsE :: Double
  }
  deriving (Generic)

deriving instance (SmallIntVecs lib, DoubleVecs lib) => Show (ComplexVectorStruct lib)

deriving instance (SmallIntVecs lib, DoubleVecs lib) => Eq (ComplexVectorStruct lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (ComplexVectorStruct lib)))
  => AlignedStorable l (ComplexVectorStruct lib)

genComplexVectorStruct :: (SmallIntVecs lib, DoubleVecs lib) => Gen (ComplexVectorStruct lib)
genComplexVectorStruct =
  ComplexVectorStruct
    <$> genFloat
    <*> SSV.replicateM genVec
    <*> SSV.replicateM genVec
    <*> genFloat
    <*> genFloat

-- GLSL: struct ComplexVectorStruct (glsl/Conformance.glsl)
instance (SmallIntVecs lib, DoubleVecs lib) => Storable (Oracle Std140 (ComplexVectorStruct lib)) where
  sizeOf _ = 112
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( ComplexVectorStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 16 (peekVecAt Std140) ptr 16
              <*> peekArrayAt 16 (peekVecAt Std140) ptr 64
              <*> peekByteOff ptr 96
              <*> peekByteOff ptr 104
          )
  poke ptr (Oracle ComplexVectorStruct{..}) = do
    pokeByteOff ptr 0 cvsA
    pokeArrayAt 16 (pokeVecAt Std140) ptr 16 cvsB
    pokeArrayAt 16 (pokeVecAt Std140) ptr 64 cvsC
    pokeByteOff ptr 96 cvsD
    pokeByteOff ptr 104 cvsE

-- GLSL: struct ComplexVectorStruct (glsl/Conformance.glsl)
instance (SmallIntVecs lib, DoubleVecs lib) => Storable (Oracle Std430 (ComplexVectorStruct lib)) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( ComplexVectorStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 8 (peekVecAt Std430) ptr 8
              <*> peekArrayAt 16 (peekVecAt Std430) ptr 32
              <*> peekByteOff ptr 64
              <*> peekByteOff ptr 72
          )
  poke ptr (Oracle ComplexVectorStruct{..}) = do
    pokeByteOff ptr 0 cvsA
    pokeArrayAt 8 (pokeVecAt Std430) ptr 8 cvsB
    pokeArrayAt 16 (pokeVecAt Std430) ptr 32 cvsC
    pokeByteOff ptr 64 cvsD
    pokeByteOff ptr 72 cvsE

-- GLSL: struct ComplexVectorStruct (glsl/Conformance.glsl)
instance (SmallIntVecs lib, DoubleVecs lib) => Storable (Oracle Scalar (ComplexVectorStruct lib)) where
  sizeOf _ = 72
  alignment _ = 8
  peek ptr =
    Oracle
      <$> ( ComplexVectorStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 6 (peekVecAt Scalar) ptr 4
              <*> peekArrayAt 16 (peekVecAt Scalar) ptr 24
              <*> peekByteOff ptr 56
              <*> peekByteOff ptr 64
          )
  poke ptr (Oracle ComplexVectorStruct{..}) = do
    pokeByteOff ptr 0 cvsA
    pokeArrayAt 6 (pokeVecAt Scalar) ptr 4 cvsB
    pokeArrayAt 16 (pokeVecAt Scalar) ptr 24 cvsC
    pokeByteOff ptr 56 cvsD
    pokeByteOff ptr 64 cvsE

complexVectorStructSuite :: forall lib. (SmallIntVecs lib, DoubleVecs lib) => Proxy lib -> TestTree
complexVectorStructSuite _ =
  sutTests
    "ComplexVectorStruct"
    (genComplexVectorStruct @lib)
    [at @Std140 Nothing, at @Std430 Nothing, at @Scalar Nothing]

--------------------------------------------------------------------------------
-- Insanity
--------------------------------------------------------------------------------

data Insanity lib = Insanity
  { ina :: Float
  , inb :: SV.Vector 7 (ComplexVectorStruct lib)
  , inc :: Word8
  , ind :: Word64
  , ine :: Word16
  , inf :: BVec3 lib
  , ing :: SV.Vector 5 (Nested lib)
  , inh :: U16Vec3 lib
  , ini :: SV.Vector 5 (SV.Vector 2 (SV.Vector 4 (Nested lib)))
  , inj :: Bool
  , ink :: DVec3 lib
  }
  deriving (Generic)

deriving instance
  (FloatVecs lib, SmallIntVecs lib, DoubleVecs lib, BoolVecs lib) => Show (Insanity lib)

deriving instance
  (FloatVecs lib, SmallIntVecs lib, DoubleVecs lib, BoolVecs lib) => Eq (Insanity lib)

instance (MemoryLayoutRules l, GAlignedStorable l (Rep (Insanity lib))) => AlignedStorable l (Insanity lib)

genInsanity :: (FloatVecs lib, SmallIntVecs lib, DoubleVecs lib, BoolVecs lib) => Gen (Insanity lib)
genInsanity =
  Insanity
    <$> genFloat
    <*> SV.replicateM genComplexVectorStruct
    <*> genInt
    <*> genInt
    <*> genInt
    <*> genVec
    <*> SV.replicateM genNested
    <*> genVec
    <*> SV.replicateM (SV.replicateM (SV.replicateM genNested))
    <*> Gen.bool
    <*> genVec

-- GLSL: struct Insanity (glsl/Conformance.glsl)
instance
  (FloatVecs lib, SmallIntVecs lib, DoubleVecs lib, BoolVecs lib)
  => Storable (Oracle Std140 (Insanity lib))
  where
  sizeOf _ = 4512
  alignment _ = 32

  peek ptr =
    Oracle
      <$> ( Insanity
              <$> peekByteOff ptr 0
              <*> peekSizedVector (Proxy @(Oracle Std140 (ComplexVectorStruct lib))) ptr 16
              <*> peekByteOff ptr 800
              <*> peekByteOff ptr 808
              <*> peekByteOff ptr 816
              <*> peekVecAt Std140 ptr 832
              <*> peekSizedVector (Proxy @(Oracle Std140 (Nested lib))) ptr 848
              <*> peekVecAt Std140 ptr 1248
              <*> peekSized3DVector (Proxy @(Oracle Std140 (Nested lib))) ptr 1264
              <*> peekByteOff ptr 4464
              <*> peekVecAt Std140 ptr 4480
          )

  poke ptr (Oracle Insanity{..}) = do
    pokeByteOff ptr 0 ina
    pokeSizedVector ptr 16 (SV.map (Oracle @Std140) inb)
    pokeByteOff ptr 800 inc
    pokeByteOff ptr 808 ind
    pokeByteOff ptr 816 ine
    pokeVecAt Std140 ptr 832 inf
    pokeSizedVector ptr 848 (SV.map (Oracle @Std140) ing)
    pokeVecAt Std140 ptr 1248 inh
    pokeSized3DVector (Proxy @(Oracle Std140 (Nested lib))) ptr 1264 ini
    pokeByteOff ptr 4464 inj
    pokeVecAt Std140 ptr 4480 ink

-- GLSL: struct Insanity (glsl/Conformance.glsl)
instance
  (FloatVecs lib, SmallIntVecs lib, DoubleVecs lib, BoolVecs lib)
  => Storable (Oracle Std430 (Insanity lib))
  where
  sizeOf _ = 3584
  alignment _ = 32

  peek ptr =
    Oracle
      <$> ( Insanity
              <$> peekByteOff ptr 0
              <*> peekSizedVector (Proxy @(Oracle Std430 (ComplexVectorStruct lib))) ptr 16
              <*> peekByteOff ptr 576
              <*> peekByteOff ptr 584
              <*> peekByteOff ptr 592
              <*> peekVecAt Std430 ptr 608
              <*> peekSizedVector (Proxy @(Oracle Std430 (Nested lib))) ptr 624
              <*> peekVecAt Std430 ptr 944
              <*> peekSized3DVector (Proxy @(Oracle Std430 (Nested lib))) ptr 960
              <*> peekByteOff ptr 3520
              <*> peekVecAt Std430 ptr 3552
          )

  poke ptr (Oracle Insanity{..}) = do
    pokeByteOff ptr 0 ina
    pokeSizedVector ptr 16 (SV.map (Oracle @Std430) inb)
    pokeByteOff ptr 576 inc
    pokeByteOff ptr 584 ind
    pokeByteOff ptr 592 ine
    pokeVecAt Std430 ptr 608 inf
    pokeSizedVector ptr 624 (SV.map (Oracle @Std430) ing)
    pokeVecAt Std430 ptr 944 inh
    pokeSized3DVector (Proxy @(Oracle Std430 (Nested lib))) ptr 960 ini
    pokeByteOff ptr 3520 inj
    pokeVecAt Std430 ptr 3552 ink

-- GLSL: struct Insanity (glsl/Conformance.glsl)
instance
  (FloatVecs lib, SmallIntVecs lib, DoubleVecs lib, BoolVecs lib)
  => Storable (Oracle Scalar (Insanity lib))
  where
  sizeOf _ = 3096
  alignment _ = 8

  peek ptr =
    Oracle
      <$> ( Insanity
              <$> peekByteOff ptr 0
              <*> peekSizedVector (Proxy @(Oracle Scalar (ComplexVectorStruct lib))) ptr 8
              <*> peekByteOff ptr 512
              <*> peekByteOff ptr 520
              <*> peekByteOff ptr 528
              <*> peekVecAt Scalar ptr 532
              <*> peekSizedVector (Proxy @(Oracle Scalar (Nested lib))) ptr 544
              <*> peekVecAt Scalar ptr 820
              <*> peekSized3DVector (Proxy @(Oracle Scalar (Nested lib))) ptr 832
              <*> peekByteOff ptr 3068
              <*> peekVecAt Scalar ptr 3072
          )

  poke ptr (Oracle Insanity{..}) = do
    pokeByteOff ptr 0 ina
    pokeSizedVector ptr 8 (SV.map (Oracle @Scalar) inb)
    pokeByteOff ptr 512 inc
    pokeByteOff ptr 520 ind
    pokeByteOff ptr 528 ine
    pokeVecAt Scalar ptr 532 inf
    pokeSizedVector ptr 544 (SV.map (Oracle @Scalar) ing)
    pokeVecAt Scalar ptr 820 inh
    pokeSized3DVector (Proxy @(Oracle Scalar (Nested lib))) ptr 832 ini
    pokeByteOff ptr 3068 inj
    pokeVecAt Scalar ptr 3072 ink

insanitySuite
  :: forall lib. (FloatVecs lib, SmallIntVecs lib, DoubleVecs lib, BoolVecs lib) => Proxy lib -> TestTree
insanitySuite _ =
  sutTestsWith
    (withShrinks 10)
    defaultTripOpts
    "Insanity"
    (genInsanity @lib)
    [at @Std140 Nothing, at @Std430 Nothing, at @Scalar Nothing]

--------------------------------------------------------------------------------
-- IntVecStruct
--------------------------------------------------------------------------------

data IntVecStruct lib = IntVecStruct
  { ivA :: Float
  , ivB :: SSV.Vector 2 (IVec3 lib)
  , ivC :: UVec2 lib
  , ivD :: UVec3 lib
  , ivE :: Int32
  }
  deriving (Generic)

deriving instance (Int32Vecs lib) => Show (IntVecStruct lib)

deriving instance (Int32Vecs lib) => Eq (IntVecStruct lib)

instance
  (MemoryLayoutRules l, GAlignedStorable l (Rep (IntVecStruct lib)))
  => AlignedStorable l (IntVecStruct lib)

genIntVecStruct :: (Int32Vecs lib) => Gen (IntVecStruct lib)
genIntVecStruct = IntVecStruct <$> genFloat <*> SSV.replicateM genVec <*> genVec <*> genVec <*> genInt

-- GLSL: struct IntVecStruct (glsl/Conformance.glsl)
instance (Int32Vecs lib) => Storable (Oracle Std140 (IntVecStruct lib)) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( IntVecStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 16 (peekVecAt Std140) ptr 16
              <*> peekVecAt Std140 ptr 48
              <*> peekVecAt Std140 ptr 64
              <*> peekByteOff ptr 76
          )
  poke ptr (Oracle IntVecStruct{..}) = do
    pokeByteOff ptr 0 ivA
    pokeArrayAt 16 (pokeVecAt Std140) ptr 16 ivB
    pokeVecAt Std140 ptr 48 ivC
    pokeVecAt Std140 ptr 64 ivD
    pokeByteOff ptr 76 ivE

-- GLSL: struct IntVecStruct (glsl/Conformance.glsl)
instance (Int32Vecs lib) => Storable (Oracle Std430 (IntVecStruct lib)) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    Oracle
      <$> ( IntVecStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 16 (peekVecAt Std430) ptr 16
              <*> peekVecAt Std430 ptr 48
              <*> peekVecAt Std430 ptr 64
              <*> peekByteOff ptr 76
          )
  poke ptr (Oracle IntVecStruct{..}) = do
    pokeByteOff ptr 0 ivA
    pokeArrayAt 16 (pokeVecAt Std430) ptr 16 ivB
    pokeVecAt Std430 ptr 48 ivC
    pokeVecAt Std430 ptr 64 ivD
    pokeByteOff ptr 76 ivE

-- GLSL: struct IntVecStruct (glsl/Conformance.glsl)
instance (Int32Vecs lib) => Storable (Oracle Scalar (IntVecStruct lib)) where
  sizeOf _ = 52
  alignment _ = 4
  peek ptr =
    Oracle
      <$> ( IntVecStruct
              <$> peekByteOff ptr 0
              <*> peekArrayAt 12 (peekVecAt Scalar) ptr 4
              <*> peekVecAt Scalar ptr 28
              <*> peekVecAt Scalar ptr 36
              <*> peekByteOff ptr 48
          )
  poke ptr (Oracle IntVecStruct{..}) = do
    pokeByteOff ptr 0 ivA
    pokeArrayAt 12 (pokeVecAt Scalar) ptr 4 ivB
    pokeVecAt Scalar ptr 28 ivC
    pokeVecAt Scalar ptr 36 ivD
    pokeByteOff ptr 48 ivE

intVecStructSuite :: forall lib. (Int32Vecs lib) => Proxy lib -> TestTree
intVecStructSuite _ =
  sutTests
    "IntVecStruct"
    (genIntVecStruct @lib)
    [at @Std140 (Just 80), at @Std430 (Just 80), at @Scalar (Just 52)]
