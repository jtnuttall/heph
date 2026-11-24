{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- Description : Conformance tests for "Foreign.GPU.Marshal.Aligned"
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Copies of whole vectors of structs, and structs holding an t'AlignedArray'.
module Foreign.GPU.Storable.Aligned.Test.Marshal (
  -- * Suites
  marshalSuite,
  marshalVecSuite,

  -- * Structs
  StructWithArrayF (StructWithArrayF),
  StructWithArrayV (StructWithArrayV),

  -- * Helpers
  getBytes,
  genSV,
)
where

import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class
import Data.Coerce
import Data.Foldable
import Data.Proxy
import Data.Traversable
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Sized as SSV
import Foreign
import Foreign.GPU.Marshal.Aligned
import Foreign.GPU.Storable.Aligned
import GHC.Generics
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

import Foreign.GPU.Storable.Aligned.Test.Glsl
import Foreign.GPU.Storable.Aligned.Test.Harness
import Foreign.GPU.Storable.Aligned.Test.Sut.Prim
import Foreign.GPU.Storable.Aligned.Test.Vocab

getBytes :: Int -> Ptr a -> IO [Word8]
getBytes n = peekArray @Word8 n . castPtr

genSV :: (Storable a) => Gen a -> Gen (SV.Vector a)
genSV gen = SV.fromList <$> Gen.list (Range.linear 1 1000) gen

-- | The marshal tests that need no vector library.
marshalSuite :: TestTree
marshalSuite =
  testGroup
    "Marshal"
    [ testProperty "dynamic vector copy byte identity" prop_dynamic_vector_copy_byte_identity
    , testProperty "dynamic vector copy" prop_dynamic_vector_copy
    , testProperty
        "dynamic vector with aligned float array copy"
        prop_dynamic_vector_with_aligned_float_array_copy
    , testGroup "StructWithArrayF" $
        layoutCaseTests
          id
          noByteImage
          genStructWithArrayF
          (at @Std140 @(StructWithArrayF Std140) (Just 80))
          <> layoutCaseTests
            id
            noByteImage
            genStructWithArrayF
            (at @Std430 @(StructWithArrayF Std430) (Just 20))
          <> layoutCaseTests
            id
            noByteImage
            genStructWithArrayF
            (at @Scalar @(StructWithArrayF Scalar) (Just 20))
    ]
 where
  noByteImage = defaultTripOpts{checkByteImage = False}

prop_dynamic_vector_copy_byte_identity :: Property
prop_dynamic_vector_copy_byte_identity = property do
  vec <- forAll $ genSV (Strided @Std140 <$> genSimple)
  let totalBytes = SV.length vec * sizeOf (SV.head vec)
  sutBytes <- liftIO $ SV.unsafeWith vec (getBytes totalBytes . castPtr)
  oracleBytes <- liftIO $ bracket (mallocBytes totalBytes) free \ptr -> do
    alignedCopyVector (AlignedPtr ptr) vec
    getBytes totalBytes ptr
  sutBytes === oracleBytes

prop_dynamic_vector_copy :: Property
prop_dynamic_vector_copy = property do
  values <- forAll $ Gen.list (Range.linear 1 100) genSimple
  let n = length values
      vec = SV.fromList $ map (Strided @Std140) values
      stride = sizeOf (SV.head vec)
      totalBytes = n * stride

  oracleResult <- liftIO $ bracket (mallocBytes totalBytes) free \ptr -> do
    alignedCopyVector (AlignedPtr (castPtr ptr)) vec
    for [0 .. n - 1] \i ->
      peekOracleAt @Std140 ptr (i * stride)

  packedResult <- liftIO $ bracket (mallocBytes totalBytes) free \ptr -> do
    alignedCopyVector (AlignedPtr (castPtr ptr)) vec
    for [0 .. n - 1] \i ->
      coerce <$> peekByteOff @(Packed Std140 Simple) ptr (i * stride)

  oracleResult === values
  packedResult === values

--------------------------------------------------------------------------------
-- StructWithArrayF
--------------------------------------------------------------------------------

-- | A struct containing a fixed-size array of scalars, copied as one block by
-- t'AlignedArray'.
data StructWithArrayF (layout :: MemoryLayout) = StructWithArrayF
  { swafA :: Float
  , swafB :: AlignedArray layout 4 Float
  }
  deriving (Generic)

deriving instance (AlignedStorable layout Float) => Show (StructWithArrayF layout)

deriving instance (AlignedStorable layout Float) => Eq (StructWithArrayF layout)

instance AlignedStorable Std140 (StructWithArrayF Std140)
instance AlignedStorable Std430 (StructWithArrayF Std430)
instance AlignedStorable Scalar (StructWithArrayF Scalar)

genStructWithArrayF :: (AlignedStorable layout Float) => Gen (StructWithArrayF layout)
genStructWithArrayF = StructWithArrayF <$> genFloat <*> (mkAlignedArray <$> SSV.replicateM genFloat)

-- GLSL: struct StructWithArrayF (glsl/Conformance.glsl)
instance Storable (Oracle Std140 (StructWithArrayF Std140)) where
  sizeOf _ = 80
  alignment _ = 16
  peek ptr =
    Oracle
      <$> (StructWithArrayF <$> peekByteOff ptr 0 <*> (AlignedArray <$> peekArrayAt 16 peekStrided ptr 16))
  poke ptr (Oracle StructWithArrayF{..}) = do
    pokeByteOff ptr 0 swafA
    pokeArrayAt 16 pokeStrided ptr 16 (unAlignedArray swafB)

-- GLSL: struct StructWithArrayF (glsl/Conformance.glsl)
instance Storable (Oracle Std430 (StructWithArrayF Std430)) where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr =
    Oracle
      <$> (StructWithArrayF <$> peekByteOff ptr 0 <*> (AlignedArray <$> peekArrayAt 4 peekStrided ptr 4))
  poke ptr (Oracle StructWithArrayF{..}) = do
    pokeByteOff ptr 0 swafA
    pokeArrayAt 4 pokeStrided ptr 4 (unAlignedArray swafB)

-- GLSL: struct StructWithArrayF (glsl/Conformance.glsl)
instance Storable (Oracle Scalar (StructWithArrayF Scalar)) where
  sizeOf _ = 20
  alignment _ = 4
  peek ptr =
    Oracle
      <$> (StructWithArrayF <$> peekByteOff ptr 0 <*> (AlignedArray <$> peekArrayAt 4 peekStrided ptr 4))
  poke ptr (Oracle StructWithArrayF{..}) = do
    pokeByteOff ptr 0 swafA
    pokeArrayAt 4 pokeStrided ptr 4 (unAlignedArray swafB)

peekStrided :: (Storable a) => Ptr b -> Int -> IO (Strided layout a)
peekStrided ptr off = Strided <$> peekByteOff ptr off

pokeStrided :: (Storable a) => Ptr b -> Int -> Strided layout a -> IO ()
pokeStrided ptr off = pokeByteOff ptr off . unStrided

prop_dynamic_vector_with_aligned_float_array_copy :: Property
prop_dynamic_vector_with_aligned_float_array_copy = property do
  values <- forAll $ Gen.list (Range.linear 1 100) (genStructWithArrayF @Std140)

  let vec = SV.fromList $ map (Strided @Std140) values
      structStride = sizeOf (SV.head vec)
      totalBytes = length values * structStride

  bracket (liftIO $ mallocBytes totalBytes) (liftIO . free) \ptr -> do
    alignedCopyVector (AlignedPtr (castPtr ptr)) vec
    for_ (zip [0 ..] values) \(i, expected) -> do
      actual <- liftIO $ peekOracleAt @Std140 ptr (i * structStride)
      actual === expected

--------------------------------------------------------------------------------
-- StructWithArrayV
--------------------------------------------------------------------------------

-- | A struct containing a fixed-size array, designed to be optimized
-- by AlignedArray.
data StructWithArrayV lib (layout :: MemoryLayout) = StructWithArrayV
  { swavA :: Float
  , swavB :: AlignedArray layout 4 (Vec4 lib)
  }
  deriving (Generic)

deriving instance
  (FloatVecs lib, MemoryLayoutRules layout, AlignedStorable layout (Vec4 lib))
  => Show (StructWithArrayV lib layout)

deriving instance
  (FloatVecs lib, MemoryLayoutRules layout, AlignedStorable layout (Vec4 lib))
  => Eq (StructWithArrayV lib layout)

instance (FloatVecs lib) => AlignedStorable Std140 (StructWithArrayV lib Std140)
instance (FloatVecs lib) => AlignedStorable Std430 (StructWithArrayV lib Std430)
instance (FloatVecs lib) => AlignedStorable Scalar (StructWithArrayV lib Scalar)

-- | The marshal tests for a vector library: copies of a struct holding an
-- t'AlignedArray' of @vec4@.
marshalVecSuite :: forall lib. (FloatVecs lib) => Proxy lib -> TestTree
marshalVecSuite _ =
  testGroup
    "Marshal"
    [ testProperty
        "dynamic vector with aligned array copy"
        (prop_dynamic_vector_with_aligned_array_copy @lib)
    , testGroup
        "StructWithArrayV"
        [ testCase "std140 packed size" $
            packedAlignedSizeOf (Proxy @Std140) (Proxy @(StructWithArrayV lib Std140)) @?= 80
        , testCase "std430 packed size" $
            packedAlignedSizeOf (Proxy @Std430) (Proxy @(StructWithArrayV lib Std430)) @?= 80
        , testCase "scalar packed size" $
            packedAlignedSizeOf (Proxy @Scalar) (Proxy @(StructWithArrayV lib Scalar)) @?= 68
        ]
    ]

prop_dynamic_vector_with_aligned_array_copy :: forall lib. (FloatVecs lib) => Property
prop_dynamic_vector_with_aligned_array_copy = property do
  values <-
    forAll $
      Gen.list
        (Range.linear 1 100)
        (StructWithArrayV @lib <$> genFloat <*> (mkAlignedArray @Std140 <$> SSV.replicateM genVec))

  let vec = SV.fromList $ map (Strided @Std140) values
      structStride = sizeOf (SV.head vec)
      totalBytes = length values * structStride

  bracket (liftIO $ mallocBytes totalBytes) (liftIO . free) \ptr -> do
    alignedCopyVector (AlignedPtr (castPtr ptr)) vec
    for_ (zip [0 ..] values) \(i, StructWithArrayV expectedFieldA expectedFieldB) -> do
      let structBaseOffset = i * structStride
          fieldAOffset = structBaseOffset
      actualFieldA <- liftIO $ peekByteOff @Float ptr fieldAOffset
      actualFieldA === expectedFieldA

      let fieldBBaseOffset = structBaseOffset + 16
          vec4Stride = 16
          expectedVecs = unAlignedArray expectedFieldB

      for_ [0 .. 3] \j -> do
        let fieldBElementOffset = fieldBBaseOffset + (fromIntegral j * vec4Stride)
        actualVec4 <- liftIO $ peekVecAt @(Vec4 lib) Std140 ptr fieldBElementOffset
        let expectedVec4 = SSV.index expectedVecs j
        actualVec4 === coerce expectedVec4
