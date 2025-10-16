{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Foreign.GPU.Marshal.AlignedSpec where

import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class
import Data.Coerce
import Data.Foldable
import Data.Traversable
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Storable.Sized as SSV
import Foreign
import GHC.Generics
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Linear (V4 (..))

import Foreign.GPU.Marshal.Aligned
import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.AlignedSpec

getBytes :: Int -> Ptr a -> IO [Word8]
getBytes n = peekArray @Word8 n . castPtr

genSV :: (Storable a) => Gen a -> Gen (SV.Vector a)
genSV gen = SV.fromList <$> Gen.list (Range.linear 1 1000) gen

hprop_dynamic_vector_copy_byte_identity :: Property
hprop_dynamic_vector_copy_byte_identity = property do
  vec <- forAll $ genSV (Strided @Std140 <$> genSimple)
  let totalBytes = SV.length vec * sizeOf (SV.head vec)
  sutBytes <- liftIO $ SV.unsafeWith vec (getBytes totalBytes . castPtr)
  oracleBytes <- liftIO $ bracket (mallocBytes totalBytes) free \ptr -> do
    alignedCopyVector (AlignedPtr ptr) vec
    getBytes totalBytes ptr
  sutBytes === oracleBytes

hprop_dynamic_vector_copy :: Property
hprop_dynamic_vector_copy = property do
  values <- forAll $ Gen.list (Range.linear 1 100) genSimple
  let n = length values
      vec = SV.fromList $ map (Strided @Std140) values
      stride = sizeOf (SV.head vec)
      totalBytes = n * stride

  oracleResult <- liftIO $ bracket (mallocBytes totalBytes) free \ptr -> do
    alignedCopyVector (AlignedPtr (castPtr ptr)) vec
    for [0 .. n - 1] \i ->
      coerce <$> peekByteOff @SimpleStd140 ptr (i * stride)

  packedResult <- liftIO $ bracket (mallocBytes totalBytes) free \ptr -> do
    alignedCopyVector (AlignedPtr (castPtr ptr)) vec
    for [0 .. n - 1] \i ->
      coerce <$> peekByteOff @(Packed Std140 Simple) ptr (i * stride)

  oracleResult === values
  packedResult === values

-- | A struct containing a fixed-size array, designed to be optimized
-- by AlignedArray.
data StructWithArray (layout :: MemoryLayout) = StructWithArray
  { fieldA :: Float
  , fieldB :: AlignedArray layout 4 (V4 Float)
  }
  deriving (Generic)

deriving instance
  (MemoryLayoutRules layout, AlignedStorable layout (V4 Float)) => Show (StructWithArray layout)

deriving instance
  (MemoryLayoutRules layout, AlignedStorable layout (V4 Float)) => Eq (StructWithArray layout)

instance AlignedStorable Std140 (StructWithArray Std140)

hprop_dynamic_vector_with_aligned_array_copy :: Property
hprop_dynamic_vector_with_aligned_array_copy = property do
  values <-
    forAll $
      Gen.list
        (Range.linear 1 100)
        (StructWithArray <$> genFloat <*> (mkAlignedArray @Std140 <$> SSV.replicateM (genV genFloat)))

  let vec = SV.fromList $ map (Strided @Std140) values
      structStride = sizeOf (SV.head vec)
      totalBytes = length values * structStride

  bracket (liftIO $ mallocBytes totalBytes) (liftIO . free) \ptr -> do
    alignedCopyVector (AlignedPtr (castPtr ptr)) vec
    for_ (zip [0 ..] values) \(i, StructWithArray expectedFieldA expectedFieldB) -> do
      let structBaseOffset = i * structStride
          fieldAOffset = structBaseOffset
      actualFieldA <- liftIO $ peekByteOff @Float ptr fieldAOffset
      actualFieldA === expectedFieldA

      let fieldBBaseOffset = structBaseOffset + 16
          vec4Stride = 16
          expectedVecs = unAlignedArray expectedFieldB

      for_ [0 .. 3] \j -> do
        let fieldBElementOffset = fieldBBaseOffset + (fromIntegral j * vec4Stride)
        actualVec4 <- liftIO $ peekByteOff @(V4 Float) ptr fieldBElementOffset
        let expectedVec4 = SSV.index expectedVecs j
        actualVec4 === coerce expectedVec4
