{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Description : Conformance tests for "Foreign.GPU.Marshal.Aligned"
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
module Foreign.GPU.Storable.Aligned.Test.Marshal (
  getBytes,
  genSV,
  hprop_dynamic_vector_copy_byte_identity,
  hprop_dynamic_vector_copy,
)
where

import Control.Exception.Lifted (bracket)
import Control.Monad.IO.Class
import Data.Coerce
import Data.Traversable
import qualified Data.Vector.Storable as SV
import Foreign
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Foreign.GPU.Marshal.Aligned
import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.Aligned.Test.Sut.Prim

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
