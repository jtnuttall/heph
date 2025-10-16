{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module Foreign.GPU.Storable.AlignedInspectionSpec where

import Data.Proxy
import GHC.Generics (Generic)
import Test.Tasty
import Test.Tasty.Inspection

import Foreign.GPU.Storable.Aligned
import Foreign.GPU.Storable.AlignedSpec

-- | Any type that pokes an array incurs allocations, even if it uses `copyBytes` internally.
-- I suspect llvm will unroll the loop if it is used, but I can't prove this by inspecting core.
data Stress
  = Stress
      Insanity
      Insanity
      Insanity
      Insanity
      Insanity
      Insanity
      Insanity
      Insanity
      Insanity
      Insanity
      Insanity
      Insanity
      Insanity
  deriving (Generic)

instance AlignedStorable Std140 Stress
instance AlignedStorable Std430 Stress
instance AlignedStorable Scalar Stress

packedAlignedSizeOf140Stress :: Proxy Std140 -> Proxy Stress -> Int
packedAlignedSizeOf140Stress = packedAlignedSizeOf
packedAlignedSizeOf430Stress :: Proxy Std430 -> Proxy Stress -> Int
packedAlignedSizeOf430Stress = packedAlignedSizeOf
packedAlignedSizeOfScalarStress :: Proxy Scalar -> Proxy Stress -> Int
packedAlignedSizeOfScalarStress = packedAlignedSizeOf

alignedAlignment140Stress :: Proxy Std140 -> Proxy Stress -> Int
alignedAlignment140Stress = alignedAlignment
alignedAlignment430Stress :: Proxy Std430 -> Proxy Stress -> Int
alignedAlignment430Stress = alignedAlignment
alignedAlignmentScalarStress :: Proxy Scalar -> Proxy Stress -> Int
alignedAlignmentScalarStress = alignedAlignment

alignedSizeOf140Stress :: Proxy Std140 -> Proxy Stress -> Int
alignedSizeOf140Stress = alignedSizeOf
alignedSizeOf430Stress :: Proxy Std430 -> Proxy Stress -> Int
alignedSizeOf430Stress = alignedSizeOf
alignedSizeOfScalarStress :: Proxy Scalar -> Proxy Stress -> Int
alignedSizeOfScalarStress = alignedSizeOf

alignedPoke140Stress :: AlignedPtr Std140 Stress -> Stress -> IO ()
alignedPoke140Stress = alignedPoke
alignedPoke430Stress :: AlignedPtr Std430 Stress -> Stress -> IO ()
alignedPoke430Stress = alignedPoke
alignedPokeScalarStress :: AlignedPtr Scalar Stress -> Stress -> IO ()
alignedPokeScalarStress = alignedPoke

alignedPeek140Stress :: AlignedPtr Std140 Stress -> IO Stress
alignedPeek140Stress = alignedPeek
alignedPeek430Stress :: AlignedPtr Std430 Stress -> IO Stress
alignedPeek430Stress = alignedPeek
alignedPeekScalarStress :: AlignedPtr Scalar Stress -> IO Stress
alignedPeekScalarStress = alignedPeek

packedAlignedSizeOf140Nested :: Proxy Std140 -> Proxy Nested -> Int
packedAlignedSizeOf140Nested = packedAlignedSizeOf
packedAlignedSizeOf430Nested :: Proxy Std430 -> Proxy Nested -> Int
packedAlignedSizeOf430Nested = packedAlignedSizeOf
packedAlignedSizeOfScalarNested :: Proxy Scalar -> Proxy Nested -> Int
packedAlignedSizeOfScalarNested = packedAlignedSizeOf

alignedSizeOf140Nested :: Proxy Std140 -> Proxy Nested -> Int
alignedSizeOf140Nested = alignedSizeOf
alignedSizeOf430Nested :: Proxy Std430 -> Proxy Nested -> Int
alignedSizeOf430Nested = alignedSizeOf
alignedSizeOfScalarNested :: Proxy Scalar -> Proxy Nested -> Int
alignedSizeOfScalarNested = alignedSizeOf

alignedAlignment140Nested :: Proxy Std140 -> Proxy Nested -> Int
alignedAlignment140Nested = alignedAlignment
alignedAlignment430Nested :: Proxy Std430 -> Proxy Nested -> Int
alignedAlignment430Nested = alignedAlignment
alignedAlignmentScalarNested :: Proxy Scalar -> Proxy Nested -> Int
alignedAlignmentScalarNested = alignedAlignment

alignedPoke140Nested :: AlignedPtr Std140 Nested -> Nested -> IO ()
alignedPoke140Nested = alignedPoke
alignedPoke430Nested :: AlignedPtr Std430 Nested -> Nested -> IO ()
alignedPoke430Nested = alignedPoke
alignedPokeScalarNested :: AlignedPtr Scalar Nested -> Nested -> IO ()
alignedPokeScalarNested = alignedPoke

alignedPeek140Nested :: AlignedPtr Std140 Nested -> IO Nested
alignedPeek140Nested = alignedPeek
alignedPeek430Nested :: AlignedPtr Std430 Nested -> IO Nested
alignedPeek430Nested = alignedPeek
alignedPeekScalarNested :: AlignedPtr Scalar Nested -> IO Nested
alignedPeekScalarNested = alignedPeek

test_inspection :: [TestTree]
test_inspection =
  [ testGroup
      "Stress"
      [ testGroup
          "std140"
          [ $(inspectTest $ hasNoGenerics 'packedAlignedSizeOf140Stress)
          , $(inspectTest $ hasNoTypeClasses 'packedAlignedSizeOf140Stress)
          , $( inspectTest $
                'packedAlignedSizeOf140Stress
                  `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedSizeOf140Stress)
          , $(inspectTest $ hasNoTypeClasses 'alignedSizeOf140Stress)
          , $( inspectTest $
                'alignedSizeOf140Stress
                  `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedAlignment140Stress)
          , $(inspectTest $ hasNoTypeClasses 'alignedAlignment140Stress)
          , $( inspectTest $
                'alignedAlignment140Stress
                  `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPoke140Stress)
          , $(inspectTest $ hasNoTypeClasses 'alignedPoke140Stress)
          , $( inspectTest $
                'alignedPoke140Stress
                  `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPeek140Stress)
          , $(inspectTest $ hasNoTypeClasses 'alignedPeek140Stress)
          , $( inspectTest $
                'alignedPeek140Stress
                  `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          ]
      , testGroup
          "std430"
          [ $(inspectTest $ hasNoGenerics 'packedAlignedSizeOf430Stress)
          , $(inspectTest $ hasNoTypeClasses 'packedAlignedSizeOf430Stress)
          , $( inspectTest $
                'packedAlignedSizeOf140Stress
                  `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedSizeOf430Stress)
          , $(inspectTest $ hasNoTypeClasses 'alignedSizeOf430Stress)
          , $( inspectTest $
                'alignedSizeOf430Stress
                  `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedAlignment430Stress)
          , $(inspectTest $ hasNoTypeClasses 'alignedAlignment430Stress)
          , $( inspectTest $
                'alignedAlignment430Stress
                  `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPoke430Stress)
          , $(inspectTest $ hasNoTypeClasses 'alignedPoke430Stress)
          , $( inspectTest $
                'alignedPoke430Stress
                  `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPeek430Stress)
          , $(inspectTest $ hasNoTypeClasses 'alignedPeek430Stress)
          , $( inspectTest $
                'alignedPeek430Stress
                  `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          ]
      , testGroup
          "scalar"
          [ $(inspectTest $ hasNoGenerics 'packedAlignedSizeOfScalarStress)
          , $(inspectTest $ hasNoTypeClasses 'packedAlignedSizeOfScalarStress)
          , $( inspectTest $
                'packedAlignedSizeOfScalarStress `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedSizeOfScalarStress)
          , $(inspectTest $ hasNoTypeClasses 'alignedSizeOfScalarStress)
          , $( inspectTest $
                'alignedSizeOfScalarStress `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedAlignmentScalarStress)
          , $(inspectTest $ hasNoTypeClasses 'alignedAlignmentScalarStress)
          , $( inspectTest $
                'alignedAlignmentScalarStress `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPokeScalarStress)
          , $(inspectTest $ hasNoTypeClasses 'alignedPokeScalarStress)
          , $( inspectTest $
                'alignedPokeScalarStress `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPeekScalarStress)
          , $(inspectTest $ hasNoTypeClasses 'alignedPeekScalarStress)
          , $( inspectTest $
                'alignedPeekScalarStress `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          ]
      ]
  , testGroup
      "Nested"
      [ testGroup
          "std140"
          [ $(inspectTest $ hasNoGenerics 'packedAlignedSizeOf140Nested)
          , $(inspectTest $ mkObligation 'packedAlignedSizeOf140Nested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'packedAlignedSizeOf140Nested)
          , $( inspectTest $
                'packedAlignedSizeOf140Nested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedSizeOf140Nested)
          , $(inspectTest $ mkObligation 'alignedSizeOf140Nested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedSizeOf140Nested)
          , $( inspectTest $
                'alignedSizeOf140Nested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedAlignment140Nested)
          , $(inspectTest $ mkObligation 'alignedAlignment140Nested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedAlignment140Nested)
          , $( inspectTest $
                'alignedAlignment140Nested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPoke140Nested)
          , $(inspectTest $ mkObligation 'alignedPoke140Nested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedPoke140Nested)
          , $( inspectTest $ 'alignedPoke140Nested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPeek140Nested)
          , $(inspectTest $ mkObligation 'alignedPeek140Nested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedPeek140Nested)
          , $( inspectTest $ 'alignedPeek140Nested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          ]
      , testGroup
          "std430"
          [ $(inspectTest $ hasNoGenerics 'packedAlignedSizeOf430Nested)
          , $(inspectTest $ mkObligation 'packedAlignedSizeOf430Nested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'packedAlignedSizeOf430Nested)
          , $( inspectTest $
                'packedAlignedSizeOf430Nested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedSizeOf430Nested)
          , $(inspectTest $ mkObligation 'alignedSizeOf430Nested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedSizeOf430Nested)
          , $( inspectTest $
                'alignedSizeOf430Nested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedAlignment430Nested)
          , $(inspectTest $ mkObligation 'alignedAlignment430Nested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedAlignment430Nested)
          , $( inspectTest $
                'alignedAlignment430Nested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPoke430Nested)
          , $(inspectTest $ mkObligation 'alignedPoke430Nested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedPoke430Nested)
          , $( inspectTest $ 'alignedPoke430Nested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPeek430Nested)
          , $(inspectTest $ mkObligation 'alignedPeek430Nested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedPeek430Nested)
          , $( inspectTest $ 'alignedPeek430Nested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          ]
      , testGroup
          "scalar"
          [ $(inspectTest $ hasNoGenerics 'packedAlignedSizeOfScalarNested)
          , $(inspectTest $ mkObligation 'packedAlignedSizeOfScalarNested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'packedAlignedSizeOfScalarNested)
          , $( inspectTest $
                'packedAlignedSizeOfScalarNested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedSizeOfScalarNested)
          , $(inspectTest $ mkObligation 'alignedSizeOfScalarNested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedSizeOfScalarNested)
          , $( inspectTest $
                'alignedSizeOfScalarNested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedAlignmentScalarNested)
          , $(inspectTest $ mkObligation 'alignedAlignmentScalarNested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedAlignmentScalarNested)
          , $( inspectTest $
                'alignedAlignmentScalarNested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPokeScalarNested)
          , $(inspectTest $ mkObligation 'alignedPokeScalarNested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedPokeScalarNested)
          , $( inspectTest $
                'alignedPokeScalarNested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          , $(inspectTest $ hasNoGenerics 'alignedPeekScalarNested)
          , $(inspectTest $ mkObligation 'alignedPeekScalarNested NoAllocation)
          , $(inspectTest $ hasNoTypeClasses 'alignedPeekScalarNested)
          , $( inspectTest $
                'alignedPeekScalarNested `doesNotUseAnyOf` ['galignedSize, 'galignedPoke, 'galignedPeek]
             )
          ]
      ]
  ]
