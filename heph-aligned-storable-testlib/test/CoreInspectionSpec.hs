{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module CoreInspectionSpec where

import Test.Tasty

import Foreign.GPU.Storable.Aligned.Test

$(inspectAlignedStorable "nestedPrim" [t|NestedPrim|] True)
$(inspectAlignedStorable "stress" [t|Stress DeepNest|] True)

test_inspection :: TestTree
test_inspection = testGroup "core" [nestedPrimInspection, stressInspection]
