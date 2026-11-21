{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module GeomancyInspectionSpec where

import Test.Tasty

import Foreign.GPU.Storable.Aligned.Test
import GeomancyConformanceSpec (G)

$(inspectAlignedStorable "nested" [t|Nested G|] True)
$(inspectAlignedStorable "uniforms" [t|Uniforms G|] True)

test_inspection :: TestTree
test_inspection = testGroup "geomancy" [nestedInspection, uniformsInspection]
