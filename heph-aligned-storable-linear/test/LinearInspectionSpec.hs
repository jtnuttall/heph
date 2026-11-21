{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -O -fsimpl-tick-factor=4000 -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module LinearInspectionSpec where

import Test.Tasty

import Foreign.GPU.Storable.Aligned.Linear ()
import Foreign.GPU.Storable.Aligned.Test
import LinearConformanceSpec (L)

$(inspectAlignedStorable "nested" [t|Nested L|] True)
$(inspectAlignedStorable "stress" [t|Stress (Insanity L)|] True)

test_inspection :: TestTree
test_inspection = testGroup "linear" [nestedInspection, stressInspection]
