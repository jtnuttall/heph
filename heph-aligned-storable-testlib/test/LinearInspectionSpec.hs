{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -fsimpl-tick-factor=200 -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

module LinearInspectionSpec where

import Test.Tasty

import Foreign.GPU.Storable.Aligned.Test
import LinearConformanceSpec (L)

$(inspectAlignedStorable "nested" [t|Nested L|] False)
$(inspectAlignedStorable "stress" [t|Stress (Insanity L)|] False)

test_inspection :: TestTree
test_inspection = testGroup "linear" [nestedInspection, stressInspection]
