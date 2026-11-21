module CoreConformanceSpec where

import Test.Tasty

import Foreign.GPU.Storable.Aligned.Test

test_core :: TestTree
test_core = testGroup "core" primSuites
