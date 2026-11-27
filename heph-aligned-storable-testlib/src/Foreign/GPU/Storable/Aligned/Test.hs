-- |
-- Description : Conformance tests for heph-aligned-storable
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- The harness, structs under test and their hand-written oracles shared by the
-- test suites of heph-aligned-storable and its instance packages.
--
-- Each struct under test has a GLSL twin in @glsl/Conformance.glsl@, and an
-- oracle (t'Oracle') per layout whose offsets and sizes are copied from
-- glslang's output for that twin, and whose alignment follows from the layout
-- rules. The tests check the derived
-- 'Foreign.GPU.Storable.Aligned.AlignedStorable' instance against the oracle.
--
-- A package that provides instances for a vector library runs the shared
-- suites by naming its types:
--
-- > data L
-- >
-- > instance FloatVecs L where
-- >   type Vec2 L = V2 Float
-- >   type Vec3 L = V3 Float
-- >   type Vec4 L = V4 Float
-- >
-- > instance GlslVec (V3 a) where ...  -- likewise V2, V4
-- >
-- > test_conformance :: TestTree
-- > test_conformance = testGroup "linear" (floatVecSuites (Proxy @L))
module Foreign.GPU.Storable.Aligned.Test (
  -- * Suites
  primSuites,
  floatVecSuites,
  allSuites,

  -- * Building blocks
  module Foreign.GPU.Storable.Aligned.Test.Harness,
  module Foreign.GPU.Storable.Aligned.Test.Glsl,
  module Foreign.GPU.Storable.Aligned.Test.Vocab,
  module Foreign.GPU.Storable.Aligned.Test.Inspection,

  -- * Structs under test
  module Foreign.GPU.Storable.Aligned.Test.Sut.Prim,
  module Foreign.GPU.Storable.Aligned.Test.Sut.FloatVec,
  module Foreign.GPU.Storable.Aligned.Test.Sut.Matrix,
  module Foreign.GPU.Storable.Aligned.Test.Sut.Composite,
  module Foreign.GPU.Storable.Aligned.Test.Marshal,
)
where

import Data.Proxy (Proxy)
import Test.Tasty (TestTree)

import Foreign.GPU.Storable.Aligned.Test.Glsl
import Foreign.GPU.Storable.Aligned.Test.Harness
import Foreign.GPU.Storable.Aligned.Test.Inspection
import Foreign.GPU.Storable.Aligned.Test.Marshal
import Foreign.GPU.Storable.Aligned.Test.Sut.Composite
import Foreign.GPU.Storable.Aligned.Test.Sut.FloatVec
import Foreign.GPU.Storable.Aligned.Test.Sut.Matrix
import Foreign.GPU.Storable.Aligned.Test.Sut.Prim
import Foreign.GPU.Storable.Aligned.Test.Vocab

-- | The suites that need no vector library: structs of scalars, nested
-- structs and arrays of them, and the marshal tests over them.
primSuites :: [TestTree]
primSuites =
  [ baseSuite
  , simpleSuite
  , layoutBugSuite
  , largePrimitivesSuite
  , mixedAlignSuite
  , boolTestSuite
  , nestedPrimSuite
  , arrayStructSuite
  , deepNestSuite
  , marshalSuite
  ]

-- | The suites that need only @vec2@, @vec3@ and @vec4@.
floatVecSuites :: (FloatVecs lib) => Proxy lib -> [TestTree]
floatVecSuites p =
  [ std140TestSuite p
  , paddingTestSuite p
  , finalPaddingTestSuite p
  , vertexSuite p
  , nestedSuite p
  , nestedNewtypeSuite p
  , vectorStructSuite p
  , tailVec3Suite p
  , marshalVecSuite p
  ]

-- | Every shared suite.
allSuites
  :: ( FloatVecs lib
     , FloatMats lib
     , DoubleVecs lib
     , DoubleMats lib
     , HalfMats lib
     , SmallIntVecs lib
     , BoolVecs lib
     , Int32Vecs lib
     )
  => Proxy lib
  -> [TestTree]
allSuites p =
  floatVecSuites p
    <> [ uniformsSuite p
       , dUniformsSuite p
       , complexPaddingSuite p
       , d2Suite p
       , doubleTroubleSuite p
       , testNonSquareSuite p
       , kitchenSinkSuite p
       , complexVectorStructSuite p
       , insanitySuite p
       , intVecStructSuite p
       ]
