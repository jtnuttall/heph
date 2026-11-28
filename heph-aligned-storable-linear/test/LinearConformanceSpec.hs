{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module LinearConformanceSpec where

import Data.Int
import Data.Proxy
import Data.Word
import Foreign.GPU.Storable.Aligned.Test
import Linear
import Numeric.Half
import Test.Tasty

import Foreign.GPU.Storable.Aligned.Linear ()

data L

instance (Component a) => GlslVec (V2 a) where
  type VecElem (V2 a) = a
  type VecDim (V2 a) = 2
  toComponents (V2 x y) = [x, y]
  fromComponents [x, y] = V2 x y
  fromComponents xs = componentArity 2 xs

instance (Component a) => GlslVec (V3 a) where
  type VecElem (V3 a) = a
  type VecDim (V3 a) = 3
  toComponents (V3 x y z) = [x, y, z]
  fromComponents [x, y, z] = V3 x y z
  fromComponents xs = componentArity 3 xs

instance (Component a) => GlslVec (V4 a) where
  type VecElem (V4 a) = a
  type VecDim (V4 a) = 4
  toComponents (V4 x y z w) = [x, y, z, w]
  fromComponents [x, y, z, w] = V4 x y z w
  fromComponents xs = componentArity 4 xs

instance (GlslVec (v a)) => GlslMat (V2 (v a)) where
  type MatElem (V2 (v a)) = VecElem (v a)
  type MatVecs (V2 (v a)) = 2
  type MatVecDim (V2 (v a)) = VecDim (v a)
  toVectors (V2 a b) = map toComponents [a, b]
  fromVectors [a, b] = V2 (fromComponents a) (fromComponents b)
  fromVectors xs = componentArity 2 xs

instance (GlslVec (v a)) => GlslMat (V3 (v a)) where
  type MatElem (V3 (v a)) = VecElem (v a)
  type MatVecs (V3 (v a)) = 3
  type MatVecDim (V3 (v a)) = VecDim (v a)
  toVectors (V3 a b c) = map toComponents [a, b, c]
  fromVectors [a, b, c] = V3 (fromComponents a) (fromComponents b) (fromComponents c)
  fromVectors xs = componentArity 3 xs

instance (GlslVec (v a)) => GlslMat (V4 (v a)) where
  type MatElem (V4 (v a)) = VecElem (v a)
  type MatVecs (V4 (v a)) = 4
  type MatVecDim (V4 (v a)) = VecDim (v a)
  toVectors (V4 a b c d) = map toComponents [a, b, c, d]
  fromVectors [a, b, c, d] = V4 (fromComponents a) (fromComponents b) (fromComponents c) (fromComponents d)
  fromVectors xs = componentArity 4 xs

instance FloatVecs L where
  type Vec2 L = V2 Float
  type Vec3 L = V3 Float
  type Vec4 L = V4 Float

instance FloatMat4 L where
  type Mat4 L = M44 Float

instance FloatMats L where
  type Mat2 L = M22 Float
  type Mat3 L = M33 Float
  type Mat3x4 L = M43 Float

instance DoubleVecs L where
  type DVec2 L = V2 Double
  type DVec3 L = V3 Double
  type DVec4 L = V4 Double

instance DoubleMat4 L where
  type DMat4 L = M44 Double

instance DoubleMats L where
  type DMat3 L = M33 Double
  type DMat3x4 L = M43 Double
  type DMat4x3 L = M34 Double
  type DMat2x4 L = M42 Double

instance HalfMats L where
  type F16Mat2x3 L = M32 Half
  type F16Mat3x2 L = M23 Half
  type F16Mat4 L = M44 Half
  type F16Mat4x2 L = M24 Half

instance SmallIntVecs L where
  type U16Vec3 L = V3 Word16

instance BoolVecs L where
  type BVec3 L = V3 Bool

instance Int32Vecs L where
  type IVec2 L = V2 Int32
  type IVec3 L = V3 Int32
  type IVec4 L = V4 Int32
  type UVec2 L = V2 Word32
  type UVec3 L = V3 Word32
  type UVec4 L = V4 Word32

test_conformance :: TestTree
test_conformance = testGroup "linear" (allSuites (Proxy @L))
