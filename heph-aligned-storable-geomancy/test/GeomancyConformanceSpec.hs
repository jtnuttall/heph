module GeomancyConformanceSpec where

import Data.Int
import Data.Proxy
import Data.Word
import Foreign.GPU.Storable.Aligned.Test
import Geomancy.DMat4 qualified as DMat4
import Geomancy.DVec2 qualified as DVec2
import Geomancy.DVec3 qualified as DVec3
import Geomancy.DVec4 qualified as DVec4
import Geomancy.I64Vec2 qualified as I64Vec2
import Geomancy.I64Vec3 qualified as I64Vec3
import Geomancy.I64Vec4 qualified as I64Vec4
import Geomancy.IVec2 qualified as IVec2
import Geomancy.IVec3 qualified as IVec3
import Geomancy.IVec4 qualified as IVec4
import Geomancy.Mat4 qualified as Mat4
import Geomancy.U64Vec2 qualified as U64Vec2
import Geomancy.U64Vec3 qualified as U64Vec3
import Geomancy.U64Vec4 qualified as U64Vec4
import Geomancy.UVec2 qualified as UVec2
import Geomancy.UVec3 qualified as UVec3
import Geomancy.UVec4 qualified as UVec4
import Geomancy.Vec2 qualified as Vec2
import Geomancy.Vec3 qualified as Vec3
import Geomancy.Vec4 qualified as Vec4
import Test.Tasty

import Foreign.GPU.Storable.Aligned.Geomancy ()

data G

instance GlslVec Vec2.Vec2 where
  type VecElem Vec2.Vec2 = Float
  type VecDim Vec2.Vec2 = 2
  toComponents v = Vec2.withVec2 v \x y -> [x, y]
  fromComponents [x, y] = Vec2.vec2 x y
  fromComponents xs = componentArity 2 xs

instance GlslVec Vec3.Vec3 where
  type VecElem Vec3.Vec3 = Float
  type VecDim Vec3.Vec3 = 3
  toComponents v = Vec3.withVec3 v \x y z -> [x, y, z]
  fromComponents [x, y, z] = Vec3.vec3 x y z
  fromComponents xs = componentArity 3 xs

instance GlslVec Vec4.Vec4 where
  type VecElem Vec4.Vec4 = Float
  type VecDim Vec4.Vec4 = 4
  toComponents v = Vec4.withVec4 v \x y z w -> [x, y, z, w]
  fromComponents [x, y, z, w] = Vec4.vec4 x y z w
  fromComponents xs = componentArity 4 xs

instance GlslVec DVec2.DVec2 where
  type VecElem DVec2.DVec2 = Double
  type VecDim DVec2.DVec2 = 2
  toComponents v = DVec2.withDVec2 v \x y -> [x, y]
  fromComponents [x, y] = DVec2.dvec2 x y
  fromComponents xs = componentArity 2 xs

instance GlslVec DVec3.DVec3 where
  type VecElem DVec3.DVec3 = Double
  type VecDim DVec3.DVec3 = 3
  toComponents v = DVec3.withDVec3 v \x y z -> [x, y, z]
  fromComponents [x, y, z] = DVec3.dvec3 x y z
  fromComponents xs = componentArity 3 xs

instance GlslVec DVec4.DVec4 where
  type VecElem DVec4.DVec4 = Double
  type VecDim DVec4.DVec4 = 4
  toComponents v = DVec4.withDVec4 v \x y z w -> [x, y, z, w]
  fromComponents [x, y, z, w] = DVec4.dvec4 x y z w
  fromComponents xs = componentArity 4 xs

instance GlslVec IVec2.IVec2 where
  type VecElem IVec2.IVec2 = Int32
  type VecDim IVec2.IVec2 = 2
  toComponents v = IVec2.withIVec2 v \x y -> [x, y]
  fromComponents [x, y] = IVec2.ivec2 x y
  fromComponents xs = componentArity 2 xs

instance GlslVec IVec3.IVec3 where
  type VecElem IVec3.IVec3 = Int32
  type VecDim IVec3.IVec3 = 3
  toComponents v = IVec3.withIVec3 v \x y z -> [x, y, z]
  fromComponents [x, y, z] = IVec3.ivec3 x y z
  fromComponents xs = componentArity 3 xs

instance GlslVec IVec4.IVec4 where
  type VecElem IVec4.IVec4 = Int32
  type VecDim IVec4.IVec4 = 4
  toComponents v = IVec4.withIVec4 v \x y z w -> [x, y, z, w]
  fromComponents [x, y, z, w] = IVec4.ivec4 x y z w
  fromComponents xs = componentArity 4 xs

instance GlslVec UVec2.UVec2 where
  type VecElem UVec2.UVec2 = Word32
  type VecDim UVec2.UVec2 = 2
  toComponents v = UVec2.withUVec2 v \x y -> [x, y]
  fromComponents [x, y] = UVec2.uvec2 x y
  fromComponents xs = componentArity 2 xs

instance GlslVec UVec3.UVec3 where
  type VecElem UVec3.UVec3 = Word32
  type VecDim UVec3.UVec3 = 3
  toComponents v = UVec3.withUVec3 v \x y z -> [x, y, z]
  fromComponents [x, y, z] = UVec3.uvec3 x y z
  fromComponents xs = componentArity 3 xs

instance GlslVec UVec4.UVec4 where
  type VecElem UVec4.UVec4 = Word32
  type VecDim UVec4.UVec4 = 4
  toComponents v = UVec4.withUVec4 v \x y z w -> [x, y, z, w]
  fromComponents [x, y, z, w] = UVec4.uvec4 x y z w
  fromComponents xs = componentArity 4 xs

instance GlslVec I64Vec2.I64Vec2 where
  type VecElem I64Vec2.I64Vec2 = Int64
  type VecDim I64Vec2.I64Vec2 = 2
  toComponents v = I64Vec2.withI64Vec2 v \x y -> [x, y]
  fromComponents [x, y] = I64Vec2.i64vec2 x y
  fromComponents xs = componentArity 2 xs

instance GlslVec I64Vec3.I64Vec3 where
  type VecElem I64Vec3.I64Vec3 = Int64
  type VecDim I64Vec3.I64Vec3 = 3
  toComponents v = I64Vec3.withI64Vec3 v \x y z -> [x, y, z]
  fromComponents [x, y, z] = I64Vec3.i64vec3 x y z
  fromComponents xs = componentArity 3 xs

instance GlslVec I64Vec4.I64Vec4 where
  type VecElem I64Vec4.I64Vec4 = Int64
  type VecDim I64Vec4.I64Vec4 = 4
  toComponents v = I64Vec4.withI64Vec4 v \x y z w -> [x, y, z, w]
  fromComponents [x, y, z, w] = I64Vec4.i64vec4 x y z w
  fromComponents xs = componentArity 4 xs

instance GlslVec U64Vec2.U64Vec2 where
  type VecElem U64Vec2.U64Vec2 = Word64
  type VecDim U64Vec2.U64Vec2 = 2
  toComponents v = U64Vec2.withU64Vec2 v \x y -> [x, y]
  fromComponents [x, y] = U64Vec2.u64vec2 x y
  fromComponents xs = componentArity 2 xs

instance GlslVec U64Vec3.U64Vec3 where
  type VecElem U64Vec3.U64Vec3 = Word64
  type VecDim U64Vec3.U64Vec3 = 3
  toComponents v = U64Vec3.withU64Vec3 v \x y z -> [x, y, z]
  fromComponents [x, y, z] = U64Vec3.u64vec3 x y z
  fromComponents xs = componentArity 3 xs

instance GlslVec U64Vec4.U64Vec4 where
  type VecElem U64Vec4.U64Vec4 = Word64
  type VecDim U64Vec4.U64Vec4 = 4
  toComponents v = U64Vec4.withU64Vec4 v \x y z w -> [x, y, z, w]
  fromComponents [x, y, z, w] = U64Vec4.u64vec4 x y z w
  fromComponents xs = componentArity 4 xs

instance GlslMat Mat4.Mat4 where
  type MatElem Mat4.Mat4 = Float
  type MatVecs Mat4.Mat4 = 4
  type MatVecDim Mat4.Mat4 = 4
  toVectors = Mat4.toListRowMajor2d
  fromVectors [[a, b, c, d], [e, f, g, h], [i, j, k, l], [m, n, o, p]] =
    Mat4.rowMajor a b c d e f g h i j k l m n o p
  fromVectors xs = componentArity 4 xs

instance GlslMat DMat4.DMat4 where
  type MatElem DMat4.DMat4 = Double
  type MatVecs DMat4.DMat4 = 4
  type MatVecDim DMat4.DMat4 = 4
  toVectors = DMat4.toListRowMajor2d
  fromVectors [[a, b, c, d], [e, f, g, h], [i, j, k, l], [m, n, o, p]] =
    DMat4.rowMajor a b c d e f g h i j k l m n o p
  fromVectors xs = componentArity 4 xs

instance FloatVecs G where
  type Vec2 G = Vec2.Vec2
  type Vec3 G = Vec3.Vec3
  type Vec4 G = Vec4.Vec4

instance FloatMat4 G where
  type Mat4 G = Mat4.Mat4

instance DoubleVecs G where
  type DVec2 G = DVec2.DVec2
  type DVec3 G = DVec3.DVec3
  type DVec4 G = DVec4.DVec4

instance DoubleMat4 G where
  type DMat4 G = DMat4.DMat4

instance Int32Vecs G where
  type IVec2 G = IVec2.IVec2
  type IVec3 G = IVec3.IVec3
  type IVec4 G = IVec4.IVec4
  type UVec2 G = UVec2.UVec2
  type UVec3 G = UVec3.UVec3
  type UVec4 G = UVec4.UVec4

test_conformance :: TestTree
test_conformance =
  testGroup "geomancy" $
    floatVecSuites p <> [uniformsSuite p, dUniformsSuite p, intVecStructSuite p]
 where
  p = Proxy @G
