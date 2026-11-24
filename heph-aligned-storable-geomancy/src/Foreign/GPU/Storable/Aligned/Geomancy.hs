{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : AlignedStorable instances for geomancy
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- 'Foreign.GPU.Storable.Aligned.AlignedStorable' instances for
-- [geomancy](https://hackage.haskell.org/package/geomancy). Import this module
-- for the instances:
--
-- > import Foreign.GPU.Storable.Aligned
-- > import Foreign.GPU.Storable.Aligned.Geomancy ()
module Foreign.GPU.Storable.Aligned.Geomancy () where

import Data.Int
import Data.Word
import Foreign.GPU.Storable.Aligned ()
import Foreign.GPU.Storable.Aligned.TH
import Geomancy.DMat4 (DMat4)
import Geomancy.DQuaternion (DQuaternion)
import Geomancy.DTransform (DTransform (..))
import Geomancy.DVec2 (DVec2, dvec2, withDVec2)
import Geomancy.DVec3 (DVec3, dvec3, withDVec3)
import qualified Geomancy.DVec3 as DVec3
import Geomancy.DVec4 (DVec4, dvec4, withDVec4)
import Geomancy.I64Vec2 (I64Vec2, i64vec2, withI64Vec2)
import Geomancy.I64Vec3 (I64Vec3, i64vec3, withI64Vec3)
import qualified Geomancy.I64Vec3 as I64Vec3
import Geomancy.I64Vec4 (I64Vec4, i64vec4, withI64Vec4)
import Geomancy.IVec2 (IVec2, ivec2, withIVec2)
import Geomancy.IVec3 (IVec3, ivec3, withIVec3)
import qualified Geomancy.IVec3 as IVec3
import Geomancy.IVec4 (IVec4, ivec4, withIVec4)
import Geomancy.Mat4 (Mat4)
import Geomancy.Point (Point (..))
import Geomancy.Quaternion (Quaternion)
import Geomancy.Transform (Transform (..))
import Geomancy.U64Vec2 (U64Vec2, u64vec2, withU64Vec2)
import Geomancy.U64Vec3 (U64Vec3, u64vec3, withU64Vec3)
import qualified Geomancy.U64Vec3 as U64Vec3
import Geomancy.U64Vec4 (U64Vec4, u64vec4, withU64Vec4)
import Geomancy.UVec2 (UVec2, uvec2, withUVec2)
import Geomancy.UVec3 (UVec3, uvec3, withUVec3)
import qualified Geomancy.UVec3 as UVec3
import Geomancy.UVec4 (UVec4, uvec4, withUVec4)
import Geomancy.Vec2 (Vec2, vec2, withVec2)
import Geomancy.Vec3 (Vec3, vec3, withVec3)
import qualified Geomancy.Vec3 as Vec3
import Geomancy.Vec4 (Vec4, vec4, withVec4)
import Language.Haskell.TH

deriveAll $
  let family e rows = [(e, n, t, mk, el) | (n, t, mk, el) <- rows]
      vectors =
        concat
          [ family
              [t|Float|]
              [ (Two, ''Vec2, 'vec2, 'withVec2)
              , (Three, ''Vec3, 'vec3, 'withVec3)
              , (Four, ''Vec4, 'vec4, 'withVec4)
              ]
          , family
              [t|Double|]
              [ (Two, ''DVec2, 'dvec2, 'withDVec2)
              , (Three, ''DVec3, 'dvec3, 'withDVec3)
              , (Four, ''DVec4, 'dvec4, 'withDVec4)
              ]
          , family
              [t|Int32|]
              [ (Two, ''IVec2, 'ivec2, 'withIVec2)
              , (Three, ''IVec3, 'ivec3, 'withIVec3)
              , (Four, ''IVec4, 'ivec4, 'withIVec4)
              ]
          , family
              [t|Word32|]
              [ (Two, ''UVec2, 'uvec2, 'withUVec2)
              , (Three, ''UVec3, 'uvec3, 'withUVec3)
              , (Four, ''UVec4, 'uvec4, 'withUVec4)
              ]
          , family
              [t|Int64|]
              [ (Two, ''I64Vec2, 'i64vec2, 'withI64Vec2)
              , (Three, ''I64Vec3, 'i64vec3, 'withI64Vec3)
              , (Four, ''I64Vec4, 'i64vec4, 'withI64Vec4)
              ]
          , family
              [t|Word64|]
              [ (Two, ''U64Vec2, 'u64vec2, 'withU64Vec2)
              , (Three, ''U64Vec3, 'u64vec3, 'withU64Vec3)
              , (Four, ''U64Vec4, 'u64vec4, 'withU64Vec4)
              ]
          ]
      vector (e, n, t, mk, el) = Composite (conT t) e n (varE mk) (varE el)
      packed =
        [ (''Vec3.Packed, ''Vec3)
        , (''DVec3.Packed, ''DVec3)
        , (''IVec3.Packed, ''IVec3)
        , (''UVec3.Packed, ''UVec3)
        , (''I64Vec3.Packed, ''I64Vec3)
        , (''U64Vec3.Packed, ''U64Vec3)
        ]
      fixed s a scalarA = \case
        Scalar -> Fixed s scalarA
        _ -> Fixed s a
   in [deriveVector l (vector v) | l <- allLayouts, v <- vectors]
        ++ [deriveFixed l [t|Mat4|] (fixed 64 16 4 l) | l <- allLayouts]
        ++ [deriveFixed l [t|DMat4|] (fixed 128 32 8 l) | l <- allLayouts]
        ++ [deriveFixed l [t|Quaternion|] (fixed 16 16 4 l) | l <- allLayouts]
        ++ [deriveFixed l [t|DQuaternion|] (fixed 32 32 8 l) | l <- allLayouts]
        ++ [deriveNewtype l [t|Transform|] [t|Mat4|] | l <- allLayouts]
        ++ [deriveNewtype l [t|DTransform|] [t|DMat4|] | l <- allLayouts]
        ++ [deriveNewtype l (conT p) (conT t) | l <- allLayouts, (p, t) <- packed]
        ++ [ deriveNewtype l [t|Point $(conT t)|] (conT t)
           | l <- allLayouts
           , (_, _, t, _, _) <- vectors
           ]
