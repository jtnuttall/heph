{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Description : Template Haskell generators for AlignedStorable instances
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Generators for 'AlignedStorable' instances of the vector, matrix and other
-- fixed-layout types of an arbitrary math library.
--
-- === __Usage example__
--
-- This is the current source code for [heph-aligned-storable-linear](https://hackage.haskell.org/package/heph-aligned-storable-linear)
-- with fully explicit impor
--
-- The template code deliberately allows for extremely terse integration modules.
--
-- @
-- -- If you still prefer explicitly listing your extensions, deriving needs
-- -- @DataKinds@, @FlexibleInstances@, and @MultiParamTypeClasses@.
-- -- GHC2024 turns all three on by default.
-- {-# LANGUAGE GHC2024 #-}
-- {-# LANGUAGE TemplateHaskell #-}
--
-- -- Deriving for a foreign library generates orphans. If you are integrating with a
-- -- library you control, you will not need this so long as you derive 'AlignedStorable'
-- -- in the same module as the type it's for.
-- {-# OPTIONS_GHC -Wno-orphans #-}
--
-- import Foreign.GPU.Storable.Aligned -- base instances
-- import Foreign.GPU.Storable.Aligned.TH
--    ( Composite(compositeEliminate, Composite, compositeType,
--                compositeElement, compositeDim, compositeConstruct),
--      Dim(Four, Two, Three),
--      dims,
--      deriveVector,
--      deriveMatrix,
--      conEliminate,
--      allLayouts,
--      deriveAll,
--      primitiveTypes,
--      floatingPrimitiveTypes )
-- import Language.Haskell.TH (appT, conE, conT)
-- import Linear (V2 (..), V3 (..), V4 (..))
--
-- deriveAll $
--  let con = \case Two -> 'V2; Three -> 'V3; Four -> 'V4
--      ty = \case Two -> ''V2; Three -> ''V3; Four -> ''V4
--      vec n e =
--        Composite
--          { compositeType = conT (ty n) `appT` e
--          , compositeElement = e
--          , compositeDim = n
--          , compositeConstruct = conE (con n)
--          , compositeEliminate = conEliminate (con n) n
--          }
--   in [deriveVector l (vec n e) | l <- allLayouts, e <- primitiveTypes, n <- dims]
--        ++ [ deriveMatrix l (vec r (conT (ty c) `appT` e))
--           | l <- allLayouts
--           , e <- floatingPrimitiveTypes
--           , r <- dims
--           , c <- dims
--           ]
-- @
module Foreign.GPU.Storable.Aligned.TH (
  -- * Specifications
  MemoryLayout (..),
  Dim (..),
  dimInt,
  dims,
  Composite (..),
  Fixed (..),

  -- * Generators
  deriveFixed,
  deriveVector,
  deriveMatrix,
  deriveNewtype,

  -- * Building blocks
  conEliminate,
  layoutType,
  allLayouts,
  deriveAll,

  -- * GLSL scalar tables
  primitives,
  primitiveTypes,
  floatingPrimitiveTypes,
) where

import Data.Int
import Data.Proxy
import Data.Word
import Language.Haskell.TH
import Numeric.Half (Half)

import Foreign.GPU.Storable.Aligned.Internal

-- |
-- The dimensionality of a vector or matrix.
data Dim = Two | Three | Four
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | The 'Dim' as a number.
dimInt :: Dim -> Int
dimInt = \case
  Two -> 2
  Three -> 3
  Four -> 4

-- | All dimensions, @[Two, Three, Four]@.
dims :: [Dim]
dims = [minBound .. maxBound]

-- | A value made of 'compositeDim' equally strided components of type
-- 'compositeElement'.
--
-- For a vector the components are scalars. For a matrix they are the vectors
-- stored contiguously in memory: the rows of linear's @V3 (V4 Float)@, or the
-- columns of a column-major library's @mat3x4@.
--
-- The generated code annotates 'compositeConstruct' and 'compositeEliminate'
-- with the types given below, so a mismatch with 'compositeType',
-- 'compositeElement' or 'compositeDim' is a type error at the splice.
data Composite = Composite
  { compositeType :: Q Type
  -- ^ The type to bind, e.g. @[t| V3 Float |]@.
  , compositeElement :: Q Type
  -- ^ The component type, e.g. @[t| Float |]@, or @[t| V4 Float |]@ for a
  -- matrix of @V4 Float@ rows.
  , compositeDim :: Dim
  -- ^ The number of components.
  , compositeConstruct :: Q Exp
  -- ^ Builds the value from its components in memory order:
  -- @e -> ... -> t@, e.g. @[| V3 |]@ or @[| vec3 |]@.
  , compositeEliminate :: Q Exp
  -- ^ Hands the components, in memory order, to a continuation:
  -- @t -> (e -> ... -> IO ()) -> IO ()@, e.g. @conEliminate 'V3 Three@ or
  -- @[| withVec3 |]@.
  }

-- | A constant size and alignment, in bytes.
data Fixed = Fixed
  { fixedSize :: Int
  , fixedAlignment :: Int
  }
  deriving (Eq, Show)

-- |
-- An instance that reads and writes the type through its
-- 'Foreign.Storable.Storable' instance, with the given size and alignment.
--
-- Correct when the 'Foreign.Storable.Storable' bytes match the GPU bytes exactly,
-- which is generally only the case for specific primitives and alignment-aware
-- newtype wrappers.
--
-- All generated methods are inlined.
--
-- === __Expected output__
--
-- @
-- instance AlignedStorable Std140 Int8 where
--   packedAlignedSizeOf _ _ = 4
--   alignedSizeOf _ _ = 4
--   alignedAlignment _ _ = 4
--   alignedPeek = defaultAlignedPeek
--   alignedPoke = defaultAlignedPoke
-- @
deriveFixed :: MemoryLayout -> Q Type -> Fixed -> Q [Dec]
deriveFixed layout t (Fixed s a) =
  let l = layoutType layout
   in [d|
        instance AlignedStorable $l $t where
          packedAlignedSizeOf _ _ = $(intE s)
          {-# INLINE packedAlignedSizeOf #-}
          alignedSizeOf _ _ = $(intE s)
          {-# INLINE alignedSizeOf #-}
          alignedAlignment _ _ = $(intE a)
          {-# INLINE alignedAlignment #-}
          alignedPeek = defaultAlignedPeek
          {-# INLINE alignedPeek #-}
          alignedPoke = defaultAlignedPoke
          {-# INLINE alignedPoke #-}
        |]

-- |
-- An instance for a GLSL vector of 'compositeDim' scalar components.
--
-- * Size: 'compositeDim' times the component's 'alignedSizeOf'.
-- * Alignment: under 'Std140' and 'Std430', the component's alignment times
--   2 for two components, 4 for three or four; under 'Scalar', the component's
--   alignment.
-- * Components are read and written one at a time, each the component's
--   alignment after the previous one, in every layout.
--
-- For
--
-- > deriveVector Std140 $
-- >   Composite [t| V3 Float |] [t| Float |] Three [| V3 |] (conEliminate 'V3 Three)
--
-- the generated instance is (with an @INLINE@ pragma on every method)
--
-- > instance AlignedStorable Std140 (V3 Float) where
-- >   packedAlignedSizeOf _ _ = 3 * alignedSizeOf (Proxy :: Proxy Std140) (Proxy :: Proxy Float)
-- >   alignedSizeOf _ _ = 3 * alignedSizeOf (Proxy :: Proxy Std140) (Proxy :: Proxy Float)
-- >   alignedAlignment _ _ = 4 * alignedAlignment (Proxy :: Proxy Std140) (Proxy :: Proxy Float)
-- >   alignedPeek =
-- >     peekStrided3
-- >       (V3 :: Float -> Float -> Float -> V3 Float)
-- >       (alignedAlignment (Proxy :: Proxy Std140) (Proxy :: Proxy Float))
-- >   alignedPoke =
-- >     pokeStrided3
-- >       ( (\(V3 x1 x2 x3) k -> k x1 x2 x3)
-- >           :: V3 Float -> (Float -> Float -> Float -> IO ()) -> IO ()
-- >       )
-- >       (alignedAlignment (Proxy :: Proxy Std140) (Proxy :: Proxy Float))
deriveVector :: MemoryLayout -> Composite -> Q [Dec]
deriveVector layout c =
  let l = layoutType layout
      e = compositeElement c
      dim = compositeDim c
      n = dimInt dim
      stdMultiplier = case dim of
        Two -> 2
        Three -> 4
        Four -> 4
      alignMultiplier = case layout of
        Std140 -> stdMultiplier
        Std430 -> stdMultiplier
        Scalar -> 1 :: Int
      size = [|$(intE n) * alignedSizeOf (Proxy :: Proxy $l) (Proxy :: Proxy $e)|]
      stride = [|alignedAlignment (Proxy :: Proxy $l) (Proxy :: Proxy $e)|]
      (mkT, elimT) = accessorTypes c
   in [d|
        instance AlignedStorable $l $(compositeType c) where
          packedAlignedSizeOf _ _ = $size
          {-# INLINE packedAlignedSizeOf #-}
          alignedSizeOf _ _ = $size
          {-# INLINE alignedSizeOf #-}
          alignedAlignment _ _ = $(intE alignMultiplier) * $stride
          {-# INLINE alignedAlignment #-}
          alignedPeek = $(peekStridedE dim) ($(compositeConstruct c) :: $mkT) $stride
          {-# INLINE alignedPeek #-}
          alignedPoke = $(pokeStridedE dim) ($(compositeEliminate c) :: $elimT) $stride
          {-# INLINE alignedPoke #-}
        |]

-- | An instance for a GLSL matrix stored as 'compositeDim' contiguous vectors
-- of type 'compositeElement'. The element must already have an instance (or
-- get one from the same splice).
--
-- * Stride between vectors: 'rowStride', the vector's packed size rounded up by
--   'layoutStride' (to 16 bytes under 'Std140').
-- * Size: 'compositeDim' times that stride; no further rounding.
-- * Alignment: the vector's alignment, rounded up by 'alignBlock'.
--
-- The same body is used in every layout.
--
-- For
--
-- > deriveMatrix Std430 $
-- >   Composite [t| V2 (V3 Double) |] [t| V3 Double |] Two [| V2 |] (conEliminate 'V2 Two)
--
-- the generated instance is (with an @INLINE@ pragma on every method)
--
-- > instance AlignedStorable Std430 (V2 (V3 Double)) where
-- >   packedAlignedSizeOf _ _ =
-- >     2 * rowStride (Proxy :: Proxy Std430) (Proxy :: Proxy (V3 Double))
-- >   alignedSizeOf = packedAlignedSizeOf
-- >   alignedAlignment l _ = alignBlock l (alignedAlignment l (Proxy :: Proxy (V3 Double)))
-- >   alignedPeek =
-- >     peekStrided2
-- >       (V2 :: V3 Double -> V3 Double -> V2 (V3 Double))
-- >       (rowStride (Proxy :: Proxy Std430) (Proxy :: Proxy (V3 Double)))
-- >   alignedPoke =
-- >     pokeStrided2
-- >       ( (\(V2 x1 x2) k -> k x1 x2)
-- >           :: V2 (V3 Double) -> (V3 Double -> V3 Double -> IO ()) -> IO ()
-- >       )
-- >       (rowStride (Proxy :: Proxy Std430) (Proxy :: Proxy (V3 Double)))
deriveMatrix :: MemoryLayout -> Composite -> Q [Dec]
deriveMatrix layout c =
  let l = layoutType layout
      r = compositeElement c
      dim = compositeDim c
      stride = [|rowStride (Proxy :: Proxy $l) (Proxy :: Proxy $r)|]
      (mkT, elimT) = accessorTypes c
   in [d|
        instance AlignedStorable $l $(compositeType c) where
          packedAlignedSizeOf _ _ = $(intE (dimInt dim)) * $stride
          {-# INLINE packedAlignedSizeOf #-}
          alignedSizeOf = packedAlignedSizeOf
          {-# INLINE alignedSizeOf #-}
          alignedAlignment l' _ = alignBlock l' (alignedAlignment l' (Proxy :: Proxy $r))
          {-# INLINE alignedAlignment #-}
          alignedPeek = $(peekStridedE dim) ($(compositeConstruct c) :: $mkT) $stride
          {-# INLINE alignedPeek #-}
          alignedPoke = $(pokeStridedE dim) ($(compositeEliminate c) :: $elimT) $stride
          {-# INLINE alignedPoke #-}
        |]

-- | An instance for a newtype, delegating to the instance of the type it wraps.
-- The arguments are the newtype and the wrapped type.
--
-- Values are converted with 'Data.Coerce.coerce', so the newtype's constructor
-- must be in scope at the splice. (Deriving via the wrapped type is not
-- possible because t'AlignedPtr' is nominal in its type argument.)
--
-- @deriveNewtype Std140 [t| Transform |] [t| Mat4 |]@ generates (with an
-- @INLINE@ pragma on every method)
--
-- > instance AlignedStorable Std140 Transform where
-- >   packedAlignedSizeOf _ _ = packedAlignedSizeOf (Proxy :: Proxy Std140) (Proxy :: Proxy Mat4)
-- >   alignedSizeOf _ _ = alignedSizeOf (Proxy :: Proxy Std140) (Proxy :: Proxy Mat4)
-- >   alignedAlignment _ _ = alignedAlignment (Proxy :: Proxy Std140) (Proxy :: Proxy Mat4)
-- >   alignedPeek = peekCoerce (Proxy :: Proxy Mat4)
-- >   alignedPoke = pokeCoerce (Proxy :: Proxy Mat4)
deriveNewtype :: MemoryLayout -> Q Type -> Q Type -> Q [Dec]
deriveNewtype layout t u =
  let l = layoutType layout
   in [d|
        instance AlignedStorable $l $t where
          packedAlignedSizeOf _ _ = packedAlignedSizeOf (Proxy :: Proxy $l) (Proxy :: Proxy $u)
          {-# INLINE packedAlignedSizeOf #-}
          alignedSizeOf _ _ = alignedSizeOf (Proxy :: Proxy $l) (Proxy :: Proxy $u)
          {-# INLINE alignedSizeOf #-}
          alignedAlignment _ _ = alignedAlignment (Proxy :: Proxy $l) (Proxy :: Proxy $u)
          {-# INLINE alignedAlignment #-}
          alignedPeek = peekCoerce (Proxy :: Proxy $u)
          {-# INLINE alignedPeek #-}
          alignedPoke = pokeCoerce (Proxy :: Proxy $u)
          {-# INLINE alignedPoke #-}
        |]

-- | An eliminator for a constructor with the given number of fields, for
-- 'compositeEliminate': @conEliminate 'V3 Three@ is
-- @\\(V3 x1 x2 x3) k -> k x1 x2 x3@.
conEliminate :: Name -> Dim -> Q Exp
conEliminate con dim = do
  xs <- traverse (\i -> newName ("x" ++ show i)) [1 .. dimInt dim]
  k <- newName "k"
  lamE [conP con (map varP xs), varP k] (foldl appE (varE k) (map varE xs))

-- | The promoted 'MemoryLayout', e.g. @Std140@.
layoutType :: MemoryLayout -> Q Type
layoutType = \case
  Std140 -> promotedT 'Std140
  Std430 -> promotedT 'Std430
  Scalar -> promotedT 'Scalar

-- | All layouts, @[Std140, Std430, Scalar]@.
allLayouts :: [MemoryLayout]
allLayouts = [minBound .. maxBound]

-- | Run a list of generators as one splice.
deriveAll :: [Q [Dec]] -> Q [Dec]
deriveAll = fmap concat . sequence

-- | The GLSL scalar types and their size and alignment in each layout, as used
-- for the instances in "Foreign.GPU.Storable.Aligned": @bool@ (as 'Bool'),
-- @float16_t@ ('Half'), @float@, @double@, and the 8-, 16-, 32- and 64-bit
-- signed and unsigned integers. Under 'Std140' the 8- and 16-bit integers take
-- 4 bytes.
primitives :: [(Q Type, MemoryLayout -> Fixed)]
primitives =
  [ ([t|Bool|], const (Fixed 4 4))
  , ([t|Half|], const (Fixed 2 2))
  , ([t|Float|], const (Fixed 4 4))
  , ([t|Double|], const (Fixed 8 8))
  , ([t|Int8|], small 1)
  , ([t|Int16|], small 2)
  , ([t|Int32|], const (Fixed 4 4))
  , ([t|Int64|], const (Fixed 8 8))
  , ([t|Word8|], small 1)
  , ([t|Word16|], small 2)
  , ([t|Word32|], const (Fixed 4 4))
  , ([t|Word64|], const (Fixed 8 8))
  ]
 where
  small n = \case
    Std140 -> Fixed 4 4
    Std430 -> Fixed n n
    Scalar -> Fixed n n

-- | The types in 'primitives'.
primitiveTypes :: [Q Type]
primitiveTypes = map fst primitives

-- | The floating-point types in 'primitives' ('Half', 'Float', 'Double'): the
-- element types of GLSL matrices.
floatingPrimitiveTypes :: [Q Type]
floatingPrimitiveTypes = [[t|Half|], [t|Float|], [t|Double|]]

intE :: Int -> Q Exp
intE = litE . integerL . fromIntegral

accessorTypes :: Composite -> (Q Type, Q Type)
accessorTypes c =
  let t = compositeType c
      components = replicate (dimInt (compositeDim c)) (compositeElement c)
      arrows r = foldr (\a b -> [t|$a -> $b|]) r components
   in (arrows t, [t|$t -> $(arrows [t|IO ()|]) -> IO ()|])

peekStridedE :: Dim -> Q Exp
peekStridedE = \case
  Two -> varE 'peekStrided2
  Three -> varE 'peekStrided3
  Four -> varE 'peekStrided4

pokeStridedE :: Dim -> Q Exp
pokeStridedE = \case
  Two -> varE 'pokeStrided2
  Three -> varE 'pokeStrided3
  Four -> varE 'pokeStrided4
