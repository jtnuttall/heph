{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Generically derive Storable instances for GPU memory layouts
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Derive @Storable@ instances that respect GPU memory layout rules (@std140@,
-- @std430@, @scalar@). Works with any product type that has a t'GHC.Generics.Generic' instance.
--
-- @
-- data Uniforms = Uniforms
--   { viewProj  :: M44 Float
--   , cameraPos :: V3 Float
--   } deriving Generic
--
-- instance AlignedStorable Std140 Uniforms
-- @
--
-- Then use "Foreign.GPU.Marshal.Aligned" to get properly-padded pointers:
--
-- @
-- withPacked \@Std140 uniforms $ \\ptr -> uploadToGPU ptr
-- @
--
-- __NB:__ @vec3@ and @mat3@ types have historically had driver bugs
-- around alignment. Prefer @vec4@/@mat4@ when possible.
module Foreign.GPU.Storable.Aligned (
  -- * The AlignedStorable class
  AlignedStorable (..),

  -- * Memory layout
  MemoryLayout (..),
  SMemoryLayout (..),
  KnownMemoryLayout (..),
  memoryLayoutVal,

  -- * Storable Wrappers
  Strided (..),
  StridedVector,
  Packed (..),
  AlignedArray (..),
  mkAlignedArray,
  withAlignedArray,
  AlignedPtr (..),

  -- * Memory layout rules
  MemoryLayoutRules (..),

  -- * Generics
  GAlignedStorable (..),
)
where

import Language.Haskell.TH (appT, conE, conT)
import Linear (V2 (..), V3 (..), V4 (..))

import Foreign.GPU.Storable.Aligned.Internal
import Foreign.GPU.Storable.Aligned.TH

--------------------------------------------------------------------------------
-- Base instances for primitive shader types
--------------------------------------------------------------------------------

deriveAll [deriveFixed l t (f l) | (t, f) <- primitives, l <- allLayouts]

--------------------------------------------------------------------------------
-- Base instances for vectors and matrices
--------------------------------------------------------------------------------

deriveAll $
  let con = \case Two -> 'V2; Three -> 'V3; Four -> 'V4
      ty = \case Two -> ''V2; Three -> ''V3; Four -> ''V4
      vec n e =
        Composite
          { compositeType = conT (ty n) `appT` e
          , compositeElement = e
          , compositeDim = n
          , compositeConstruct = conE (con n)
          , compositeEliminate = conEliminate (con n) n
          }
   in [deriveVector l (vec n e) | l <- allLayouts, e <- primitiveTypes, n <- dims]
        ++ [ deriveMatrix l (vec r (conT (ty c) `appT` e))
           | l <- allLayouts
           , e <- floatingPrimitiveTypes
           , r <- dims
           , c <- dims
           ]
