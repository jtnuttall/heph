{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- The vector and matrix types in the example below are linear's. Instances for
-- them live in @heph-aligned-storable-linear@, and
-- @heph-aligned-storable-geomancy@ provides them for geomancy;
-- "Foreign.GPU.Storable.Aligned.TH" binds other libraries.
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

import Foreign.GPU.Storable.Aligned.Internal
import Foreign.GPU.Storable.Aligned.TH

--------------------------------------------------------------------------------
-- Base instances for primitive shader types
--------------------------------------------------------------------------------

deriveAll [deriveFixed l t (f l) | (t, f) <- primitives, l <- allLayouts]
