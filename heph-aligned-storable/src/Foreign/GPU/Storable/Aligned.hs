{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : Generically derive Storable instances for GPU memory layouts
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- Derive @Storable@ instances that adhere to GPU memory layout rules (@std140@,
-- @std430@, @scalar@).
--
-- This mechanism should work with any record that:
--
-- 1. Has a t'GHC.Generics.Generic' instance.
-- 2. Contains only types that have their own 'AlignedStorable' instances.
--
-- == Example
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
