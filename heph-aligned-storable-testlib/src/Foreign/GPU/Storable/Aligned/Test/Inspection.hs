{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Description : Inspection-test targets for AlignedStorable
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
module Foreign.GPU.Storable.Aligned.Test.Inspection (
  Stress (..),
)
where

import GHC.Generics (Generic)

import Foreign.GPU.Storable.Aligned

-- | Any type that pokes an array incurs allocations, even if it uses `copyBytes` internally.
-- I suspect llvm will unroll the loop if it is used, but I can't prove this by inspecting core.
data Stress a
  = Stress
      a
      a
      a
      a
      a
      a
      a
      a
      a
      a
      a
      a
      a
  deriving (Generic)

instance (MemoryLayoutRules l, AlignedStorable l a) => AlignedStorable l (Stress a)
