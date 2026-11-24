{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Description : AlignedStorable instances for linear
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- 'Foreign.GPU.Storable.Aligned.AlignedStorable' instances for
-- [linear](https://hackage.haskell.org/package/linear). Import this module for
-- instances:
--
-- > import Foreign.GPU.Storable.Aligned
-- > import Foreign.GPU.Storable.Aligned.Linear ()
--
-- Two rules for matrices:
--
-- 1. Declare the GLSL block with @layout(row_major)@. linear's @Mrc@ is @r@
--    rows of row vectors, and the instances write those rows in order, which is
--    the memory order @row_major@ makes GLSL read. Matrices stay column-major
--    for arithmetic; nothing is transposed on upload.
-- 2. @Mrc@ (@r@ rows by @c@ columns) is GLSL's @matCxR@ (@c@ columns of
--    @r@-vectors): @M32 Float@ is @mat2x3@, @M24 Double@ is @dmat4x2@.
module Foreign.GPU.Storable.Aligned.Linear () where

import Foreign.GPU.Storable.Aligned ()
import Foreign.GPU.Storable.Aligned.TH
import Language.Haskell.TH (appT, conE, conT)
import Linear (V2 (..), V3 (..), V4 (..))

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
