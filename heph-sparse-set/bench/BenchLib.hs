{-# OPTIONS_GHC -Wno-orphans #-}

-- | Allows plug-and-play for tasty-bench and criterion
module BenchLib (
  Benchmark,
  Benchmarkable,
  bgroup,
  defaultMain,
  env,
  perBatchEnv,
  perRunEnv,
  bench,
  nf,
  nfIO,
  whnf,
) where

import Control.DeepSeq
import Criterion
import Criterion.Main

import Data.Primitive.MutVar
import Data.SparseSet.Generic.Mutable qualified as G
import Data.SparseSet.Unboxed.Mutable qualified as U

-- |
-- __NOTE__: The definition is the same as that for MVar and IORef in `deepseq`, so the same
-- caveat applies: Only strict in reference, not in value.
instance NFData (MutVar s a) where
  rnf = rwhnf

instance (NFData (v s a)) => NFData (G.MutableSparseSet v s a)
instance NFData (U.MutableSparseSet s a)
