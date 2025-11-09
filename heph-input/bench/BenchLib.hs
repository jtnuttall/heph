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
  nfAppIO,
  whnf,
) where

import Criterion
import Criterion.Main
