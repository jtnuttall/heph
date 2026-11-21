-- |
-- Description : Conformance tests for heph-aligned-storable
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
--
-- The harness, structs under test and their hand-written oracles shared by the
-- test suites of heph-aligned-storable and its instance packages.
module Foreign.GPU.Storable.Aligned.Test (
  module Foreign.GPU.Storable.Aligned.Test.Harness,
  module Foreign.GPU.Storable.Aligned.Test.Inspection,
  module Foreign.GPU.Storable.Aligned.Test.Marshal,
  module Foreign.GPU.Storable.Aligned.Test.Sut.Prim,
)
where

import Foreign.GPU.Storable.Aligned.Test.Harness
import Foreign.GPU.Storable.Aligned.Test.Inspection
import Foreign.GPU.Storable.Aligned.Test.Marshal
import Foreign.GPU.Storable.Aligned.Test.Sut.Prim
