{-# LANGUAGE AllowAmbiguousTypes #-}

-- |
-- Description : Round-trip harness for AlignedStorable conformance tests
-- Copyright   : (c) Jeremy Nuttall, 2025
-- License     : BSD-3-Clause
-- Maintainer  : jeremy@jeremy-nuttall.com
-- Stability   : experimental
module Foreign.GPU.Storable.Aligned.Test.Harness (
  -- * Oracles
  Oracle (..),
  peekOracleAt,
  pokeOracleAt,

  -- * Round trips
  Canary (..),
  genCanary,
  genCanaries,
  ptrTripping,
  ptrTrippingWith,
  TripOpts (..),
  defaultTripOpts,

  -- * Test trees
  LayoutCase (..),
  at,
  sutTests,
  sutTestsWith,
  layoutCaseTests,
  layoutName,

  -- * Generators
  genFloat,
  genInt,
  infinity,

  -- * Sized vectors
  peekSizedVector,
  pokeSizedVector,
  peekSized3DVector,
  pokeSized3DVector,
)
where

import Control.Exception.Lifted (bracket)
import Control.Monad (replicateM, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Coerce
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Proxy
import Data.Typeable (Typeable, showsTypeRep, typeRep)
import Data.Vector.Generic qualified as GV
import Data.Vector.Generic.Sized qualified as SGV
import Foreign
import Foreign.GPU.Storable.Aligned
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric (showHex)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testProperty)

--------------------------------------------------------------------------------
-- Oracles
--------------------------------------------------------------------------------

-- | A value whose 'Storable' instance is a hand-written oracle for layout @l@:
-- every offset and size is a literal read off glslang's output for the GLSL
-- twin of @a@ (see @glsl/Conformance.glsl@), and the alignment is a literal that
-- follows from the layout rules.
--
-- @Oracle l a@ and @t'Strided' l a@ are both newtypes over @a@, so 'ptrTripping'
-- can compare them directly.
newtype Oracle (l :: MemoryLayout) (a :: Type) = Oracle {unOracle :: a}
  deriving stock (Eq, Show)

-- | Peek a member through its oracle for layout @l@.
peekOracleAt :: forall l a b. (Storable (Oracle l a)) => Ptr b -> Int -> IO a
peekOracleAt ptr off = unOracle <$> peekByteOff @(Oracle l a) ptr off

-- | Poke a member through its oracle for layout @l@.
pokeOracleAt :: forall l a b. (Storable (Oracle l a)) => Ptr b -> Int -> a -> IO ()
pokeOracleAt ptr off = pokeByteOff ptr off . Oracle @l

--------------------------------------------------------------------------------
-- Test harness
--------------------------------------------------------------------------------

newtype Canary = Canary {unCanary :: Word64}
  deriving newtype (Eq, Num, Storable)

instance Show Canary where
  showsPrec _ (Canary n) = showHex n

genCanary :: Gen Canary
genCanary =
  Gen.element
    [ 0xDEADBEEFDEADBEEF
    , 0xCAFEBABECAFEBABE
    , 0xABADBABEABADBABE
    , 0xCDCDCDCDCDCDCDCD
    , 0x5555555555555555
    , 0xAAAAAAAAAAAAAAAA
    , 0x0000000000000000
    , 0xFFFFFFFFFFFFFFFF
    , 0x0123456789ABCDEF
    , 0xFEDCBA9876543210
    ]

genCanaries :: Int -> Gen [Canary]
genCanaries sz = replicateM sz genCanary

-- |
-- Given a SUT and an oracle, checks that they round trip with oneself and one another.
--
-- We validate four ways: SUT vs. SUT, Oracle vs. Oracle, SUT vs. Oracle, Oracle vs. SUT.
-- At the top of the test, the SUT size and alignment is verified to match the Oracle's.
--
-- This verifies:
-- 1. The SUT is coherent.
-- 2. The Oracle is coherent.
-- 3. The SUT's 'peek' is the inverse of the Oracle's 'poke'.
-- 4. The Oracle's 'peek' is the inverse of the SUT's 'poke'.
--
-- This makes it highly probable that we catch subtle unidirectional bugs.
--
-- For each combination (a, b):
-- 1. Allocate a buffer approximately 3*sizeOf(SUT) - rounded up to the next multiple of sizeof(Canary)
-- 2. Poison the buffer with a repeating bit pattern (0xCD)
-- 3. Fill the edges of the buffer with a canary value.
-- 4. Check that the canaries got written to the right place.
-- 5. Poke @a@ into the middle of the buffer
-- 6. Check the canaries -- if we overrun or undrrun the buffer, it's very likely to show here.
-- 7. Peek @b@ out of the middle of the buffer
-- 8. Out of an overabundance of caution, check the canaries again
-- 9. Check that the peeked value of @b@ is equal to the poked value of @a@
-- 10. Check that the peeked value of @b@ is equal to the poked value of @b@
--
-- This makes it incredibly unlikely (but not impossible) that this module corrupts memory in a way
-- that isn't caught in test.
--
-- Finally, unless 'checkByteImage' is off, the SUT and the Oracle are each poked into a zeroed
-- buffer of the SUT's size and the two byte images are compared. This catches writes into padding,
-- which neither the round trips nor the canaries can see.
ptrTripping
  :: forall s o m
   . ( HasCallStack
     , Coercible s o
     , MonadBaseControl IO m
     , MonadIO m
     , Typeable s
     , Typeable o
     , Eq s
     , Eq o
     , Show s
     , Show o
     , Storable s
     , Storable o
     )
  => s
  -> o
  -> PropertyT m ()
ptrTripping = ptrTrippingWith defaultTripOpts

-- | Options for 'ptrTrippingWith'.
newtype TripOpts = TripOpts
  { checkByteImage :: Bool
  -- ^ Compare the bytes the SUT and the Oracle poke into zeroed buffers. Turn
  -- this off for SUTs containing an t'AlignedArray': it copies the padding of
  -- its elements, which is uninitialized.
  }
  deriving stock (Show)

-- | All checks on.
defaultTripOpts :: TripOpts
defaultTripOpts = TripOpts{checkByteImage = True}

-- | 'ptrTripping' with options.
ptrTrippingWith
  :: forall s o m
   . ( HasCallStack
     , Coercible s o
     , MonadBaseControl IO m
     , MonadIO m
     , Typeable s
     , Typeable o
     , Eq s
     , Eq o
     , Show s
     , Show o
     , Storable s
     , Storable o
     )
  => TripOpts
  -> s
  -> o
  -> PropertyT m ()
ptrTrippingWith opts sut oracle = do
  let sz = sizeOf sut
      cSz = sizeOf @Canary undefined
      pSz = roundUpTo cSz sz
      tSz = sz + 2 * pSz
      lPadOffsets = [0, cSz .. pSz - cSz]
      rPadOffsets = [pSz + sz, pSz + sz + cSz .. tSz - cSz]
      poison = 0xCD
  sz === sizeOf oracle
  alignment sut === alignment oracle

  lCanaries <- zip lPadOffsets <$> forAll (genCanaries (length lPadOffsets))
  rCanaries <- zip rPadOffsets <$> forAll (genCanaries (length rPadOffsets))

  let populateCanaries ptr = liftIO do
        traverse_ (uncurry (pokeByteOff ptr)) lCanaries
        traverse_ (uncurry (pokeByteOff ptr)) rCanaries

      checkCanary ptr offset canary = do
        result <- liftIO $ peekByteOff ptr offset
        Canary result === canary

      checkCanaries ptr = do
        annotate $ "Left pad: 0-" <> show pSz
        traverse_ (uncurry (checkCanary ptr)) lCanaries
        annotate $ "Right pad: " <> show (pSz + sz) <> "-" <> show tSz
        traverse_ (uncurry (checkCanary ptr)) rCanaries

      checkRoundTrip
        :: forall a b
         . ( HasCallStack
           , Coercible a b
           , Typeable a
           , Typeable b
           , Eq b
           , Show b
           , Storable a
           , Storable b
           )
        => a
        -> b
        -> PropertyT m ()
      checkRoundTrip a b = bracket (liftIO $ mallocBytes @a tSz) (liftIO . free) \ptr -> do
        annotate
          $ showsTypeRep (typeRep (Proxy @a))
            . showString " x "
            . showsTypeRep (typeRep (Proxy @b))
          $ ""
        liftIO $ fillBytes ptr poison tSz
        populateCanaries ptr
        checkCanaries ptr

        liftIO $ pokeByteOff ptr pSz a
        checkCanaries ptr

        (result :: b) <- liftIO $ peekByteOff (castPtr @a @b ptr) pSz
        checkCanaries ptr

        result === coerce a
        result === b

  checkRoundTrip sut sut
  checkRoundTrip oracle oracle
  checkRoundTrip sut oracle
  checkRoundTrip oracle sut

  when (checkByteImage opts) do
    let byteImage :: forall a. (Storable a) => a -> IO [Word8]
        byteImage x = allocaBytesAligned sz (alignment sut) \ptr -> do
          fillBytes ptr 0 sz
          poke (castPtr ptr) x
          peekArray sz ptr
    sutImage <- liftIO $ byteImage sut
    oracleImage <- liftIO $ byteImage oracle
    annotate "Byte image: SUT, then Oracle"
    sutImage === oracleImage

--------------------------------------------------------------------------------
-- Test trees
--------------------------------------------------------------------------------

-- | One layout to test a struct under: the layout, plus the expected
-- 'packedAlignedSizeOf' when it is asserted.
data LayoutCase a where
  LayoutCase
    :: (KnownMemoryLayout l, Typeable l, AlignedStorable l a, Storable (Oracle l a))
    => Proxy l
    -> Maybe Int
    -> LayoutCase a

-- | @at \@Std140 (Just 40)@: test under 'Std140' and assert a packed size of 40.
at
  :: forall l a
   . (KnownMemoryLayout l, Typeable l, AlignedStorable l a, Storable (Oracle l a))
  => Maybe Int
  -> LayoutCase a
at = LayoutCase (Proxy @l)

-- | A group named after the struct with, per layout, a @"\<layout\> packed size"@
-- unit test (when the size is given) and a @"\<layout\> round trip"@ property
-- that runs 'ptrTripping' on @t'Strided' l a@ against @t'Oracle' l a@.
sutTests :: (Eq a, Show a, Typeable a) => String -> Gen a -> [LayoutCase a] -> TestTree
sutTests = sutTestsWith id defaultTripOpts

-- | 'sutTests' with a modifier for each property (e.g. @'withShrinks' 10@) and
-- t'TripOpts'.
sutTestsWith
  :: (Eq a, Show a, Typeable a)
  => (Property -> Property)
  -> TripOpts
  -> String
  -> Gen a
  -> [LayoutCase a]
  -> TestTree
sutTestsWith modify opts name gen = testGroup name . concatMap (layoutCaseTests modify opts gen)

-- | The tests 'sutTestsWith' generates for one layout, for structs whose type
-- depends on the layout (and so need one t'LayoutCase' per type).
layoutCaseTests
  :: forall a
   . (Eq a, Show a, Typeable a)
  => (Property -> Property)
  -> TripOpts
  -> Gen a
  -> LayoutCase a
  -> [TestTree]
layoutCaseTests modify opts gen (LayoutCase (pl :: Proxy l) expected) =
  [ testCase (lname <> " packed size") (packedAlignedSizeOf pl (Proxy @a) @?= n)
  | Just n <- [expected]
  ]
    <> [ testProperty (lname <> " round trip") . modify $ property do
           x <- forAll gen
           ptrTrippingWith opts (Strided @l x) (Oracle @l x)
       ]
 where
  lname = layoutName (memoryLayoutVal pl)

-- | The lower-case GLSL name of a layout, as used in test names.
layoutName :: MemoryLayout -> String
layoutName = \case
  Std140 -> "std140"
  Std430 -> "std430"
  Scalar -> "scalar"

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------
genFloat :: (RealFloat a) => Gen a
genFloat = Gen.realFloat (Range.linearFrac (-infinity) infinity)

genInt :: (Bounded a, Integral a) => Gen a
genInt = Gen.integral (Range.linear minBound maxBound)

infinity :: (Fractional a) => a
infinity = 1 / 0

peekSizedVector
  :: forall n a b c v
   . (KnownNat n, Storable a, Coercible a b, GV.Vector v b)
  => Proxy a
  -> Ptr c
  -> Int
  -> IO (SGV.Vector v n b)
peekSizedVector _ p baseOffset = SGV.generateM \i ->
  coerce <$> peekByteOff @a p (baseOffset + fromIntegral i * sizeOf @a undefined)

pokeSizedVector
  :: forall n a b v
   . (KnownNat n, Storable a, GV.Vector v a)
  => Ptr b
  -> Int
  -> SGV.Vector v n a
  -> IO ()
pokeSizedVector p baseOffset = SGV.imapM_ \i a -> pokeByteOff p (baseOffset + sizeOf @a undefined * fromIntegral i) a

peekSized3DVector
  :: forall n m o a b c v
   . ( KnownNat n
     , KnownNat m
     , KnownNat o
     , Storable a
     , Coercible a b
     , GV.Vector v b
     , GV.Vector v (SGV.Vector v o b)
     , GV.Vector v (SGV.Vector v m (SGV.Vector v o b))
     )
  => Proxy a
  -> Ptr c
  -> Int
  -> IO (SGV.Vector v n (SGV.Vector v m (SGV.Vector v o b)))
peekSized3DVector _ p baseOffset =
  let strideO = sizeOf @a undefined
      strideM = strideO * fromIntegral (natVal @o Proxy)
      strideN = strideM * fromIntegral (natVal @m Proxy)
   in SGV.generateM \i ->
        SGV.generateM \j ->
          SGV.generateM \k ->
            let offset = baseOffset + fromIntegral i * strideN + fromIntegral j * strideM + fromIntegral k * strideO
             in coerce <$> peekByteOff @a p offset

pokeSized3DVector
  :: forall n m o a b c v
   . ( KnownNat n
     , KnownNat m
     , KnownNat o
     , Storable a
     , Coercible a b
     , GV.Vector v b
     , GV.Vector v (SGV.Vector v o b)
     , GV.Vector v (SGV.Vector v m (SGV.Vector v o b))
     )
  => Proxy a
  -> Ptr c
  -> Int
  -> SGV.Vector v n (SGV.Vector v m (SGV.Vector v o b))
  -> IO ()
pokeSized3DVector _ p baseOffset =
  let strideO = sizeOf @a undefined
      strideM = strideO * fromIntegral (natVal @o Proxy)
      strideN = strideM * fromIntegral (natVal @m Proxy)
   in SGV.imapM_ \i ->
        SGV.imapM_ \j ->
          SGV.imapM_ \k a ->
            let offset = baseOffset + fromIntegral i * strideN + fromIntegral j * strideM + fromIntegral k * strideO
             in pokeByteOff @a p offset (coerce a)

-- Duplicating this from the main module so it doesn't need to be exported.
roundUpTo :: (Bits a, Num a) => a -> a -> a
roundUpTo multiple val = (val + multiple - 1) .&. complement (multiple - 1)
