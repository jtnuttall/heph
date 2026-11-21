module CoreConformanceSpec where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

import Foreign.GPU.Storable.Aligned.Test

test_prim :: [TestTree]
test_prim =
  [ testProperty "base float is storable" hprop_base_float_is_storable
  , testProperty "base int16 round trips" hprop_base_int16_round_trips
  , testCase "simple std140 packed size" unit_simple_std140_packed_size
  , testProperty "simple std140 round trip" hprop_simple_std140_round_trip
  , testCase "simple std430 packed size" unit_simple_std430_packed_size
  , testProperty "simple std430 round trip" hprop_simple_std430_round_trip
  , testCase "simple scalar packed size" unit_simple_scalar_packed_size
  , testProperty "simple scalar round trip" hprop_simple_scalar_round_trip
  , testCase "layoutbug std140 packed size" unit_layoutbug_std140_packed_size
  , testProperty "layoutbug std140 round trip" hprop_layoutbug_std140_round_trip
  , testCase "layoutbug std430 packed size" unit_layoutbug_std430_packed_size
  , testProperty "layoutbug std430 round trip" hprop_layoutbug_std430_round_trip
  , testCase "layoutbug scalar packed size" unit_layoutbug_scalar_packed_size
  , testProperty "layoutbug scalar round trip" hprop_layoutbug_scalar_round_trip
  , testCase "largeprimitives std140 packed size" unit_largeprimitives_std140_packed_size
  , testProperty "largeprimitives std140 round trip" hprop_largeprimitives_std140_round_trip
  , testCase "largeprimitives std430 packed size" unit_largeprimitives_std430_packed_size
  , testProperty "largeprimitives std430 round trip" hprop_largeprimitives_std430_round_trip
  , testProperty "largeprimitives scalar round trip" hprop_largeprimitives_scalar_round_trip
  , testProperty "mixedalign std140 round trip" hprop_mixedalign_std140_round_trip
  , testProperty "mixedalign std430 round trip" hprop_mixedalign_std430_round_trip
  , testProperty "mixedalign scalar round trip" hprop_mixedalign_scalar_round_trip
  , testProperty "booltest std140 round trip" hprop_booltest_std140_round_trip
  , testProperty "booltest std430 round trip" hprop_booltest_std430_round_trip
  , testProperty "booltest scalar round trip" hprop_booltest_scalar_round_trip
  ]

test_marshal :: [TestTree]
test_marshal =
  [ testProperty "dynamic vector copy byte identity" hprop_dynamic_vector_copy_byte_identity
  , testProperty "dynamic vector copy" hprop_dynamic_vector_copy
  ]
