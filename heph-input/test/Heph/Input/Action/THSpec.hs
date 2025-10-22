{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Heph.Input.Action.THSpec where

import Heph.Input.Action
import Heph.Input.Action.TH

import Data.Typeable
import Hedgehog as HH
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Heph.Input (SomeAction)
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

-- Test action GADT
data TestAction (src :: ActionSource) where
  Jump :: TestAction Button
  Crouch :: TestAction Button
  Move :: TestAction Axis2D
  Look :: TestAction Axis2D
  Zoom :: TestAction Axis1D

makeAction ''TestAction

-- Unit tests
unit_toActionIdUnique :: Assertion
unit_toActionIdUnique = do
  toActionId (SomeAction Jump) @?= 0
  toActionId (SomeAction Crouch) @?= 1
  toActionId (SomeAction Move) @?= 2
  toActionId (SomeAction Look) @?= 3
  toActionId (SomeAction Zoom) @?= 4

unit_maxActionIdCorrect :: Assertion
unit_maxActionIdCorrect = maxActionId @TestAction @?= 4

-- Property tests
hprop_actionIdRoundtrip :: Property
hprop_actionIdRoundtrip = property $ do
  action <- forAll genTestAction
  let actionId = toActionId action
      roundtripped = fromActionId @TestAction actionId
  toActionId roundtripped === actionId

hprop_allActionIdsInRange :: Property
hprop_allActionIdsInRange = property $ do
  action <- forAll genTestAction
  let actionId = toActionId action
  HH.assert $ actionId >= 0
  HH.assert $ actionId <= maxActionId @TestAction

-- -- Generators
genTestAction :: Gen (SomeAction TestAction)
genTestAction =
  Gen.element
    [ SomeAction Jump
    , SomeAction Crouch
    , SomeAction Move
    , SomeAction Look
    , SomeAction Zoom
    ]
