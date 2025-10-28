{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Heph.Input.Action.THSpec where

import Heph.Input
import Heph.Input.Action
import Heph.Input.Action.TH

import Data.Constraint.Extras.TH
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Dependent.Sum (DSum ((:=>)), (==>))
import Data.Functor.Identity
import Data.GADT.Compare.TH
import Data.GADT.Show (gshow)
import Data.GADT.Show.TH
import Data.Primitive (SmallArray)
import Data.Set (Set)
import Data.Set qualified as Set
import Hedgehog as HH
import Hedgehog.Gen qualified as Gen
import Test.Tasty.HUnit

-- Test action GADT
data TestAction (src :: ActionSource) where
  Jump :: TestAction Button
  Crouch :: TestAction Button
  Move :: TestAction Axis2D
  Look :: TestAction Axis2D
  Zoom :: TestAction Axis1D

makeAction ''TestAction

data TestActionTag f where
  JumpTag :: TestActionTag (InputSource Button)

deriveGEq ''TestActionTag
deriveGCompare ''TestActionTag
deriveGShow ''TestActionTag
deriveArgDict ''TestActionTag

-- x :: DMap TestActionTag Set
-- x =
--   foldl'
--     (\mp (k :=> v :: DSum TestActionTag []) -> DMap.insertWith' Set.union k (Set.fromList v) mp)
--     DMap.empty
--     [ JumpTag
--         :=> [ MouseButton MouseButtonLeft
--             , MouseButton MouseButtonRight
--             ]
--     ]

-- >>> show x
-- "fromList [JumpTag :=> [SourceButton (InputMouseButton MouseButtonLeft),SourceButton (InputMouseButton MouseButtonRight)]]"

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
