{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Heph.Input.Action.THSpec where

import Heph.Input

-- Test action GADT
data TestAction (src :: ActionSource) where
  Jump :: TestAction Button
  Crouch :: TestAction Button
  Move :: TestAction Axis2D
  Look :: TestAction Axis2D
  Zoom :: TestAction Axis1D

makeAction ''TestAction

-- TODO: Reintroduce tests here
