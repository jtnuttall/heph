{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Template Haskell code generation for action types.
--
-- This module provides 'makeAction', which automatically generates lawful
-- 'Actionlike' instances for action GADTs.
module Heph.Input.Action.TH (makeAction) where

import Heph.Input.Action

import Control.Monad
import Data.Foldable
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax

-- | Generate an 'Actionlike' instance for an action GADT.
--
-- This Template Haskell function creates a lawful 'Actionlike' instance that
-- maps action constructors to unique integer IDs. It ensures:
--
-- * Dense, zero-based ID assignment (0 to n-1 for n constructors)
-- * Correct round-trip behavior ('fromActionId' . 'toActionId' = 'id')
-- * Efficient compile-time generation (no runtime overhead)
--
-- ==== __Usage__
--
-- Call 'makeAction' immediately after defining your action GADT:
--
-- @
-- {\-# LANGUAGE DataKinds #-\}
-- {\-# LANGUAGE TemplateHaskell #-\}
--
-- data MyAction (src :: 'ActionSource') where
--   Jump   :: MyAction 'Button'
--   Sprint :: MyAction 'Button'
--   Move   :: MyAction 'Axis2D'
--   Look   :: MyAction 'Axis2D'
--
-- 'makeAction' ''MyAction
-- @
--
-- ==== __Requirements__
--
-- The action type must satisfy:
--
-- * __GADT with at least one type variable__ for the 'ActionSource' phantom type
-- * __All constructors must be nullary__ (no fields allowed)
-- * __At least one constructor__ must be defined
--
-- Violating these requirements will result in a compile-time error with a
-- helpful message.
--
-- ==== __Safety__
--
-- Never write manual 'Actionlike' instances. The Template Haskell generator
-- ensures correctness guarantees that are difficult to verify manually and
-- critical for the safety of the 'ActionMap' implementation.
makeAction :: Name -> DecsQ
makeAction n = do
  dt <- reifyDatatype n

  validateType dt

  let ids = zip [0 ..] dt.datatypeCons
      maxId = length ids - 1
      toActionCases = map (uncurry mkToAction) ids
      fromActionCases = map (uncurry mkFromAction) ids <> [catchAll]
      catchAll = do
        bad <- newName "bad"
        match
          (varP bad)
          (normalB [|error $ "Library error: No action for action ID " <> show $(varE bad)|])
          []

  [d|
    instance Actionlike $(conT n) where
      toActionId a = $(caseE [|a|] toActionCases)
      {-# INLINE toActionId #-}
      fromActionId i = $(caseE [|i|] fromActionCases)
      {-# INLINE fromActionId #-}
      maxActionId = $(lift maxId)
      {-# INLINE maxActionId #-}
    |]
 where
  validVariants = [Datatype, DataInstance]

  validateType dt = do
    when (null dt.datatypeCons) do
      fail "Can't generate Actionlike for a datatype with no constructors"

    for_ dt.datatypeCons \ctor -> do
      unless (null ctor.constructorFields) do
        fail "Actionlike instances cannot have any fields"

    when (dt.datatypeVariant `notElem` validVariants) do
      fail $
        "Actionlike can only be generated for "
          <> show validVariants
          <> ", but got "
          <> show dt.datatypeVariant

    when (null dt.datatypeVars) do
      fail "Actionlike instances require GADTs with at least one type variable for the mapping"

  mkToAction i info =
    match
      [p|SomeAction $(conP info.constructorName [])|]
      (normalB (lift i))
      []

  mkFromAction i info =
    match
      (litP (IntegerL i))
      (normalB [|SomeAction $(conE info.constructorName)|])
      []
