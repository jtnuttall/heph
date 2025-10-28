{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Template Haskell code generation for action types.
--
-- This module provides 'makeAction', which automatically generates lawful
-- 'Actionlike' instances for action GADTs.
module Heph.Input.Action.TH (makeAction) where

import Heph.Input.Action

import Control.DeepSeq
import Control.Monad
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Map (DMap)
import Data.Dependent.Map qualified as DMap
import Data.Foldable
import Data.GADT.Compare.TH (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Data.Primitive.SmallArray
import Data.Set (Set)
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Datatype
import Language.Haskell.TH.Syntax

data CtorInfo = CtorInfo
  { constructorName :: Name
  , actionMapFieldName :: Name
  , inputSourceType :: Q Type
  }

mkCtorInfo :: DatatypeInfo -> Name -> [CtorInfo]
mkCtorInfo dt vn =
  map
    ( \ctor ->
        let actionMapFieldName = mkName ("actionMap_" <> nameBase ctor.constructorName)
            inputSourceType = case ctor.constructorContext of
              [EqualityT `AppT` VarT v `AppT` PromotedT r]
                | v == vn ->
                    conT ''InputSource `appT` promotedT r
              [EqualityT `AppT` PromotedT r `AppT` VarT v]
                | v == vn ->
                    conT ''InputSource `appT` promotedT r
              bad ->
                fail $
                  "Constructor "
                    <> nameBase ctor.constructorName
                    <> " has an unsupported GADT constraint. Expected: ["
                    <> nameBase vn
                    <> " ~ 'SomePromotedType], Got: "
                    <> show bad
         in CtorInfo
              { constructorName = ctor.constructorName
              , ..
              }
    )
    dt.datatypeCons

newtype ActionF f (src :: ActionSource) = ActionF (f (InputSource src))

instance Semigroup (ActionF Set a) where
  ActionF l <> ActionF r = ActionF $ l <> r

instance Monoid (ActionF Set a) where
  mempty = ActionF mempty

makeAction :: Name -> DecsQ
makeAction n = do
  dt <- reifyDatatype n
  tyVarName <- validateType dt

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

      ctorInfo = mkCtorInfo dt tyVarName
      actionMapName = mkName (nameBase n <> "Map")

  actionlike <-
    instanceD
      (cxt [])
      (conT ''Actionlike `appT` conT n)
      [ dataInstD
          (cxt [])
          ''ActionMap2
          [conT n]
          Nothing
          [ recC
              actionMapName
              ( map
                  ( \info ->
                      varBangType
                        info.actionMapFieldName
                        ( bangType
                            (bang sourceUnpack sourceStrict)
                            (conT ''SmallArray `appT` info.inputSourceType)
                        )
                  )
                  ctorInfo
              )
          ]
          [derivClause Nothing [conT ''Generic]]
      , do
          mappings <- newName "mappings"
          funD
            'compileActions
            [ clause
                [varP mappings]
                ( normalB
                    [e|
                      let
                        dmap :: DMap $(conT n) (ActionF Set)
                        dmap =
                          foldr
                            ( \(ActionMapping act set) ->
                                DMap.insertWith' (<>) act (ActionF set)
                            )
                            DMap.empty
                            $(varE mappings)
                       in
                        $( recConE
                            actionMapName
                            ( map
                                ( \info -> do
                                    val <-
                                      [e|
                                        let sources =
                                              case DMap.lookup $(conE info.constructorName) dmap of
                                                Just (ActionF r) -> toList r
                                                Nothing -> []
                                         in case smallArrayFromList sources of
                                              arr -> arr -- force whnf
                                        |]
                                    pure (info.actionMapFieldName, val)
                                )
                                ctorInfo
                            )
                         )
                      |]
                )
                []
            ]
      , do
          actionMap <- newName "actionMap"
          action <- newName "action"

          funD
            'actionSources
            [ clause
                [varP actionMap, varP action]
                ( normalB
                    ( caseE
                        (varE action)
                        ( map
                            ( \info ->
                                match
                                  (conP info.constructorName [])
                                  (normalB (varE info.actionMapFieldName `appE` varE actionMap))
                                  []
                            )
                            ctorInfo
                        )
                    )
                )
                []
            ]
      , pragInlD 'actionSources Inline FunLike AllPhases
      , valD (varP 'toActionId) (normalB (lamCaseE toActionCases)) []
      , pragInlD 'toActionId Inline FunLike AllPhases
      , valD (varP 'fromActionId) (normalB (lamCaseE fromActionCases)) []
      , pragInlD 'fromActionId Inline FunLike AllPhases
      , valD (varP 'maxActionId) (normalB (lift maxId)) []
      , pragInlD 'maxActionId Inline FunLike AllPhases
      ]

  nfDataActionMap <- instanceD (cxt []) (conT ''NFData `appT` (conT ''ActionMap2 `appT` conT n)) []

  gCompare <- deriveGCompare n
  gEq <- deriveGEq n
  gShow <- deriveGShow n
  argDict <- deriveArgDict n

  pure $ concat [[actionlike, nfDataActionMap], gCompare, gEq, gShow, argDict]
 where
  validVariants = [Datatype, DataInstance]

  validateType dt = do
    when (null dt.datatypeCons) do
      fail "Can't generate Actionlike for a datatype with no constructors"

    for_ dt.datatypeCons \ctor -> do
      unless (null ctor.constructorFields) do
        fail "Actionlike instances cannot have any fields"

    unless (dt.datatypeVariant `elem` validVariants) do
      fail $
        "Actionlike can only be generated for "
          <> show validVariants
          <> ", but got "
          <> show dt.datatypeVariant

    case dt.datatypeVars of
      [tv] -> pure $ tvName tv
      bad ->
        fail $
          "Actionlike GADT must have exactly one type variable, but "
            <> nameBase n
            <> " has "
            <> show (length bad)

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
