{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Primitive.SmallArray
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.Datatype

data CtorInfo = CtorInfo
  { constructorName :: Name
  , actionMapFieldName :: Name
  , inputSourceType :: Q Type
  }

mkCtorInfo :: Name -> NonEmpty ConstructorInfo -> NonEmpty CtorInfo
mkCtorInfo vn =
  fmap
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
                  unlines
                    [ "Constructor " <> nameBase ctor.constructorName <> " has an unsupported GADT constraint."
                    , ""
                    , "Expected: "
                    , nameBase vn <> " ~ 'SomePromotedType)"
                    , ""
                    , "Got:"
                    , pprint bad
                    ]
         in CtorInfo
              { constructorName = ctor.constructorName
              , ..
              }
    )

makeAction :: Name -> DecsQ
makeAction n = do
  dt <- reifyDatatype n
  ctorInfo <- validateType dt

  let actionMapName = mkName (nameBase n <> "Map")

      declareActionMap =
        dataInstD
          (cxt [])
          ''ActionMap
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
                  (toList ctorInfo)
              )
          ]
          [derivClause Nothing [conT ''Generic]]

      declareCompileActions = do
        mappings <- newName "mappings"
        funD
          'compileActions
          [ clause
              [varP mappings]
              ( normalB
                  [e|
                    let
                      -- 'nub' is quadratic, but since we expect m to be small, this is acceptable
                      -- to maintain insertion order.
                      dmap :: DMap $(conT n) ActionSet
                      !dmap =
                        DMap.map (\(ActionSet set) -> ActionSet (L.nub set))
                          . foldr
                            ( \(act :=> set) ->
                                DMap.insertWith' (<>) act (ActionSet set)
                            )
                            DMap.empty
                          $ $(varE mappings)
                     in
                      $( recConE
                          actionMapName
                          ( map
                              ( \info -> do
                                  val <-
                                    [e|
                                      let sources =
                                            case DMap.lookup $(conE info.constructorName) dmap of
                                              Just (ActionSet r) -> toList r
                                              Nothing -> []
                                       in smallArrayFromList sources
                                      |]
                                  pure (info.actionMapFieldName, val)
                              )
                              (toList ctorInfo)
                          )
                       )
                    |]
              )
              []
          ]

      declareActionSources = do
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
                          (toList ctorInfo)
                      )
                  )
              )
              []
          ]

  actionlike <-
    instanceD
      (cxt [])
      (conT ''Actionlike `appT` conT n)
      [ declareActionMap
      , declareCompileActions
      , declareActionSources
      , pragInlD 'actionSources Inline FunLike AllPhases
      ]

  -- NFData via Generic
  nfDataActionMap <- instanceD (cxt []) (conT ''NFData `appT` (conT ''ActionMap `appT` conT n)) []

  gCompare <- deriveGCompare n
  gEq <- deriveGEq n
  gShow <- deriveGShow n
  argDict <- deriveArgDict n

  pure $ concat [[actionlike, nfDataActionMap], gCompare, gEq, gShow, argDict]
 where
  validVariants = [Datatype, DataInstance]

  validateType dt = do
    for_ dt.datatypeCons \ctor -> do
      unless (null ctor.constructorFields) do
        fail "Actionlike instances cannot have any fields"

    unless (dt.datatypeVariant `elem` validVariants) do
      fail $
        "Actionlike can only be generated for "
          <> show validVariants
          <> ", but got "
          <> show dt.datatypeVariant

    tyVar <- case dt.datatypeVars of
      [tv] -> pure $ tvName tv
      bad ->
        fail $
          "Actionlike GADT must have exactly one type variable, but "
            <> nameBase n
            <> " has "
            <> show (length bad)

    case NE.nonEmpty dt.datatypeCons of
      Just ne -> pure $ mkCtorInfo tyVar ne
      Nothing -> fail "Can't generate Actionlike for a datatype with no constructors"

-- TODO: reintroduce validations and scrubbing

-- -  -- Make sure that input sources are reasonable and will not cause div by zero errors.
-- -  cleanSource = \case
-- -    SourceStick1D a sens (Deadzone dz) -> SourceStick1D a sens (Deadzone (clamp (-0.99, 0.99) dz))
-- -    SourceStick x y sens (Deadzone dz) -> SourceStick x y sens (Deadzone (clamp (-0.99, 0.99) dz))
