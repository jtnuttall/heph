{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant irrefutable pattern" #-}

module Main (main) where

import BenchLib
import Heph.Input
import Heph.Input.Buffer as Buf
import Heph.Input.Internal.BoundedArray.Primitive.Mutable qualified as MPA
import Linear.V2

-- Test action GADTs for different scales

-- Small game: 10 actions (typical indie game)
data SmallGame (src :: ActionSource) where
  SGJump :: SmallGame Button
  SGCrouch :: SmallGame Button
  SGSprint :: SmallGame Button
  SGInteract :: SmallGame Button
  SGShoot :: SmallGame Button
  SGReload :: SmallGame Button
  SGMove :: SmallGame Axis2D
  SGLook :: SmallGame Axis2D
  SGZoom :: SmallGame Axis1D
  SGThrottle :: SmallGame Axis1D

makeAction ''SmallGame

-- Medium game: 20 actions
data MediumGame (src :: ActionSource) where
  MGJump :: MediumGame Button
  MGCrouch :: MediumGame Button
  MGSprint :: MediumGame Button
  MGInteract :: MediumGame Button
  MGShoot :: MediumGame Button
  MGReload :: MediumGame Button
  MGMelee :: MediumGame Button
  MGGrenade :: MediumGame Button
  MGSwitchWeapon :: MediumGame Button
  MGUse :: MediumGame Button
  MGMap :: MediumGame Button
  MGInventory :: MediumGame Button
  MGPause :: MediumGame Button
  MGScreenshot :: MediumGame Button
  MGMove :: MediumGame Axis2D
  MGLook :: MediumGame Axis2D
  MGStrafe :: MediumGame Axis2D
  MGZoom :: MediumGame Axis1D
  MGThrottle :: MediumGame Axis1D
  MGLeanAxis :: MediumGame Axis1D

makeAction ''MediumGame

-- Large game: 100 actions (complex game with many systems)
data LargeGame (src :: ActionSource) where
  -- Movement (10)
  LGJump :: LargeGame Button
  LGCrouch :: LargeGame Button
  LGSprint :: LargeGame Button
  LGWalk :: LargeGame Button
  LGProne :: LargeGame Button
  LGClimb :: LargeGame Button
  LGSlide :: LargeGame Button
  LGRoll :: LargeGame Button
  LGMove :: LargeGame Axis2D
  LGMoveVertical :: LargeGame Axis1D
  -- Combat (20)
  LGShoot :: LargeGame Button
  LGADS :: LargeGame Button
  LGReload :: LargeGame Button
  LGMelee :: LargeGame Button
  LGGrenade :: LargeGame Button
  LGThrowGrenade :: LargeGame Button
  LGNextWeapon :: LargeGame Button
  LGPrevWeapon :: LargeGame Button
  LGWeapon1 :: LargeGame Button
  LGWeapon2 :: LargeGame Button
  LGWeapon3 :: LargeGame Button
  LGWeapon4 :: LargeGame Button
  LGWeapon5 :: LargeGame Button
  LGToggleFireMode :: LargeGame Button
  LGToggleSafety :: LargeGame Button
  LGLook :: LargeGame Axis2D
  LGLookHorizontal :: LargeGame Axis1D
  LGLookVertical :: LargeGame Axis1D
  LGZoom :: LargeGame Axis1D
  LGCycleZoom :: LargeGame Button
  -- Interaction (15)
  LGUse :: LargeGame Button
  LGInteract :: LargeGame Button
  LGPickUp :: LargeGame Button
  LGDrop :: LargeGame Button
  LGExamine :: LargeGame Button
  LGOpen :: LargeGame Button
  LGClose :: LargeGame Button
  LGLock :: LargeGame Button
  LGUnlock :: LargeGame Button
  LGTalk :: LargeGame Button
  LGTrade :: LargeGame Button
  LGGive :: LargeGame Button
  LGTake :: LargeGame Button
  LGCraft :: LargeGame Button
  LGRepair :: LargeGame Button
  -- Inventory & Menus (20)
  LGInventory :: LargeGame Button
  LGMap :: LargeGame Button
  LGJournal :: LargeGame Button
  LGQuests :: LargeGame Button
  LGSkills :: LargeGame Button
  LGEquipment :: LargeGame Button
  LGStats :: LargeGame Button
  LGSettings :: LargeGame Button
  LGPause :: LargeGame Button
  LGQuickSave :: LargeGame Button
  LGQuickLoad :: LargeGame Button
  LGScreenshot :: LargeGame Button
  LGMenuUp :: LargeGame Button
  LGMenuDown :: LargeGame Button
  LGMenuLeft :: LargeGame Button
  LGMenuRight :: LargeGame Button
  LGMenuSelect :: LargeGame Button
  LGMenuBack :: LargeGame Button
  LGMenuNavigate :: LargeGame Axis2D
  LGTabLeft :: LargeGame Button
  -- Vehicle (15)
  LGVehicleEnter :: LargeGame Button
  LGVehicleExit :: LargeGame Button
  LGVehicleAccel :: LargeGame Axis1D
  LGVehicleBrake :: LargeGame Axis1D
  LGVehicleSteer :: LargeGame Axis1D
  LGVehicleSteerXY :: LargeGame Axis2D
  LGVehicleHandbrake :: LargeGame Button
  LGVehicleBoost :: LargeGame Button
  LGVehicleHorn :: LargeGame Button
  LGVehicleLights :: LargeGame Button
  LGVehicleCamera :: LargeGame Button
  LGVehicleRadio :: LargeGame Button
  LGVehicleShoot :: LargeGame Button
  LGVehicleSpecial :: LargeGame Button
  LGVehicleEject :: LargeGame Button
  -- Social & Communication (10)
  LGVoiceChat :: LargeGame Button
  LGTextChat :: LargeGame Button
  LGEmote1 :: LargeGame Button
  LGEmote2 :: LargeGame Button
  LGEmote3 :: LargeGame Button
  LGEmote4 :: LargeGame Button
  LGPing :: LargeGame Button
  LGMark :: LargeGame Button
  LGTeamMenu :: LargeGame Button
  LGScoreboard :: LargeGame Button
  -- Camera & View (10)
  LGToggleCamera :: LargeGame Button
  LGCameraZoom :: LargeGame Axis1D
  LGCameraRotate :: LargeGame Axis2D
  LGCameraPan :: LargeGame Axis2D
  LGCameraUp :: LargeGame Button
  LGCameraDown :: LargeGame Button
  LGCameraLeft :: LargeGame Button
  LGCameraRight :: LargeGame Button
  LGFreeLook :: LargeGame Button
  LGResetCamera :: LargeGame Button

makeAction ''LargeGame

-- Main benchmark entry point
main :: IO ()
main =
  defaultMain
    [ actionMapBenchmarks
    , inputQueryBenchmarks
    , frameManagementBenchmarks
    , baselineBenchmarks
    ]

-- ============================================================================
-- ActionMap Creation Benchmarks
-- ============================================================================

actionMapBenchmarks :: Benchmark
actionMapBenchmarks =
  bgroup
    "ActionMap Creation"
    [ bgroup
        "Small Game (10 actions)"
        [ bench "Single binding per action" $
            nf newActionMap smallGameSingleBindings
        , bench "Multiple bindings (2 per action)" $
            nf newActionMap smallGameMultipleBindings
        , bench "Many bindings (5 per action)" $
            nf newActionMap smallGameManyBindings
        ]
    , bgroup
        "Medium Game (20 actions)"
        [ bench "Single binding per action" $
            nf newActionMap mediumGameSingleBindings
        , bench "Multiple bindings (2 per action)" $
            nf newActionMap mediumGameMultipleBindings
        ]
    , bgroup
        "Large Game (100 actions)"
        [ bench "Single binding per action" $
            nf newActionMap largeGameSingleBindings
        , bench "Multiple bindings (2 per action)" $
            nf newActionMap largeGameMultipleBindings
        ]
    ]
 where
  smallGameSingleBindings =
    [ SGJump ~> [Key ScancodeSpace]
    , SGCrouch ~> [Key ScancodeLCtrl]
    , SGSprint ~> [Key ScancodeLShift]
    , SGInteract ~> [Key ScancodeE]
    , SGShoot ~> [MouseButton MouseButtonLeft]
    , SGReload ~> [Key ScancodeR]
    , SGMove ~> [LeftStick 1.0 0.15]
    , SGLook ~> [MouseMotion 1.0]
    , SGZoom ~> [AsAxis (Key ScancodeEquals)]
    , SGThrottle ~> [AsAxis (Key ScancodeW)]
    ]

  smallGameMultipleBindings =
    [ SGJump ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]
    , SGCrouch ~> [Key ScancodeLCtrl, GamepadButton ControllerButtonB]
    , SGSprint ~> [Key ScancodeLShift, GamepadButton ControllerButtonLeftStick]
    , SGInteract ~> [Key ScancodeE, GamepadButton ControllerButtonX]
    , SGShoot ~> [MouseButton MouseButtonLeft, GamepadButton ControllerButtonRightShoulder]
    , SGReload ~> [Key ScancodeR, GamepadButton ControllerButtonY]
    , SGMove
        ~> [DPad (Key ScancodeA) (Key ScancodeW) (Key ScancodeS) (Key ScancodeD), LeftStick 1.0 0.15]
    , SGLook ~> [MouseMotion 1.0, RightStick 2.0 0.2]
    , SGZoom ~> [AsAxis (Key ScancodeEquals), AsAxis (GamepadButton ControllerButtonDpadUp)]
    , SGThrottle
        ~> [AsAxis (Key ScancodeW), GamepadTrigger ControllerAxisTriggerLeft 1.0 0.1]
    ]

  smallGameManyBindings =
    [ SGJump
        ~> [ Key ScancodeSpace
           , GamepadButton ControllerButtonA
           , GamepadButton ControllerButtonB
           , Key ScancodeUp
           , GamepadButton ControllerButtonDpadUp
           ]
    , SGCrouch
        ~> [ Key ScancodeLCtrl
           , GamepadButton ControllerButtonB
           , Key ScancodeC
           , GamepadButton ControllerButtonRightStick
           , Key ScancodeDown
           ]
    , SGSprint
        ~> [ Key ScancodeLShift
           , GamepadButton ControllerButtonLeftStick
           , Key ScancodeRShift
           , GamepadButton ControllerButtonX
           , GamepadButton ControllerButtonY
           ]
    , SGInteract
        ~> [ Key ScancodeE
           , GamepadButton ControllerButtonX
           , Key ScancodeF
           , MouseButton MouseButtonMiddle
           , GamepadButton ControllerButtonA
           ]
    , SGShoot
        ~> [ MouseButton MouseButtonLeft
           , GamepadButton ControllerButtonRightShoulder
           , Key ScancodeLCtrl
           , GamepadButton ControllerButtonA
           , MouseButton MouseButtonRight
           ]
    , SGReload
        ~> [ Key ScancodeR
           , GamepadButton ControllerButtonY
           , Key ScancodeT
           , GamepadButton ControllerButtonLeftShoulder
           , MouseButton MouseButtonMiddle
           ]
    , SGMove
        ~> [ DPad (Key ScancodeA) (Key ScancodeW) (Key ScancodeS) (Key ScancodeD)
           , LeftStick 1.0 0.15
           , DPad (Key ScancodeLeft) (Key ScancodeUp) (Key ScancodeDown) (Key ScancodeRight)
           , DPad
              (GamepadButton ControllerButtonDpadLeft)
              (GamepadButton ControllerButtonDpadUp)
              (GamepadButton ControllerButtonDpadDown)
              (GamepadButton ControllerButtonDpadRight)
           , DPad (Key Scancode4) (Key Scancode8) (Key Scancode5) (Key Scancode6)
           ]
    , SGLook
        ~> [ MouseMotion 1.0
           , RightStick 2.0 0.2
           , GamepadStick ControllerAxisLeftX ControllerAxisLeftY 1.5 0.15
           , DPad (Key ScancodeJ) (Key ScancodeI) (Key ScancodeK) (Key ScancodeL)
           , MouseAxis2D MouseX MouseY 0.8
           ]
    , SGZoom
        ~> [ AsAxis (Key ScancodeEquals)
           , AsAxis (GamepadButton ControllerButtonDpadUp)
           , AsAxis (Key ScancodeEquals)
           , GamepadTrigger ControllerAxisTriggerRight 1.0 0.1
           , AsAxis (Key ScancodeMinus)
           ]
    , SGThrottle
        ~> [ AsAxis (Key ScancodeW)
           , GamepadTrigger ControllerAxisTriggerLeft 1.0 0.1
           , AsAxis (MouseButton MouseButtonRight)
           , GamepadTrigger ControllerAxisLeftY 1.0 0.15
           , AsAxis (Key ScancodeSpace)
           ]
    ]

  mediumGameSingleBindings =
    [ MGJump ~> [Key ScancodeSpace]
    , MGCrouch ~> [Key ScancodeLCtrl]
    , MGSprint ~> [Key ScancodeLShift]
    , MGInteract ~> [Key ScancodeE]
    , MGShoot ~> [MouseButton MouseButtonLeft]
    , MGReload ~> [Key ScancodeR]
    , MGMelee ~> [Key ScancodeF]
    , MGGrenade ~> [Key ScancodeG]
    , MGSwitchWeapon ~> [Key ScancodeQ]
    , MGUse ~> [Key ScancodeE]
    , MGMap ~> [Key ScancodeM]
    , MGInventory ~> [Key ScancodeI]
    , MGPause ~> [Key ScancodeEscape]
    , MGScreenshot ~> [Key ScancodeF12]
    , MGMove ~> [LeftStick 1.0 0.15]
    , MGLook ~> [MouseMotion 1.0]
    , MGStrafe ~> [RightStick 1.0 0.15]
    , MGZoom ~> [AsAxis (Key ScancodeEquals)]
    , MGThrottle ~> [GamepadTrigger ControllerAxisTriggerRight 1.0 0.1]
    , MGLeanAxis ~> [GamepadTrigger ControllerAxisRightX 1.0 0.2]
    ]

  mediumGameMultipleBindings =
    [ MGJump ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]
    , MGCrouch ~> [Key ScancodeLCtrl, GamepadButton ControllerButtonB]
    , MGSprint ~> [Key ScancodeLShift, GamepadButton ControllerButtonLeftStick]
    , MGInteract ~> [Key ScancodeE, GamepadButton ControllerButtonX]
    , MGShoot ~> [MouseButton MouseButtonLeft, GamepadButton ControllerButtonRightShoulder]
    , MGReload ~> [Key ScancodeR, GamepadButton ControllerButtonY]
    , MGMelee ~> [Key ScancodeF, GamepadButton ControllerButtonRightStick]
    , MGGrenade ~> [Key ScancodeG, GamepadButton ControllerButtonLeftShoulder]
    , MGSwitchWeapon ~> [Key ScancodeQ, GamepadButton ControllerButtonDpadDown]
    , MGUse ~> [Key ScancodeE, GamepadButton ControllerButtonX]
    , MGMap ~> [Key ScancodeM, GamepadButton ControllerButtonBack]
    , MGInventory ~> [Key ScancodeI, GamepadButton ControllerButtonStart]
    , MGPause ~> [Key ScancodeEscape, GamepadButton ControllerButtonStart]
    , MGScreenshot ~> [Key ScancodeF12, GamepadButton ControllerButtonGuide]
    , MGMove
        ~> [DPad (Key ScancodeA) (Key ScancodeW) (Key ScancodeS) (Key ScancodeD), LeftStick 1.0 0.15]
    , MGLook ~> [MouseMotion 1.0, RightStick 2.0 0.2]
    , MGStrafe
        ~> [ DPad (Key ScancodeLeft) (Key ScancodeUp) (Key ScancodeDown) (Key ScancodeRight)
           , RightStick 1.0 0.15
           ]
    , MGZoom ~> [AsAxis (Key ScancodeEquals), AsAxis (GamepadButton ControllerButtonDpadUp)]
    , MGThrottle
        ~> [AsAxis (Key ScancodeW), GamepadTrigger ControllerAxisTriggerLeft 1.0 0.1]
    , MGLeanAxis
        ~> [ GamepadTrigger ControllerAxisLeftX 1.0 0.2
           , GamepadTrigger ControllerAxisRightY 1.0 0.15
           ]
    ]

  largeGameSingleBindings =
    [ LGJump ~> [Key ScancodeSpace]
    , LGCrouch ~> [Key ScancodeLCtrl]
    , LGSprint ~> [Key ScancodeLShift]
    , LGWalk ~> [Key ScancodeLAlt]
    , LGProne ~> [Key ScancodeZ]
    , LGClimb ~> [Key ScancodeSpace]
    , LGSlide ~> [Key ScancodeC]
    , LGRoll ~> [Key ScancodeV]
    , LGMove ~> [LeftStick 1.0 0.15]
    , LGMoveVertical ~> [GamepadTrigger ControllerAxisRightY 1.0 0.1]
    , -- Add remaining actions with bindings...
      LGShoot ~> [MouseButton MouseButtonLeft]
    , LGADS ~> [MouseButton MouseButtonRight]
    , LGReload ~> [Key ScancodeR]
    , LGMelee ~> [Key ScancodeF]
    , LGGrenade ~> [Key ScancodeG]
    , LGThrowGrenade ~> [Key ScancodeT]
    , LGNextWeapon ~> [Key ScancodeE]
    , LGPrevWeapon ~> [Key ScancodeQ]
    , LGWeapon1 ~> [Key Scancode1]
    , LGWeapon2 ~> [Key Scancode2]
    , LGWeapon3 ~> [Key Scancode3]
    , LGWeapon4 ~> [Key Scancode4]
    , LGWeapon5 ~> [Key Scancode5]
    , LGToggleFireMode ~> [Key ScancodeB]
    , LGToggleSafety ~> [Key ScancodeN]
    , LGLook ~> [MouseMotion 1.0]
    , LGLookHorizontal ~> [MouseAxis1D MouseX 1.0]
    , LGLookVertical ~> [MouseAxis1D MouseY 1.0]
    , LGZoom ~> [AsAxis (Key ScancodeEquals)]
    , LGCycleZoom ~> [Key ScancodeX]
    , LGUse ~> [Key ScancodeE]
    , LGInteract ~> [Key ScancodeF]
    , LGPickUp ~> [Key ScancodeG]
    , LGDrop ~> [Key ScancodeH]
    , LGExamine ~> [Key ScancodeY]
    , LGOpen ~> [Key ScancodeO]
    , LGClose ~> [Key ScancodeP]
    , LGLock ~> [Key ScancodeL]
    , LGUnlock ~> [Key ScancodeU]
    , LGTalk ~> [Key ScancodeT]
    , LGTrade ~> [Key ScancodeJ]
    , LGGive ~> [Key ScancodeK]
    , LGTake ~> [Key ScancodeComma]
    , LGCraft ~> [Key ScancodePeriod]
    , LGRepair ~> [Key ScancodeSlash]
    , LGInventory ~> [Key ScancodeI]
    , LGMap ~> [Key ScancodeM]
    , LGJournal ~> [Key ScancodeJ]
    , LGQuests ~> [Key ScancodeQ]
    , LGSkills ~> [Key ScancodeK]
    , LGEquipment ~> [Key ScancodeE]
    , LGStats ~> [Key ScancodeC]
    , LGSettings ~> [Key ScancodeEscape]
    , LGPause ~> [Key ScancodeEscape]
    , LGQuickSave ~> [Key ScancodeF5]
    , LGQuickLoad ~> [Key ScancodeF9]
    , LGScreenshot ~> [Key ScancodeF12]
    , LGMenuUp ~> [Key ScancodeUp]
    , LGMenuDown ~> [Key ScancodeDown]
    , LGMenuLeft ~> [Key ScancodeLeft]
    , LGMenuRight ~> [Key ScancodeRight]
    , LGMenuSelect ~> [Key ScancodeReturn]
    , LGMenuBack ~> [Key ScancodeEscape]
    , LGMenuNavigate ~> [DPad (Key ScancodeLeft) (Key ScancodeUp) (Key ScancodeDown) (Key ScancodeRight)]
    , LGTabLeft ~> [Key ScancodeTab]
    , LGVehicleEnter ~> [Key ScancodeF]
    , LGVehicleExit ~> [Key ScancodeF]
    , LGVehicleAccel ~> [AsAxis (Key ScancodeW)]
    , LGVehicleBrake ~> [AsAxis (Key ScancodeS)]
    , LGVehicleSteer ~> [GamepadTrigger ControllerAxisLeftX 1.0 0.1]
    , LGVehicleSteerXY ~> [LeftStick 1.0 0.15]
    , LGVehicleHandbrake ~> [Key ScancodeSpace]
    , LGVehicleBoost ~> [Key ScancodeLShift]
    , LGVehicleHorn ~> [Key ScancodeH]
    , LGVehicleLights ~> [Key ScancodeL]
    , LGVehicleCamera ~> [Key ScancodeC]
    , LGVehicleRadio ~> [Key ScancodeR]
    , LGVehicleShoot ~> [MouseButton MouseButtonLeft]
    , LGVehicleSpecial ~> [Key ScancodeX]
    , LGVehicleEject ~> [Key ScancodeE]
    , LGVoiceChat ~> [Key ScancodeV]
    , LGTextChat ~> [Key ScancodeT]
    , LGEmote1 ~> [Key ScancodeZ]
    , LGEmote2 ~> [Key ScancodeX]
    , LGEmote3 ~> [Key ScancodeC]
    , LGEmote4 ~> [Key ScancodeV]
    , LGPing ~> [Key ScancodeG]
    , LGMark ~> [MouseButton MouseButtonMiddle]
    , LGTeamMenu ~> [Key ScancodeU]
    , LGScoreboard ~> [Key ScancodeTab]
    , LGToggleCamera ~> [Key ScancodeV]
    , LGCameraZoom ~> [AsAxis (Key ScancodeEquals)]
    , LGCameraRotate ~> [MouseMotion 1.0]
    , LGCameraPan ~> [RightStick 1.0 0.2]
    , LGCameraUp ~> [Key ScancodeUp]
    , LGCameraDown ~> [Key ScancodeDown]
    , LGCameraLeft ~> [Key ScancodeLeft]
    , LGCameraRight ~> [Key ScancodeRight]
    , LGFreeLook ~> [Key ScancodeLAlt]
    , LGResetCamera ~> [Key ScancodeHome]
    ]

  largeGameMultipleBindings =
    [ LGJump ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]
    , LGCrouch ~> [Key ScancodeLCtrl, GamepadButton ControllerButtonB]
    , LGSprint ~> [Key ScancodeLShift, GamepadButton ControllerButtonLeftStick]
    , LGWalk ~> [Key ScancodeLAlt, GamepadButton ControllerButtonRightStick]
    , LGProne ~> [Key ScancodeZ, GamepadButton ControllerButtonDpadDown]
    , LGClimb ~> [Key ScancodeSpace, GamepadButton ControllerButtonY]
    , LGSlide ~> [Key ScancodeC, GamepadButton ControllerButtonB]
    , LGRoll ~> [Key ScancodeV, GamepadButton ControllerButtonX]
    , LGMove
        ~> [DPad (Key ScancodeA) (Key ScancodeW) (Key ScancodeS) (Key ScancodeD), LeftStick 1.0 0.15]
    , LGMoveVertical
        ~> [GamepadTrigger ControllerAxisLeftY 1.0 0.1, AsAxis (Key ScancodeSpace)]
    , LGShoot ~> [MouseButton MouseButtonLeft, GamepadButton ControllerButtonRightShoulder]
    , LGADS ~> [MouseButton MouseButtonRight, GamepadButton ControllerButtonLeftShoulder]
    , LGReload ~> [Key ScancodeR, GamepadButton ControllerButtonY]
    , LGMelee ~> [Key ScancodeF, GamepadButton ControllerButtonRightStick]
    , LGGrenade ~> [Key ScancodeG, GamepadButton ControllerButtonLeftShoulder]
    , LGThrowGrenade ~> [Key ScancodeT, GamepadButton ControllerButtonDpadUp]
    , LGNextWeapon ~> [Key ScancodeE, GamepadButton ControllerButtonDpadRight]
    , LGPrevWeapon ~> [Key ScancodeQ, GamepadButton ControllerButtonDpadLeft]
    , LGWeapon1 ~> [Key Scancode1, GamepadButton ControllerButtonA]
    , LGWeapon2 ~> [Key Scancode2, GamepadButton ControllerButtonB]
    , LGWeapon3 ~> [Key Scancode3, GamepadButton ControllerButtonX]
    , LGWeapon4 ~> [Key Scancode4, GamepadButton ControllerButtonY]
    , LGWeapon5 ~> [Key Scancode5, GamepadButton ControllerButtonLeftShoulder]
    , LGToggleFireMode ~> [Key ScancodeB, GamepadButton ControllerButtonBack]
    , LGToggleSafety ~> [Key ScancodeN, GamepadButton ControllerButtonStart]
    , LGLook ~> [MouseMotion 1.0, RightStick 2.0 0.2]
    , LGLookHorizontal
        ~> [MouseAxis1D MouseX 1.0, GamepadTrigger ControllerAxisLeftX 2.0 0.2]
    , LGLookVertical
        ~> [MouseAxis1D MouseY 1.0, GamepadTrigger ControllerAxisLeftY 2.0 0.2]
    , LGZoom ~> [AsAxis (Key ScancodeEquals), AsAxis (GamepadButton ControllerButtonDpadUp)]
    , LGCycleZoom ~> [Key ScancodeX, GamepadButton ControllerButtonRightStick]
    , LGUse ~> [Key ScancodeE, GamepadButton ControllerButtonX]
    , LGInteract ~> [Key ScancodeF, GamepadButton ControllerButtonA]
    , LGPickUp ~> [Key ScancodeG, GamepadButton ControllerButtonY]
    , LGDrop ~> [Key ScancodeH, GamepadButton ControllerButtonB]
    , LGExamine ~> [Key ScancodeY, GamepadButton ControllerButtonDpadDown]
    , LGOpen ~> [Key ScancodeO, GamepadButton ControllerButtonDpadUp]
    , LGClose ~> [Key ScancodeP, GamepadButton ControllerButtonDpadRight]
    , LGLock ~> [Key ScancodeL, GamepadButton ControllerButtonLeftShoulder]
    , LGUnlock ~> [Key ScancodeU, GamepadButton ControllerButtonRightShoulder]
    , LGTalk ~> [Key ScancodeT, GamepadButton ControllerButtonStart]
    , LGTrade ~> [Key ScancodeJ, GamepadButton ControllerButtonBack]
    , LGGive ~> [Key ScancodeK, GamepadButton ControllerButtonDpadLeft]
    , LGTake ~> [Key ScancodeComma, GamepadButton ControllerButtonDpadDown]
    , LGCraft ~> [Key ScancodePeriod, GamepadButton ControllerButtonY]
    , LGRepair ~> [Key ScancodeSlash, GamepadButton ControllerButtonB]
    , LGInventory ~> [Key ScancodeI, GamepadButton ControllerButtonStart]
    , LGMap ~> [Key ScancodeM, GamepadButton ControllerButtonBack]
    , LGJournal ~> [Key ScancodeJ, GamepadButton ControllerButtonDpadUp]
    , LGQuests ~> [Key ScancodeQ, GamepadButton ControllerButtonDpadRight]
    , LGSkills ~> [Key ScancodeK, GamepadButton ControllerButtonDpadLeft]
    , LGEquipment ~> [Key ScancodeE, GamepadButton ControllerButtonDpadDown]
    , LGStats ~> [Key ScancodeC, GamepadButton ControllerButtonLeftStick]
    , LGSettings ~> [Key ScancodeEscape, GamepadButton ControllerButtonStart]
    , LGPause ~> [Key ScancodeEscape, GamepadButton ControllerButtonStart]
    , LGQuickSave ~> [Key ScancodeF5, GamepadButton ControllerButtonLeftShoulder]
    , LGQuickLoad ~> [Key ScancodeF9, GamepadButton ControllerButtonRightShoulder]
    , LGScreenshot ~> [Key ScancodeF12, GamepadButton ControllerButtonGuide]
    , LGMenuUp ~> [Key ScancodeUp, GamepadButton ControllerButtonDpadUp]
    , LGMenuDown ~> [Key ScancodeDown, GamepadButton ControllerButtonDpadDown]
    , LGMenuLeft ~> [Key ScancodeLeft, GamepadButton ControllerButtonDpadLeft]
    , LGMenuRight ~> [Key ScancodeRight, GamepadButton ControllerButtonDpadRight]
    , LGMenuSelect ~> [Key ScancodeReturn, GamepadButton ControllerButtonA]
    , LGMenuBack ~> [Key ScancodeEscape, GamepadButton ControllerButtonB]
    , LGMenuNavigate
        ~> [ DPad (Key ScancodeLeft) (Key ScancodeUp) (Key ScancodeDown) (Key ScancodeRight)
           , LeftStick 1.0 0.15
           ]
    , LGTabLeft ~> [Key ScancodeTab, GamepadButton ControllerButtonLeftShoulder]
    , LGVehicleEnter ~> [Key ScancodeF, GamepadButton ControllerButtonY]
    , LGVehicleExit ~> [Key ScancodeF, GamepadButton ControllerButtonB]
    , LGVehicleAccel
        ~> [AsAxis (Key ScancodeW), GamepadTrigger ControllerAxisTriggerRight 1.0 0.1]
    , LGVehicleBrake
        ~> [AsAxis (Key ScancodeS), GamepadTrigger ControllerAxisTriggerLeft 1.0 0.1]
    , LGVehicleSteer
        ~> [ GamepadTrigger ControllerAxisRightX 1.0 0.1
           , GamepadTrigger ControllerAxisLeftX 1.0 0.15
           ]
    , LGVehicleSteerXY
        ~> [LeftStick 1.0 0.15, DPad (Key ScancodeA) (Key ScancodeW) (Key ScancodeS) (Key ScancodeD)]
    , LGVehicleHandbrake ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]
    , LGVehicleBoost ~> [Key ScancodeLShift, GamepadButton ControllerButtonX]
    , LGVehicleHorn ~> [Key ScancodeH, GamepadButton ControllerButtonLeftStick]
    , LGVehicleLights ~> [Key ScancodeL, GamepadButton ControllerButtonRightStick]
    , LGVehicleCamera ~> [Key ScancodeC, GamepadButton ControllerButtonBack]
    , LGVehicleRadio ~> [Key ScancodeR, GamepadButton ControllerButtonStart]
    , LGVehicleShoot ~> [MouseButton MouseButtonLeft, GamepadButton ControllerButtonRightShoulder]
    , LGVehicleSpecial ~> [Key ScancodeX, GamepadButton ControllerButtonY]
    , LGVehicleEject ~> [Key ScancodeE, GamepadButton ControllerButtonB]
    , LGVoiceChat ~> [Key ScancodeV, GamepadButton ControllerButtonDpadUp]
    , LGTextChat ~> [Key ScancodeT, GamepadButton ControllerButtonDpadDown]
    , LGEmote1 ~> [Key ScancodeZ, GamepadButton ControllerButtonDpadLeft]
    , LGEmote2 ~> [Key ScancodeX, GamepadButton ControllerButtonDpadRight]
    , LGEmote3 ~> [Key ScancodeC, GamepadButton ControllerButtonDpadUp]
    , LGEmote4 ~> [Key ScancodeV, GamepadButton ControllerButtonDpadDown]
    , LGPing ~> [Key ScancodeG, GamepadButton ControllerButtonLeftShoulder]
    , LGMark ~> [MouseButton MouseButtonMiddle, GamepadButton ControllerButtonRightShoulder]
    , LGTeamMenu ~> [Key ScancodeU, GamepadButton ControllerButtonBack]
    , LGScoreboard ~> [Key ScancodeTab, GamepadButton ControllerButtonStart]
    , LGToggleCamera ~> [Key ScancodeV, GamepadButton ControllerButtonRightStick]
    , LGCameraZoom
        ~> [AsAxis (Key ScancodeEquals), GamepadTrigger ControllerAxisLeftY 1.0 0.2]
    , LGCameraRotate ~> [MouseMotion 1.0, RightStick 1.5 0.2]
    , LGCameraPan ~> [RightStick 1.0 0.2, MouseMotion 0.5]
    , LGCameraUp ~> [Key ScancodeUp, GamepadButton ControllerButtonDpadUp]
    , LGCameraDown ~> [Key ScancodeDown, GamepadButton ControllerButtonDpadDown]
    , LGCameraLeft ~> [Key ScancodeLeft, GamepadButton ControllerButtonDpadLeft]
    , LGCameraRight ~> [Key ScancodeRight, GamepadButton ControllerButtonDpadRight]
    , LGFreeLook ~> [Key ScancodeLAlt, GamepadButton ControllerButtonLeftShoulder]
    , LGResetCamera ~> [Key ScancodeHome, GamepadButton ControllerButtonLeftStick]
    ]

-- ============================================================================
-- Input Query Benchmarks (HOT PATH)
-- ============================================================================

inputQueryBenchmarks :: Benchmark
inputQueryBenchmarks =
  bgroup
    "Input Queries (Hot Path)"
    [ bgroup
        "absoluteInput - Button"
        [ bench "Single binding" $
            perRunEnv setupSmallGameSingle $ \ ~(buf, mp) ->
              absoluteInput buf mp SGJump
        , bench "Multiple bindings (2)" $
            perRunEnv setupSmallGameMultiple $ \ ~(buf, mp) ->
              absoluteInput buf mp SGJump
        , bench "Many bindings (5)" $
            perRunEnv setupSmallGameMany $ \ ~(buf, mp) ->
              absoluteInput buf mp SGJump
        ]
    , bgroup
        "absoluteInput - Axis1D"
        [ bench "Single binding" $
            perRunEnv setupSmallGameSingle $ \ ~(buf, mp) ->
              absoluteInput buf mp SGZoom
        , bench "Multiple bindings (2)" $
            perRunEnv setupSmallGameMultiple $ \ ~(buf, mp) ->
              absoluteInput buf mp SGZoom
        , bench "Many bindings (5)" $
            perRunEnv setupSmallGameMany $ \ ~(buf, mp) ->
              absoluteInput buf mp SGZoom
        ]
    , bgroup
        "absoluteInput - Axis2D"
        [ bench "Single binding" $
            perRunEnv setupSmallGameSingle $ \ ~(buf, mp) ->
              absoluteInput buf mp SGMove
        , bench "Multiple bindings (2)" $
            perRunEnv setupSmallGameMultiple $ \ ~(buf, mp) ->
              absoluteInput buf mp SGMove
        , bench "Many bindings (5)" $
            perRunEnv setupSmallGameMany $ \ ~(buf, mp) ->
              absoluteInput buf mp SGMove
        ]
    , bgroup
        "deltaInput - Button"
        [ bench "Single binding" $
            perRunEnv setupSmallGameSingle $ \ ~(buf, mp) ->
              deltaInput buf mp SGJump
        , bench "Multiple bindings (2)" $
            perRunEnv setupSmallGameMultiple $ \ ~(buf, mp) ->
              deltaInput buf mp SGJump
        ]
    , bgroup
        "deltaInput - Axis2D"
        [ bench "Single binding" $
            perRunEnv setupSmallGameSingle $ \ ~(buf, mp) ->
              deltaInput buf mp SGLook
        , bench "Multiple bindings (2)" $
            perRunEnv setupSmallGameMultiple $ \ ~(buf, mp) ->
              deltaInput buf mp SGLook
        ]
    , bgroup
        "Sequential queries (simulate game loop)"
        [ bench "10 actions" $
            perRunEnv setupSmallGameMultiple $ \ ~(buf, mp) -> do
              !_ <- absoluteInput buf mp SGJump
              !_ <- absoluteInput buf mp SGCrouch
              !_ <- absoluteInput buf mp SGSprint
              !_ <- absoluteInput buf mp SGShoot
              !_ <- absoluteInput buf mp SGReload
              !_ <- absoluteInput buf mp SGMove
              !_ <- absoluteInput buf mp SGLook
              !_ <- deltaInput buf mp SGInteract
              !_ <- deltaInput buf mp SGZoom
              deltaInput buf mp SGThrottle
        , bench "20 actions" $
            perRunEnv setupMediumGameMultiple $ \ ~(buf, mp) -> do
              !_ <- absoluteInput buf mp MGJump
              !_ <- absoluteInput buf mp MGCrouch
              !_ <- absoluteInput buf mp MGSprint
              !_ <- absoluteInput buf mp MGShoot
              !_ <- absoluteInput buf mp MGReload
              !_ <- absoluteInput buf mp MGMelee
              !_ <- absoluteInput buf mp MGGrenade
              !_ <- absoluteInput buf mp MGSwitchWeapon
              !_ <- absoluteInput buf mp MGUse
              !_ <- absoluteInput buf mp MGInventory
              !_ <- absoluteInput buf mp MGMove
              !_ <- absoluteInput buf mp MGLook
              !_ <- absoluteInput buf mp MGStrafe
              !_ <- deltaInput buf mp MGInteract
              !_ <- deltaInput buf mp MGMap
              !_ <- deltaInput buf mp MGPause
              !_ <- deltaInput buf mp MGScreenshot
              !_ <- deltaInput buf mp MGZoom
              !_ <- deltaInput buf mp MGThrottle
              deltaInput buf mp MGLeanAxis
        ]
    ]
 where
  setupSmallGameSingle = do
    buf <- Buf.newBufferedInput
    let mp = smallGameSingleBindings
    pure (buf, mp)
   where
    smallGameSingleBindings =
      newActionMap @SmallGame
        [ SGJump ~> [Key ScancodeSpace]
        , SGCrouch ~> [Key ScancodeLCtrl]
        , SGSprint ~> [Key ScancodeLShift]
        , SGInteract ~> [Key ScancodeE]
        , SGShoot ~> [MouseButton MouseButtonLeft]
        , SGReload ~> [Key ScancodeR]
        , SGMove ~> [LeftStick 1.0 0.15]
        , SGLook ~> [MouseMotion 1.0]
        , SGZoom ~> [AsAxis (Key ScancodeEquals)]
        , SGThrottle ~> [AsAxis (Key ScancodeW)]
        ]

  setupSmallGameMultiple = do
    buf <- Buf.newBufferedInput
    let mp = smallGameMultipleBindings
    pure (buf, mp)
   where
    smallGameMultipleBindings =
      newActionMap @SmallGame
        [ SGJump ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]
        , SGCrouch ~> [Key ScancodeLCtrl, GamepadButton ControllerButtonB]
        , SGSprint ~> [Key ScancodeLShift, GamepadButton ControllerButtonLeftStick]
        , SGInteract ~> [Key ScancodeE, GamepadButton ControllerButtonX]
        , SGShoot ~> [MouseButton MouseButtonLeft, GamepadButton ControllerButtonRightShoulder]
        , SGReload ~> [Key ScancodeR, GamepadButton ControllerButtonY]
        , SGMove
            ~> [DPad (Key ScancodeA) (Key ScancodeW) (Key ScancodeS) (Key ScancodeD), LeftStick 1.0 0.15]
        , SGLook ~> [MouseMotion 1.0, RightStick 2.0 0.2]
        , SGZoom ~> [AsAxis (Key ScancodeEquals), AsAxis (GamepadButton ControllerButtonDpadUp)]
        , SGThrottle
            ~> [AsAxis (Key ScancodeW), GamepadTrigger ControllerAxisTriggerRight 1.0 0.1]
        ]

  setupSmallGameMany = do
    buf <- Buf.newBufferedInput
    let mp = smallGameManyBindings
    pure (buf, mp)
   where
    smallGameManyBindings =
      newActionMap @SmallGame
        [ SGJump
            ~> [ Key ScancodeSpace
               , GamepadButton ControllerButtonA
               , GamepadButton ControllerButtonB
               , Key ScancodeUp
               , GamepadButton ControllerButtonDpadUp
               ]
        , SGCrouch
            ~> [ Key ScancodeLCtrl
               , GamepadButton ControllerButtonB
               , Key ScancodeC
               , GamepadButton ControllerButtonRightStick
               , Key ScancodeDown
               ]
        , SGSprint
            ~> [ Key ScancodeLShift
               , GamepadButton ControllerButtonLeftStick
               , Key ScancodeRShift
               , GamepadButton ControllerButtonX
               , GamepadButton ControllerButtonY
               ]
        , SGInteract
            ~> [ Key ScancodeE
               , GamepadButton ControllerButtonX
               , Key ScancodeF
               , MouseButton MouseButtonMiddle
               , GamepadButton ControllerButtonA
               ]
        , SGShoot
            ~> [ MouseButton MouseButtonLeft
               , GamepadButton ControllerButtonRightShoulder
               , Key ScancodeLCtrl
               , GamepadButton ControllerButtonA
               , MouseButton MouseButtonRight
               ]
        , SGReload
            ~> [ Key ScancodeR
               , GamepadButton ControllerButtonY
               , Key ScancodeT
               , GamepadButton ControllerButtonLeftShoulder
               , MouseButton MouseButtonMiddle
               ]
        , SGMove
            ~> [ DPad (Key ScancodeA) (Key ScancodeW) (Key ScancodeS) (Key ScancodeD)
               , LeftStick 1.0 0.15
               , DPad (Key ScancodeLeft) (Key ScancodeUp) (Key ScancodeDown) (Key ScancodeRight)
               , DPad
                  (GamepadButton ControllerButtonDpadLeft)
                  (GamepadButton ControllerButtonDpadUp)
                  (GamepadButton ControllerButtonDpadDown)
                  (GamepadButton ControllerButtonDpadRight)
               , DPad (Key Scancode4) (Key Scancode8) (Key Scancode5) (Key Scancode6)
               ]
        , SGLook
            ~> [ MouseMotion 1.0
               , RightStick 2.0 0.2
               , GamepadStick ControllerAxisLeftX ControllerAxisLeftY 1.5 0.15
               , DPad (Key ScancodeJ) (Key ScancodeI) (Key ScancodeK) (Key ScancodeL)
               , MouseAxis2D MouseX MouseY 0.8
               ]
        , SGZoom
            ~> [ AsAxis (Key ScancodeEquals)
               , AsAxis (GamepadButton ControllerButtonDpadUp)
               , AsAxis (Key ScancodeEquals)
               , GamepadTrigger ControllerAxisTriggerLeft 1.0 0.1
               , AsAxis (Key ScancodeMinus)
               ]
        , SGThrottle
            ~> [ AsAxis (Key ScancodeW)
               , GamepadTrigger ControllerAxisTriggerLeft 1.0 0.1
               , AsAxis (MouseButton MouseButtonRight)
               , GamepadTrigger ControllerAxisLeftY 1.0 0.15
               , AsAxis (Key ScancodeSpace)
               ]
        ]

  setupMediumGameMultiple = do
    buf <- Buf.newBufferedInput
    let mp = mediumGameMultipleBindings
    pure (buf, mp)
   where
    mediumGameMultipleBindings =
      newActionMap @MediumGame
        [ MGJump ~> [Key ScancodeSpace, GamepadButton ControllerButtonA]
        , MGCrouch ~> [Key ScancodeLCtrl, GamepadButton ControllerButtonB]
        , MGSprint ~> [Key ScancodeLShift, GamepadButton ControllerButtonLeftStick]
        , MGInteract ~> [Key ScancodeE, GamepadButton ControllerButtonX]
        , MGShoot ~> [MouseButton MouseButtonLeft, GamepadButton ControllerButtonRightShoulder]
        , MGReload ~> [Key ScancodeR, GamepadButton ControllerButtonY]
        , MGMelee ~> [Key ScancodeF, GamepadButton ControllerButtonRightStick]
        , MGGrenade ~> [Key ScancodeG, GamepadButton ControllerButtonLeftShoulder]
        , MGSwitchWeapon ~> [Key ScancodeQ, GamepadButton ControllerButtonDpadDown]
        , MGUse ~> [Key ScancodeE, GamepadButton ControllerButtonX]
        , MGMap ~> [Key ScancodeM, GamepadButton ControllerButtonBack]
        , MGInventory ~> [Key ScancodeI, GamepadButton ControllerButtonStart]
        , MGPause ~> [Key ScancodeEscape, GamepadButton ControllerButtonStart]
        , MGScreenshot ~> [Key ScancodeF12, GamepadButton ControllerButtonGuide]
        , MGMove
            ~> [DPad (Key ScancodeA) (Key ScancodeW) (Key ScancodeS) (Key ScancodeD), LeftStick 1.0 0.15]
        , MGLook ~> [MouseMotion 1.0, RightStick 2.0 0.2]
        , MGStrafe
            ~> [ DPad (Key ScancodeLeft) (Key ScancodeUp) (Key ScancodeDown) (Key ScancodeRight)
               , RightStick 1.0 0.15
               ]
        , MGZoom ~> [AsAxis (Key ScancodeEquals), AsAxis (GamepadButton ControllerButtonDpadUp)]
        , MGThrottle
            ~> [AsAxis (Key ScancodeW), GamepadTrigger ControllerAxisTriggerRight 1.0 0.1]
        , MGLeanAxis
            ~> [ GamepadTrigger ControllerAxisRightX 1.0 0.2
               , GamepadTrigger ControllerAxisLeftY 1.0 0.15
               ]
        ]

-- ============================================================================
-- Frame Management Benchmarks
-- ============================================================================

frameManagementBenchmarks :: Benchmark
frameManagementBenchmarks =
  bgroup
    "Frame Management"
    [ bench "newBufferedInput" $ nfIO newBufferedInput
    , bench "prepareBufferedInput" $
        perRunEnv newBufferedInput $ \ ~buf ->
          prepareBufferedInput buf
    , bench "Full frame cycle" $
        perRunEnv setupFrame $ \ ~(buf, mp) -> do
          prepareBufferedInput buf
          -- Simulate writing some input
          MPA.write buf.thisInput.kbScancodes ScancodeW True
          MPA.write buf.thisInput.kbScancodes ScancodeSpace True
          MPA.write buf.thisInput.mouseAxes MouseX 5.0
          MPA.write buf.thisInput.mouseAxes MouseY (-2.5)
          -- Query some actions
          !_ <- absoluteInput buf mp SGMove
          !_ <- deltaInput buf mp SGJump
          deltaInput buf mp SGLook
    ]
 where
  setupFrame = do
    buf <- Buf.newBufferedInput
    let mp =
          newActionMap @SmallGame
            [ SGJump ~> [Key ScancodeSpace]
            , SGMove ~> [DPad (Key ScancodeA) (Key ScancodeW) (Key ScancodeS) (Key ScancodeD)]
            , SGLook ~> [MouseMotion 1.0]
            , SGShoot ~> [MouseButton MouseButtonLeft]
            , SGReload ~> [Key ScancodeR]
            , SGCrouch ~> [Key ScancodeLCtrl]
            , SGSprint ~> [Key ScancodeLShift]
            , SGInteract ~> [Key ScancodeE]
            , SGZoom ~> [AsAxis (Key ScancodeEquals)]
            , SGThrottle ~> [AsAxis (Key ScancodeW)]
            ]
    pure (buf, mp)

-- ============================================================================
-- Baseline Comparison Benchmarks
-- ============================================================================

baselineBenchmarks :: Benchmark
baselineBenchmarks =
  bgroup
    "Baselines (Raw Buffer Access)"
    [ bench "Single MPA.read (Bool)" $
        perRunEnv setupBaseline $ \ ~buf ->
          MPA.read buf.thisInput.kbScancodes ScancodeSpace
    , bench "Single MPA.read (Float)" $
        perRunEnv setupBaseline $ \ ~buf ->
          MPA.read buf.thisInput.mouseAxes MouseX
    , bench "Two MPA.read (V2 simulation)" $
        perRunEnv setupBaseline $ \ ~buf -> do
          x <- MPA.read buf.thisInput.mouseAxes MouseX
          y <- MPA.read buf.thisInput.mouseAxes MouseY
          pure $ V2 x y
    , bench "Delta query (read last + this)" $
        perRunEnv setupBaseline $ \ ~buf -> do
          last' <- MPA.read buf.lastInput.kbScancodes ScancodeSpace
          this <- MPA.read buf.thisInput.kbScancodes ScancodeSpace
          pure (last', this)
    , bench "10 raw reads (game loop simulation)" $
        perRunEnv setupBaseline $ \ ~buf -> do
          !_ <- MPA.read buf.thisInput.kbScancodes ScancodeSpace
          !_ <- MPA.read buf.thisInput.kbScancodes ScancodeLCtrl
          !_ <- MPA.read buf.thisInput.kbScancodes ScancodeLShift
          !_ <- MPA.read buf.thisInput.kbScancodes ScancodeE
          !_ <- MPA.read buf.thisInput.mouseButtons MouseButtonLeft
          !_ <- MPA.read buf.thisInput.kbScancodes ScancodeR
          !_ <- MPA.read buf.thisInput.kbScancodes ScancodeW
          !_ <- MPA.read buf.thisInput.mouseAxes MouseX
          !_ <- MPA.read buf.thisInput.mouseAxes MouseY
          MPA.read buf.thisInput.kbScancodes ScancodeA
    , bench "prepareBufferedInput vs manual buffer swap" $
        perRunEnv setupBaseline $ \ ~buf -> do
          Buf.copyInputBuffer buf.lastInput buf.thisInput
          MPA.write buf.thisInput.mouseAxes MouseX 0
          MPA.write buf.thisInput.mouseAxes MouseY 0
    ]
 where
  setupBaseline = Buf.newBufferedInput
