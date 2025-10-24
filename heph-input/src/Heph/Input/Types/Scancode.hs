{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Heph.Input.Types.Scancode (Scancode (..)) where

import Heph.Input.Internal.BoundedArray.Primitive.Mutable

import Control.DeepSeq
import GHC.Generics

data Scancode
  = -- Letters
    ScancodeA
  | ScancodeB
  | ScancodeC
  | ScancodeD
  | ScancodeE
  | ScancodeF
  | ScancodeG
  | ScancodeH
  | ScancodeI
  | ScancodeJ
  | ScancodeK
  | ScancodeL
  | ScancodeM
  | ScancodeN
  | ScancodeO
  | ScancodeP
  | ScancodeQ
  | ScancodeR
  | ScancodeS
  | ScancodeT
  | ScancodeU
  | ScancodeV
  | ScancodeW
  | ScancodeX
  | ScancodeY
  | ScancodeZ
  | -- Numbers
    Scancode1
  | Scancode2
  | Scancode3
  | Scancode4
  | Scancode5
  | Scancode6
  | Scancode7
  | Scancode8
  | Scancode9
  | Scancode0
  | -- Function keys
    ScancodeF1
  | ScancodeF2
  | ScancodeF3
  | ScancodeF4
  | ScancodeF5
  | ScancodeF6
  | ScancodeF7
  | ScancodeF8
  | ScancodeF9
  | ScancodeF10
  | ScancodeF11
  | ScancodeF12
  | -- Arrow keys
    ScancodeUp
  | ScancodeDown
  | ScancodeLeft
  | ScancodeRight
  | -- Modifiers
    ScancodeLCtrl
  | ScancodeRCtrl
  | ScancodeLShift
  | ScancodeRShift
  | ScancodeLAlt
  | ScancodeRAlt
  | ScancodeLGUI
  | ScancodeRGUI
  | -- Special keys
    ScancodeSpace
  | ScancodeReturn
  | ScancodeEscape
  | ScancodeBackspace
  | ScancodeTab
  | ScancodeCapsLock
  | ScancodePrintScreen
  | ScancodeScrollLock
  | ScancodePause
  | ScancodeInsert
  | ScancodeDelete
  | ScancodeHome
  | ScancodeEnd
  | ScancodePageUp
  | ScancodePageDown
  | -- Punctuation and symbols
    ScancodeMinus
  | ScancodeEquals
  | ScancodeLeftBracket
  | ScancodeRightBracket
  | ScancodeBackslash
  | ScancodeSemicolon
  | ScancodeApostrophe
  | ScancodeGrave
  | ScancodeComma
  | ScancodePeriod
  | ScancodeSlash
  | -- Numpad
    ScancodeNumLock
  | ScancodeNumDivide
  | ScancodeNumMultiply
  | ScancodeNumMinus
  | ScancodeNumPlus
  | ScancodeNumEnter
  | ScancodeNum1
  | ScancodeNum2
  | ScancodeNum3
  | ScancodeNum4
  | ScancodeNum5
  | ScancodeNum6
  | ScancodeNum7
  | ScancodeNum8
  | ScancodeNum9
  | ScancodeNum0
  | ScancodeNumPeriod
  deriving (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving (Primlike) via (PrimlikeEnum Scancode)

instance NFData Scancode
