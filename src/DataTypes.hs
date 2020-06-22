{-# LANGUAGE FlexibleInstances #-}

module DataTypes where

import           Foreign.C.Types                ( CLong )

type Tick = Integer

class Empty a where
  emptyElem :: a

instance Empty [a] where
  emptyElem = []

type Note        = CLong
type MidiChannel = CLong
type Velocity    = CLong

