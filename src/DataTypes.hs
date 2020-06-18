{-# LANGUAGE FlexibleInstances #-}

module DataTypes where

import           Foreign.C.Types                ( CLong )

type Tick = Integer

class Empty a where
  emptyElem :: a

instance Empty [a] where
  emptyElem = []

type Octave = Int

data NoteName = A | B | C | D | E | F | G
  deriving ( Eq, Show, Enum, Read )

data NoteMod = NoteNormal | NoteSharp
  deriving ( Eq )

instance Show NoteMod where
  show NoteNormal = ""
  show NoteSharp  = "#"

data PianoNote = PianoNote NoteName NoteMod Octave
  deriving ( Eq )

instance Show PianoNote where
  show (PianoNote name mod oct) = show name <> show mod <> show oct

type Note        = CLong
type MidiChannel = CLong
type Velocity    = CLong

data MsgType = NoteOn  Note Velocity
             | NoteOff Note Velocity
  deriving (Show)

data MidiMessage = MidiMessage { msgChannel :: MidiChannel
                               , msgType    :: MsgType }
  deriving (Show)

noteOnMsg :: Note -> MidiChannel -> Velocity -> MidiMessage
noteOnMsg n c v = MidiMessage c $ NoteOn n v

noteOffMsg :: Note -> MidiChannel -> Velocity -> MidiMessage
noteOffMsg n c v = MidiMessage c $ NoteOff n 0

type MidiFunc = MidiChannel -> Velocity -> MidiMessage
