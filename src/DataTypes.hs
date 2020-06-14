module DataTypes where

import           Foreign.C.Types                ( CLong )

type Octave = Int

data NoteName = A | B | C | D | E | F | G
  deriving ( Show, Enum )

data NoteMod = NoteNormal | NoteSharp

instance Show NoteMod where
  show NoteNormal = ""
  show NoteSharp  = "#"

data PianoNote = PianoNote NoteName NoteMod Octave

instance Show PianoNote where
  show (PianoNote name mod oct) = show name <> show mod <> show oct

type Note = CLong

data MsgType = NoteOn  Note CLong
             | NoteOff Note CLong
  deriving (Show)

data MidiMessage = MidiMessage { msgChannel :: CLong
                               , msgType    :: MsgType }
  deriving (Show)
