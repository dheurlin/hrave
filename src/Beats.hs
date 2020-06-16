{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

module Beats where

import DataTypes
import Animations

import           Foreign.C.Types                ( CLong )


-- bass :: Animation [MidiMessage]
-- bass = cycleAnimation (Just 8)
--   $ Animation [(0, [noteAOff, noteDOn]), (8, [noteDOff, noteAOn])]
--  where
--   noteDOn =
--     MidiMessage 1 $ NoteOn (pianoNoteToMidi $ PianoNote D NoteNormal 1) 100
--   noteDOff =
--     MidiMessage 1 $ NoteOff (pianoNoteToMidi $ PianoNote D NoteNormal 1) 100

--   noteAOn =
--     MidiMessage 1 $ NoteOn (pianoNoteToMidi $ PianoNote A NoteNormal 2) 100
--   noteAOff =
--     MidiMessage 1 $ NoteOff (pianoNoteToMidi $ PianoNote A NoteNormal 2) 100


data BeatUnit = BeatOn | BeatOff | BeatEmpty
  deriving ( Show )

instance Empty BeatUnit where
  emptyElem = BeatEmpty

beatToMidi :: CLong -> [Note] -> BeatUnit -> [MidiMessage]
beatToMidi channel ns BeatOn =
  [ MidiMessage channel $ NoteOn note 100 | note <- ns ]
beatToMidi channel ns BeatOff =
  [ MidiMessage channel $ NoteOff note 100 | note <- [0 .. 127] ]
beatToMidi channel ns BeatEmpty = []

class BeatRep b where
  type BeatContents b :: *
  beatOn              :: b -> BeatContents b
  beatOff             :: b -> BeatContents b

instance BeatRep () where
  type BeatContents () = BeatUnit
  beatOff              = const BeatOff
  beatOn               = const BeatOff

instance BeatRep [Note] where
  type BeatContents [Note] = [MidiChannel -> Velocity -> MidiMessage]
  beatOff                  = map noteOnMsg
  beatOn                   = map noteOffMsg


data NoteRole a = NNote a | NPause
  deriving ( Eq, Show )

data NoteDuration = DWhole
                  | DHalf
                  | DQuarter
                  | DEight
                  | DSixteenth
                  | DDotted NoteDuration
  deriving ( Eq, Show )

data BNote a = BNote  NoteDuration (NoteRole a)
             | BTuple NoteDuration [NoteRole a]
  deriving ( Eq, Show )

newtype Sequence a = Sequence [BNote a]

-- TODO should be able to program parts and fuse them into single animation


beat :: Animation BeatUnit
beat = cycleAnimation $ Animation
  [ (3, BeatOn) , (1, BeatOff)
  , (3, BeatOn) , (1, BeatOff)
  , (3, BeatOn), (1, BeatOff)
  , pauseFrame 4
  ]

