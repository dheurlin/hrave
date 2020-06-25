{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

module Beats where

import Config
import DataTypes
import Animations
import Chords
import Notes
import Midi

------- Data Types for representing units of a beat ---------------------------

type MidiMsgFunc = MidiChannel -> Velocity -> [MidiMessage]

instance Empty MidiMsgFunc where
  emptyElem = const . const []

class BeatRep b where
  type BeatContents b :: *
  beatOn              :: b -> BeatContents b
  beatOff             :: b -> BeatContents b

instance BeatRep [Note] where
  type BeatContents [Note] = MidiMsgFunc
  beatOff _ channel _ = noteOffAll channel
  beatOn ns channel v = [ noteOnMsg n channel v | n <- ns ]


-- | A datatype for a beat that does not have any pitch information of its own,
-- meaning it only specifies the rhythm and applies the currently held notes
data ChordBeat = CBeat

type HeldNoteFun = [Note] -> NoteRange -> OctShift -> MidiMsgFunc

instance BeatRep ChordBeat where
  type BeatContents ChordBeat = HeldNoteFun
  beatOff _ _ _ _     channel _  = noteOffAll channel
  beatOn _ ns rng sft channel v  =
    [ noteOnMsg (shiftToRange rng $ octShift sft n) channel v | n <- ns ]

instance Empty HeldNoteFun where
  emptyElem = const . const .const emptyElem


-- | A dataype representing the role of a note within a chord, i.e. root,
-- fifth, third etc.
data ChordRole = IRoot
               | IFifth
  deriving ( Eq, Show )

-- | A 'ChordRole' together with an octave shift, so we can e.g. go down
-- to the fifth below the root instead of up
data RelNote = RelNote { rRole     :: ChordRole
                       , rOctShift :: OctShift
                       }

relToNote :: Chord -> RelNote -> Note
relToNote chord n@(RelNote _ oct) = octShift oct $ relToNote' chord n
  where
    relToNote' (Chord root _) (RelNote IRoot  _) = root
    relToNote' (Chord root _) (RelNote IFifth _) = root + 7

type ChordFunc = Chord -> NoteRange -> OctShift -> MidiMsgFunc

instance BeatRep [RelNote] where
  type BeatContents [RelNote] = ChordFunc
  beatOff _ _ _ _ channel _ = noteOffAll channel
  beatOn ns chord rng sft channel vel =
    [ noteOnMsg (shiftToRange rng $ octShift sft n) channel vel
    | n <- map (relToNote chord) ns
    ]

instance Empty ChordFunc where
  emptyElem = const . const . const emptyElem

--------------- Data Types for representing rhythm of a beat ------------------

-- TODO should be able to program parts and fuse them into single animation
data NoteRole a = NNote a | NPause
  deriving ( Eq, Show )

data NoteDuration = DWhole
                  | DHalf
                  | DQuarter
                  | DEighth
                  | DSixteenth
                  | DDotted NoteDuration
  deriving ( Eq, Show )

data BNote a = BNote  NoteDuration (NoteRole a)
             | BTuple NoteDuration [NoteRole a]
  deriving ( Eq, Show )

newtype Sequence a = Sequence [BNote a]

nNote  = NNote
nPause = NNote

mkNote :: a -> NoteDuration -> BNote a
mkNote a d = BNote d $ NNote a

mkPause :: NoteDuration -> BNote a
mkPause d = BNote d NPause

mkTuple :: [NoteRole a] -> NoteDuration -> BNote a
mkTuple ns d = BTuple d ns

------------------- Compiling a beat sequence to an animation -----------------

-- This assumes every subdivision can be converted to a whole number of
-- ticks, maybe change to a rational number and round when compiling to animation?
durationToTicks :: NoteDuration -> Tick
durationToTicks DWhole      = ticksPerMeasure
durationToTicks DHalf       = ticksPerMeasure `div` 2
durationToTicks DQuarter    = ticksPerMeasure `div` 4
durationToTicks DEighth     = ticksPerMeasure `div` 8
durationToTicks DSixteenth  = ticksPerMeasure `div` 16
durationToTicks (DDotted d) = (3 * durationToTicks d) `div` 2


-- | A helper function for 'compileNote' which turns a 'NoteRole' into an
-- animation with a specified length in ticks
roleToTimes
  :: (BeatRep a, b ~ BeatContents a, Empty b)
  => Tick       -- ^ Number of ticks to fit the note into
  -> NoteRole a -- ^ The note to convert
  -> Animation b
roleToTimes ticks (NNote a) = Animation [(ticks - 3, beatOn a), (3, beatOff a)]
roleToTimes ticks NPause    = pauseAnim ticks

compileNote
  :: (BeatRep a, b ~ BeatContents a, Empty b) => BNote a -> Animation b
compileNote (BNote  d r ) = roleToTimes (durationToTicks d) r
compileNote (BTuple d ns) = mconcat $ zipWith roleToTimes ds ns
 where
    -- If it is not possible to subdivide the duration evenly, this makes
    -- sure the whole duration gets filled by extending the last beat
  ds =
    let (div, rest) = durationToTicks d `divMod` fromIntegral (length ns)
    in  replicate (length ns - 1) div ++ [div + rest]

compileSequence
  :: (BeatRep a, b ~ BeatContents a, Empty b)
  => Sequence a
  -> Animation b
compileSequence (Sequence notes) = mconcat $ map compileNote notes
