{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

module Beats where

import Config
import DataTypes
import Animations
import Chords
import Notes

import           Foreign.C.Types                ( CLong )


class BeatRep b where
  type BeatContents b :: *
  beatOn              :: b -> BeatContents b
  beatOff             :: b -> BeatContents b

instance BeatRep [Note] where
  type BeatContents [Note] = [MidiFunc]
  beatOff                  = map noteOnMsg
  beatOn                   = map noteOffMsg


-- | A datatype reprenting a unit of a beat with no pitch information
data BeatUnit = BeatOn | BeatOff | BeatEmpty
  deriving ( Show )

instance Empty BeatUnit where
  emptyElem = BeatEmpty

instance BeatRep () where
  type BeatContents () = BeatUnit
  beatOff              = const BeatOff
  beatOn               = const BeatOn


-- TODO should also take velocity as argument
beatToMidi :: MidiChannel -> [Note] -> BeatUnit -> [MidiMessage]
beatToMidi channel ns BeatOn =
  [ MidiMessage channel $ NoteOn note 100 | note <- ns ]
beatToMidi channel ns BeatOff = noteOffAll channel
  -- [ MidiMessage channel $ NoteOff note 100 | note <- [0 .. 127] ]
beatToMidi channel ns BeatEmpty = []


noteOffAll :: MidiChannel -> [ MidiMessage ]
noteOffAll c = [ MidiMessage c $ NoteOff note 100 | note <- [0 .. 127] ]

data ChordRole = IRoot
               | IFifth
  deriving ( Eq, Show )

data RelNote = RelNote { rRole     :: ChordRole
                       , rOctShift :: Int
                       }

relToNote :: Chord -> RelNote -> Note
relToNote chord n@(RelNote _ oct) = octShift (relToNote' chord n) oct
  where
    relToNote' (Chord root _) (RelNote IRoot  _) = root
    relToNote' (Chord root _) (RelNote IFifth _) = root + 7

instance BeatRep [RelNote] where
  type BeatContents [RelNote] = Chord -> MidiChannel -> [MidiMessage]
  beatOff ns chord = noteOffAll
  beatOn ns chord channel = -- TODO add velocity as argument
    [ noteOnMsg n channel 100 | n <- map (relToNote chord) ns ]

instance Empty (Chord -> MidiChannel -> [MidiMessage]) where
  emptyElem = const . const []

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


beat :: Animation BeatUnit
beat = cycleAnimation $ Animation
  [ (3, BeatOn) , (1, BeatOff)
  , (3, BeatOn) , (1, BeatOff)
  , (3, BeatOn) , (1, BeatOff)
  , pauseFrame 4
  ]


printAnim :: Show a => Int -> Animation a -> IO ()
printAnim n (Animation as) = mapM_ print $ take n as

sumDurations :: Animation a -> Integer
sumDurations (Animation as) = sum . map fst $ as


-- 42 F#1 Closed Hi Hat

tickSeq :: Sequence [Note]
tickSeq = Sequence [ mkNote [42] DQuarter ]

beatAnim :: Animation [MidiFunc]
beatAnim = cycleAnimation $ compileSequence tickSeq
