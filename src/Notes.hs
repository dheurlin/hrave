module Notes where

import DataTypes
import Util

import Data.List
import Data.Maybe
import Data.Char
import Text.Read

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

pianoNotes :: [PianoNote]
pianoNotes =
  concat
    $ [ map ($ oct) octNotes |  oct <- [0 .. 9] ]
  where octNotes = [ PianoNote A NoteNormal
                   , PianoNote A NoteSharp
                   , PianoNote B NoteNormal
                   , PianoNote C NoteNormal
                   , PianoNote C NoteSharp
                   , PianoNote D NoteNormal
                   , PianoNote D NoteSharp
                   , PianoNote E NoteNormal
                   , PianoNote F NoteNormal
                   , PianoNote F NoteSharp
                   , PianoNote G NoteNormal
                   , PianoNote G NoteSharp
                   ]

midiToPianoNote :: Note -> Maybe PianoNote
midiToPianoNote n
  | n < 21    = Nothing
  | otherwise = Just $ pianoNotes !! fromIntegral (n - 21)

pianoNoteToMidi :: PianoNote -> Note
pianoNoteToMidi note = fromIntegral $ fromJust (elemIndex note pianoNotes) + 21

pn :: String -> PianoNote
pn = pn'
 where
  pn' n@[note, '#', oct]
    | not (okNote note) = e n
    | not (okOct oct)   = e n
    | badMod note       = e n
    | otherwise         = PianoNote (read [note]) NoteSharp (read [oct])
  pn' n@[note, oct]
    | not (okNote note) = e n
    | not (okOct oct)   = e n
    | otherwise         = PianoNote (read [note]) NoteNormal (read [oct])
  pn' n           = e n

  okNote c = isJust (readMaybe [c] :: Maybe NoteName)
  okOct c = isDigit c && i >= 0 && i <= 9 where i = read [c] :: Octave
  badMod 'B' = True
  badMod 'E' = True
  badMod _   = False

  e n = error $ "Invalid note name: " <> n

octShift :: OctShift -> Note -> Note
octShift s n = octShift' where

  octShift' | s > 0, shifted <= 127 = shifted
            | s < 0, shifted >= 0   = shifted
            | otherwise             = n

  shifted = n + (12 * s)

-- | Octave shifts a note so that it is below the target note
octShiftBelow :: Note -> Note -> Note
octShiftBelow = octShiftTill (-1)

-- | Octave shifts a note so that it is above the target note
octShiftAbove :: Note -> Note -> Note
octShiftAbove = octShiftTill 1

octShiftTill :: Int -> Note -> Note -> Note
octShiftTill sgn target n = octShift (fromIntegral sgn * (target - n) // 12) n

-- | Places a note so that it is below the target tone, as close as possible
moveBelow :: Note -> Note -> Note
moveBelow target n
  | n > target = octShiftBelow target n
  | n < target = octShift ((target - n) `div` 12) n
  | otherwise  = n

moveBelowMby :: Maybe Note -> [Note] -> Maybe [Note]
moveBelowMby (Just target) n = Just $ map (moveBelow target) n
moveBelowMby _             _ = Nothing

type NoteRange = (Maybe Note, Maybe Note)

fullRange :: NoteRange
fullRange = (Nothing, Nothing)

-- | Octave shifts a note so that it is within the specified range
shiftToRange :: NoteRange -> Note -> Note
shiftToRange (low, high) n
  | Just lowLim  <- low , n < lowLim  = octShiftAbove lowLim  n
  | Just highLim <- high, n > highLim = octShiftBelow highLim n
  | otherwise                         = n
