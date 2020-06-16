module Notes where

import DataTypes

import Data.List
import Data.Maybe
import Data.Char
import Text.Read

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

