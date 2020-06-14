module Notes where

import DataTypes

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
