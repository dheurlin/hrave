module Notes where

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

pianoNotes :: [PianoNote]
pianoNotes =
  concat . concat
    $ [ [ [PianoNote name NoteNormal oct, PianoNote name NoteSharp oct]
        | name <- [A .. G]
        ]
      | oct <- [0 .. 9]
      ]

midiToPianoNote :: Int -> Maybe PianoNote
midiToPianoNote n
  | n < 21    = Nothing
  | otherwise = Just $ pianoNotes !! (n - 21)
