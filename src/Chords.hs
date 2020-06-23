module Chords where

import           DataTypes
import           Notes
import           Midi

import           Foreign.C.Types                ( CLong )
import           Data.List
import           Data.Maybe

data ChordQuality = MajorChord
                  | MinorChord
                  | DomSeventh
                  | MajSeventh
                  | MinSeventh
                  | UnknownChord
                  -- TODO More chord qualities
  deriving ( Eq, Show )

data Chord = Chord { rootNote :: Note, chordQuality :: ChordQuality }
  deriving ( Eq, Show )


type NoteInterval = CLong

toChord :: [Note] -> Maybe Chord
toChord [] = Nothing
toChord ns = Just $ Chord shiftedRoot quality
 where
  inversions =
    [ (n, toQuality $ sort $ map ((`mod` 12) . subtract n) ns) | n <- ns ]

  firstJust []                 = Nothing
  firstJust ((n, Just q) : _ ) = Just (n, q)
  firstJust (_           : xs) = firstJust xs

  (root, quality) = fromMaybe (minimum ns, UnknownChord) (firstJust inversions)

  -- shifts the octave of root so that it is below all other notes
  shiftedRoot = octShiftBelow (minimum ns) root

-- TODO ambiguous chords: (bring back fifth and see if that resolves it?)
-- Min7 vs 6
toQuality :: [NoteInterval] -> Maybe ChordQuality
toQuality = toQuality' . rmFifth . nub
 where
  toQuality' [0, 4]     = Just MajorChord
  toQuality' [0, 3]     = Just MinorChord
  toQuality' [0, 4, 10] = Just DomSeventh
  toQuality' [0, 4, 11] = Just MajSeventh
  toQuality' [0, 3, 10] = Just MinSeventh
  toQuality' _          = Nothing

  rmFifth = filter (/= 7)

testMajor :: [Note]
testMajor =
  [ pianoNoteToMidi $ pn "D2"
  , pianoNoteToMidi $ pn "A2"
  , pianoNoteToMidi $ pn "F#2"
  ]

-- | Updates the record of which notes are held and what chord was last played
updateHeldChord :: MidiMessage -> ([Note], Maybe Chord) -> ([Note], Maybe Chord)
updateHeldChord msgs (heldOld, chordOld) = (heldNew, chordNew)
  where
    heldNew = updateHeld msgs heldOld
    chordNew
      | length heldNew >= length heldOld = toChord heldNew
      | otherwise                        = chordOld

updateHeldChordMany
  :: [MidiMessage] -> ([Note], Maybe Chord) -> ([Note], Maybe Chord)
updateHeldChordMany = flip $ foldl (flip updateHeldChord)
