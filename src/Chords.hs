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
toChord ns = Just $ Chord root quality
 where
  inversions =
    [ (n, toQuality $ sort $ map ((`mod` 12) . subtract n) ns) | n <- ns ]

  firstJust []                 = Nothing
  firstJust ((n, Just q) : xs) = Just (n, q)
  firstJust (x           : xs) = firstJust xs

  (root, quality) = fromMaybe (minimum ns, UnknownChord) (firstJust inversions)

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
  -- elmems needles haystack = all (`elem` haystack) needles

testMajor :: [Note]
testMajor =
  [ pianoNoteToMidi $ pn "D2"
  , pianoNoteToMidi $ pn "A2"
  , pianoNoteToMidi $ pn "F#2"
  ]

-- TODO updateHeldChord still seems problematic, as it often changes a major
-- chord to a minor chord or vice versa when keys are released.
--
-- Possible solutions:
-- 1. Add a delay for noteOff events so that events that happen in quick
--    succession will be registered at same time
-- 2. Don't allow change of chord quality without first lifting all notes
--
-- I think I prefer 2. since it's easier to implement and might not affect
-- convenience that much

-- | Updates the record of which notes are held and what chord was last played
updateHeldChord :: MidiMessage -> ([Note], Maybe Chord) -> ([Note], Maybe Chord)
updateHeldChord msgs (heldOld, chordOld) = (heldNew, chordNew)
  where
    heldNew = updateHeld msgs heldOld
    chordNew
      | length heldNew >= length heldOld = toChord heldNew
      | Just c <- toChord heldNew,
        chordQuality c /= UnknownChord   = Just c
      | otherwise                        = chordOld

updateHeldChordMany
  :: [MidiMessage] -> ([Note], Maybe Chord) -> ([Note], Maybe Chord)
updateHeldChordMany = flip $ foldl (flip updateHeldChord)

-- updateHeld :: MidiMessage -> [Note] -> [Note]
-- updateHeld (MidiMessage _ (NoteOn n _))  held = n : held
-- updateHeld (MidiMessage _ (NoteOff n _)) held = filter (/= n) held
-- updateHeld _ held = held

-- updateHeldMany :: [MidiMessage] -> [Note] -> [Note]
-- updateHeldMany = flip $ foldl (flip updateHeld)
