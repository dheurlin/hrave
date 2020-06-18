module Chords where

import           DataTypes
import           Notes

import           Foreign.C.Types                ( CLong )
import           Data.List
import           Data.Maybe

data ChordQuality = MajorChord
                  | MinorChord
                  | DomSeventh
                  | MajSeventh
                  | MinSeventh
                  | UnknownChord
  deriving ( Show )

data Chord = Chord { rootNote :: Note, chordQuality :: ChordQuality }
  deriving ( Show )


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
