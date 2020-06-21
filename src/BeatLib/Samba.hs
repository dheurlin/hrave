module BeatLib.Samba where

import qualified BeatLib.DrumMap               as D

import           Beats
import           DataTypes
import           Animations

bassSeq :: Sequence [RelNote]
bassSeq = Sequence
  [ mkNote [RelNote IRoot  (-1)] $ DDotted DEighth
  , mkNote [RelNote IFifth (-1)]   DSixteenth
  , mkNote [RelNote IFifth (-1)] $ DDotted DEighth
  , mkNote [RelNote IRoot  (-1)]   DSixteenth
  ]

bass = cycleAnimation $ compileSequence bassSeq

chordSeq :: Sequence ()
chordSeq = Sequence
  [ mkNote () DSixteenth
  , mkPause   DSixteenth
  , mkNote () DSixteenth
  , mkPause   DEighth
  , mkNote () DSixteenth
  , mkPause   DSixteenth
  , mkNote () DSixteenth
  , mkPause   DSixteenth
  , mkNote () DSixteenth
  , mkPause   DSixteenth
  , mkNote () DSixteenth
  , mkNote () DSixteenth
  , mkPause   DSixteenth
  , mkNote () DSixteenth
  , mkPause   DSixteenth
  ]

chords :: Animation BeatUnit
chords = cycleAnimation $ compileSequence chordSeq

maracasSeq :: Sequence [Note]
maracasSeq = Sequence
  [ mkNote [D.maracas] DSixteenth
  , mkPause            DSixteenth
  , mkNote [D.maracas] DSixteenth
  , mkNote [D.maracas] DSixteenth
  ]

maracas = cycleAnimation $ compileSequence maracasSeq

bongoSeq :: Sequence [Note]
bongoSeq = Sequence
  [ mkNote [D.hiBongo ] $ DDotted DEighth
  , mkNote [D.lowBongo]   DSixteenth
  , mkNote [D.lowBongo] $ DDotted DEighth
  , mkNote [D.hiBongo ]   DSixteenth
  ]

bongos = cycleAnimation $ compileSequence bongoSeq
