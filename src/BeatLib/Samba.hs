module BeatLib.Samba where

import Beats
import Animations

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
