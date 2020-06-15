{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}

module FRP where

import           Util
import           Midi
import           Notes
import           DataTypes
import           Animations

import           Control.Monad
import           Data.Maybe
import           Data.Either
import           Data.Functor                   ( ($>) )

import qualified System.Posix.Signals as SIG    ( installHandler
                                                , Handler(..)
                                                , sigINT
                                                )

import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                , ThreadId
                                                , killThread
                                                )

import           Reactive.Banana
import           Control.Event.Handler
import           Reactive.Banana.Frameworks

import qualified Sound.PortMidi                as PM

ta :: Animation String
ta = cycleAnimation Nothing $ Animation
  [ (0, "Sagan om bajsmannen, del 2")
  , (1, "Den här gången lite bajsenödigare")
  , (1, "En kort paus nu då...")
  , (4, "..Och så kör vi igen!")
  ]

chords :: Animation [MidiMessage]
chords = cycleAnimation Nothing $ Animation
  [ (0, chordOn) , (2, chordOff)
  , (2, chordOn) , (2, chordOff)
  , (2, chordOn), (2, chordOff)
  , (5, chordOff)
  ]
 where
  noteD  = pianoNoteToMidi $ PianoNote D NoteNormal 2
  noteFs = pianoNoteToMidi $ PianoNote F NoteSharp 2
  noteA  = pianoNoteToMidi $ PianoNote A NoteNormal 3

  mkChord t =
    [ MidiMessage 0 (t noteD 100)
    , MidiMessage 0 (t noteFs 100)
    , MidiMessage 0 (t noteA 100)
    ]

  chordOn  = mkChord NoteOn
  chordOff = mkChord NoteOff

bass :: Animation [MidiMessage]
bass = cycleAnimation (Just 8)
  $ Animation [(0, [noteAOff, noteDOn]), (8, [noteDOff, noteAOn])]
 where
  noteDOn =
    MidiMessage 1 $ NoteOn (pianoNoteToMidi $ PianoNote D NoteNormal 1) 100
  noteDOff =
    MidiMessage 1 $ NoteOff (pianoNoteToMidi $ PianoNote D NoteNormal 1) 100

  noteAOn =
    MidiMessage 1 $ NoteOn (pianoNoteToMidi $ PianoNote A NoteNormal 2) 100
  noteAOff =
    MidiMessage 1 $ NoteOff (pianoNoteToMidi $ PianoNote A NoteNormal 2) 100

makeNetworkDescription
  :: AddHandler () -> AddHandler [MidiMessage] -> PM.PMStream -> MomentIO ()
makeNetworkDescription addTickEvent addMidiEvent outputStream = do
  eMidi      <- fromAddHandler addMidiEvent
  held       <- accumB [] $ updateHeldMany <$> eMidi
  eHeld      <- changes held
  eTick      <- fromAddHandler addTickEvent
  -- By having an Integer counter, we should never get an overflow
  eCtr       <- accumE (-1 :: Integer) $ eTick $> (+ 1)
  -- eFrame     <- makeFrameEvent eCtr testAnimation
  eFrame     <- makeFrameEvent eCtr (toAbsAnimation chords)
  eBass      <- makeFrameEvent eCtr (toAbsAnimation bass)

  reactimate'
    $ fmap (putStrLn <$> ("Held notes: " <>) . show . mapMaybe midiToPianoNote)
    <$> eHeld

  -- reactimate $ print <$> eCtr
  reactimate $ writeStream outputStream <$> eFrame
  reactimate $ writeStream outputStream <$> eBass


setup :: IO ()
setup = withPM $ do
  inputStream               <- openInputStream
  outputStream              <- openOutputStream
  (addMidiEvent, fireMidi)  <- newAddHandler
  (addTickEvent, tick, tid) <- makeTick

  network                   <- compile
    (makeNetworkDescription addTickEvent addMidiEvent outputStream)
  actuate network

  -- Close stream and kill tick thread on interrupt
  SIG.installHandler
    SIG.sigINT
    (SIG.Catch $ PM.close inputStream >> PM.close outputStream >> killThread tid
    )
    Nothing

  forever
    (  (readStream inputStream >>= \case
         [] -> pure ()
         xs -> fireMidi xs
       )
    >> threadDelay 500
    )


makeTick :: IO (AddHandler (), Handler () ,ThreadId)
makeTick = do
  (addTickEvent, tick) <- newAddHandler

  tid <- forkIO . forever $ do
    threadDelay 100_000
    tick ()

  pure (addTickEvent, tick, tid)

