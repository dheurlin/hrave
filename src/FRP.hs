{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}

module FRP where

import           Util
import           Midi
import           Notes
import           DataTypes
import           Animations

import           Foreign.C.Types                ( CLong )

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

-- bass :: Animation [MidiMessage]
-- bass = cycleAnimation (Just 8)
--   $ Animation [(0, [noteAOff, noteDOn]), (8, [noteDOff, noteAOn])]
--  where
--   noteDOn =
--     MidiMessage 1 $ NoteOn (pianoNoteToMidi $ PianoNote D NoteNormal 1) 100
--   noteDOff =
--     MidiMessage 1 $ NoteOff (pianoNoteToMidi $ PianoNote D NoteNormal 1) 100

--   noteAOn =
--     MidiMessage 1 $ NoteOn (pianoNoteToMidi $ PianoNote A NoteNormal 2) 100
--   noteAOff =
--     MidiMessage 1 $ NoteOff (pianoNoteToMidi $ PianoNote A NoteNormal 2) 100


data BeatUnit = BeatOn | BeatOff
  deriving ( Show )

beatToMidi :: CLong -> [Note] -> BeatUnit -> [MidiMessage]
beatToMidi channel ns BeatOn =
  [ MidiMessage channel $ NoteOn note 100 | note <- ns ]
beatToMidi channel ns BeatOff =
  [ MidiMessage channel $ NoteOff note 100 | note <- [0 .. 127] ]


beat :: Animation BeatUnit
beat = cycleAnimation Nothing $ Animation
  [ (0, BeatOn) , (2, BeatOff)
  , (2, BeatOn) , (2, BeatOff)
  , (2, BeatOn), (2, BeatOff)
  , (5, BeatOff)
  ]

makeNetworkDescription
  :: AddHandler () -> AddHandler [MidiMessage] -> PM.PMStream -> MomentIO ()
makeNetworkDescription addTickEvent addMidiEvent outputStream = do
  -- Set up counter
  eTick <- fromAddHandler addTickEvent
  -- By having an Integer counter, we should never get an overflow
  eCtr  <- accumE (-1 :: Integer) $ eTick $> (+ 1)

  eMidi <- fromAddHandler addMidiEvent          -- stream of midi events
  held  <- accumB [] $ updateHeldMany <$> eMidi -- currently held notes

  eBeat <- makeFrameEvent eCtr (toAbsAnimation beat)
  let eChordBeat = beatToMidi 0 <$> held <@> eBeat

  reactimate $ writeStream outputStream <$> eChordBeat
  -- reactimate
  --   $ (putStrLn <$> ("Held notes: " <>) . show . mapMaybe midiToPianoNote)
  --   <$> eHeld


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

