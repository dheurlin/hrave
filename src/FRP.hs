{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}

module FRP where

import           Config
import           Util
import           Midi
import           MidiIO
import           DataTypes
import           Animations
import           Chords
import           Notes

import qualified BeatLib.Samba as Samba

import           Control.Monad
import           Data.Functor                   ( ($>)
                                                , (<&>)
                                                )

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


-- TODO let these be changed dynamically
bpm           = 115

chordChannel  = 0
chordVelocity = 100
chordOctShift = 1
chordRange    = fullRange

bassChannel   = 1
bassVelocity  = 100
bassOctShift  = 0
bassRange     = fullRange

drumChannel   = 9
drumVelocity  = 100

melodyChannel  = 2
melodyOctShift = 0

splitPoint = 60

mkNetworkDescription
  :: AddHandler () -> AddHandler [MidiMessage] -> PM.PMStream -> MomentIO ()
mkNetworkDescription addTickEvent addMidiEvent outputStream = do

  eTick <- fromAddHandler addTickEvent
  eCtr  <- accumE (-1 :: Tick) $ eTick $> (+ 1) -- Should never overflow :)

  eMidi <- fromAddHandler addMidiEvent -- stream of midi events
  let (eLower, eUpper) = splitTup (splitMidi splitPoint <$> eMidi)

  eHeldAndChord <- accumE ([], Nothing) $ updateHeldChordMany <$> eLower

  held   <- stepper [] $ fst <$> eHeldAndChord -- currently held notes
  chord  <- stepper (Chord 49 MajorChord) $ filterJust $ snd <$> eHeldAndChord

  eChordAnim <- makeFrameEvent eCtr (toAbsAnimation Samba.chords)
  eBassAnim  <- makeFrameEvent eCtr (toAbsAnimation Samba.bass)
  eDrumAnim  <- makeFrameEvent eCtr (toAbsAnimation Samba.bongos)

  let eDrums = eDrumAnim           <&> ($ drumChannel)  <&> ($ drumVelocity)

      eChord = eChordAnim <~> held <&> ($ chordRange)
                                   <&> ($ chordOctShift)
                                   <&> ($ chordChannel)
                                   <&> ($ chordVelocity)

      eBass  = eBassAnim  <~> chord <&> ($ bassRange)
                                    <&> ($ bassOctShift)
                                    <&> ($ bassChannel)
                                    <&> ($ bassVelocity)

      eMel   = map (changeChannel melodyChannel) <$> eUpper

  eHeldChord <- changes chord
  eHeld      <- changes held

  reactimate' $ fmap print <$> eHeldChord
  reactimate' $ fmap print <$> eHeld

  let eOut = foldl1 (unionWith (<>)) [eChord, eBass, eDrums, eMel]
  reactimate $ writeStream outputStream <$> eOut


-- Pick input and output devices and start network
frpMain :: IO ()
frpMain = withPM $ uncurry startNetwork =<< pickDevices

-- Starts the network with my input and output devices
frpMain' :: IO ()
frpMain' = withPM $ do
  input  <- getInputDevice
  output <- getOutputDevice
  startNetwork input output

startNetwork :: PM.DeviceID -> PM.DeviceID -> IO ()
startNetwork input output = do
  inputStr                 <- openInputStream input
  outputStr                <- openOutputStream output
  (addMidiEvent, fireMidi) <- newAddHandler
  (addTickEvent, tid     ) <- makeTick

  network <- compile (mkNetworkDescription addTickEvent addMidiEvent outputStr)
  actuate network

  -- Close stream,  kill tick thread, and mute all notes on interrupt
  SIG.installHandler SIG.sigINT (SIG.Catch $ do
      pause network
      threadDelay 100_000
      writeStream outputStr [MidiMessage c AllNotesOff | c <- [0..15]]
      threadDelay 1000_000
      PM.close inputStr
      PM.close outputStr
      killThread tid
      putStrLn "Exited"
    ) Nothing

  -- Poll MIDI input and activate MIDI event on change
  forever $ do
    readStream inputStr >>= \case
      [] -> pure ()
      xs -> fireMidi xs
    threadDelay 1_000


makeTick :: IO (AddHandler (), ThreadId)
makeTick = do
  (addTickEvent, tick) <- newAddHandler

  tid <- forkIO . forever $ do
    threadDelay (tickPeriod bpm)
    tick ()

  pure (addTickEvent, tid)

-- makeTickEvent
--   :: Int -- Initial delay
--   -> Event (Int -> Int) -- Change delay
--   -> MomentIO (Event ())
