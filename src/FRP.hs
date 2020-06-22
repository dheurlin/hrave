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
bpm           = 100

chordChannel  = 0
chordVelocity = 100

bassChannel   = 1
bassVelocity  = 100

drumChannel   = 9
drumVelocity  = 100

splitPoint = 60

makeNetworkDescription
  :: AddHandler () -> AddHandler [MidiMessage] -> PM.PMStream -> MomentIO ()
makeNetworkDescription addTickEvent addMidiEvent outputStream = do

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

  let eChord = eChordAnim <~> held  <&> ($ chordChannel) <&> ($ chordVelocity)
      eBass  = eBassAnim  <~> chord <&> ($ bassChannel)  <&> ($ bassVelocity)
      eDrums = eDrumAnim            <&> ($ drumChannel)  <&> ($ drumVelocity)

  eHeldChord <- changes chord
  eHeld      <- changes held

  reactimate' $ fmap print <$> eHeldChord
  reactimate' $ fmap print <$> eHeld

  let eOut = foldl1 (unionWith (<>)) [eChord, eBass, eDrums, eUpper]
  reactimate $ writeStream outputStream <$> eOut

  -- reactimate $ writeStream outputStream <$> eChord
  -- reactimate $ writeStream outputStream <$> eBass
  -- reactimate $ writeStream outputStream <$> eDrums
  -- reactimate $ writeStream outputStream <$> eLower -- Just echo notes



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
  inputStream               <- openInputStream  input
  outputStream              <- openOutputStream output
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

  -- Poll MIDI input and activate MIDI event on change
  forever $ do
    readStream inputStream >>= \case
      [] -> pure ()
      xs -> fireMidi xs
    threadDelay 1_000

  putStrLn "Exited"


makeTick :: IO (AddHandler (), Handler () ,ThreadId)
makeTick = do
  (addTickEvent, tick) <- newAddHandler

  tid <- forkIO . forever $ do
    threadDelay (tickPeriod bpm)
    tick ()

  pure (addTickEvent, tick, tid)

-- makeTickEvent
--   :: Int -- Initial delay
--   -> Event (Int -> Int) -- Change delay
--   -> MomentIO (Event ())
