{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}

module FRP where

import           Config
import           Util
import           Midi
import           Notes
import           DataTypes
import           Animations
import           Beats
import           Chords

import qualified BeatLib.Samba as Samba

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


-- TODO let this be changed dynamically
bpm = 100
chordChannel = 0
bassChannel  = 1
drumChannel  = 9


makeNetworkDescription
  :: AddHandler () -> AddHandler [MidiMessage] -> PM.PMStream -> MomentIO ()
makeNetworkDescription addTickEvent addMidiEvent outputStream = do
  eTick <- fromAddHandler addTickEvent
  -- By having an Integer counter, we should never get an overflow
  eCtr  <- accumE (-1 :: Tick) $ eTick $> (+ 1)

  -- held   <- accumB [] $ updateHeldMany <$> eMidi
  eMidi         <- fromAddHandler addMidiEvent          -- stream of midi events
  eHeldAndChord <- accumE ([], Nothing) $ updateHeldChordMany <$> eMidi

  held   <- stepper [] $ fst <$> eHeldAndChord -- currently held notes
  chord  <- stepper (Chord 49 MajorChord) $ filterJust $ snd <$> eHeldAndChord

  eChordAnim <- makeFrameEvent eCtr (toAbsAnimation Samba.chords)
  eBassAnim  <- makeFrameEvent eCtr (toAbsAnimation Samba.bass)
  eDrumAnim  <- makeFrameEvent eCtr (toAbsAnimation Samba.bongos)

  let eChord = beatToMidi chordChannel <$> held                <@> eChordAnim
  let eBass  = (\chord fun -> fun chord bassChannel) <$> chord <@> eBassAnim
  let eDrums = map (\f -> f drumChannel 100)                   <$> eDrumAnim

  eHeldChord <- changes chord
  eHeld      <- changes held

  reactimate' $ fmap print <$> eHeldChord
  reactimate' $ fmap print <$> eHeld

  reactimate $ writeStream outputStream <$> eChord
  reactimate $ writeStream outputStream <$> eBass
  reactimate $ writeStream outputStream <$> eDrums



  -- reactimate
  --   $ (putStrLn <$> ("Held notes: " <>) . show . mapMaybe midiToPianoNote)
  --   <$> eHeld


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

  forever
    (  (readStream inputStream >>= \case
         [] -> pure ()
         xs -> fireMidi xs
       )
    >> threadDelay 500
    )
  putStrLn "Exited"


makeTick :: IO (AddHandler (), Handler () ,ThreadId)
makeTick = do
  (addTickEvent, tick) <- newAddHandler

  tid <- forkIO . forever $ do
    threadDelay (tickPeriod bpm)
    -- threadDelay 100_000
    tick ()

  pure (addTickEvent, tick, tid)

-- makeTickEvent
--   :: Int -- Initial delay
--   -> Event (Int -> Int) -- Change delay
--   -> MomentIO (Event ())
