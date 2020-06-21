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


makeNetworkDescription
  :: AddHandler () -> AddHandler [MidiMessage] -> PM.PMStream -> MomentIO ()
makeNetworkDescription addTickEvent addMidiEvent outputStream = do
  eTick <- fromAddHandler addTickEvent
  -- By having an Integer counter, we should never get an overflow
  eCtr  <- accumE (-1 :: Tick) $ eTick $> (+ 1)

  eMidi         <- fromAddHandler addMidiEvent          -- stream of midi events
  eHeldAndChord <- accumE ([], Nothing) $ updateHeldChordMany <$> eMidi
  -- held   <- accumB [] $ updateHeldMany <$> eMidi -- currently held notes
  held   <- stepper [] $ fst <$> eHeldAndChord
  chord  <- stepper (Chord 49 MajorChord) $ filterJust $ snd <$> eHeldAndChord

  eBeat     <- makeFrameEvent eCtr (toAbsAnimation testBeat)
  eTickFun  <- makeFrameEvent eCtr (toAbsAnimation beatAnim)
  eBass     <- makeFrameEvent eCtr (toAbsAnimation testBass)

  let eChordBeat = beatToMidi 0 <$> held <@> eBeat
  let eClick     = beatToMidi 9 [42] <$> eBeat
  let eTickBeat = map (\f -> f 9 100) <$> eTickFun
  let eBassEvents = (\chord fun -> fun chord 1) <$> chord <@> eBass

  eChord <- changes chord
  eHeld <- changes held

  reactimate' $ fmap print <$> eChord
  reactimate' $ fmap print <$> eHeld

  reactimate $ writeStream outputStream <$> eChordBeat
  reactimate $ writeStream outputStream <$> eBassEvents
  -- reactimate $ writeStream outputStream <$> eTickBeat
  -- reactimate $ writeStream outputStream <$> eClick



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
