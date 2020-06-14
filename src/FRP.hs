{-# LANGUAGE LambdaCase #-}

module FRP where

import           Util
import           Midi
import           Notes
import           DataTypes

import           Control.Monad
import           Data.Maybe

import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                , ThreadId
                                                )

import           Reactive.Banana
import           Control.Event.Handler
import           Reactive.Banana.Frameworks

import qualified Sound.PortMidi                as PM

makeNetworkDescription :: AddHandler () -> AddHandler [MidiMessage] -> MomentIO ()
makeNetworkDescription addTickEvent addMidiEvent = do
  eMidi     <- fromAddHandler addMidiEvent
  eHeld     <- accumE [] $ updateHeldMany <$> eMidi

  reactimate $
    (putStrLn <$> ("Held notes: " <>) . show . mapMaybe midiToPianoNote) <$> eHeld


test :: IO ()
test = withPM $ do
  stream <- openInputStream
  mapM_ print =<< readStream stream
  void $ PM.close stream

setup :: IO ()
setup = withPM $ do
  stream                    <- openInputStream
  (addMidiEvent, fireMidi)  <- newAddHandler
  (addTickEvent, tick, tid) <- makeTick
  network <- compile (makeNetworkDescription addTickEvent addMidiEvent)
  actuate network

  forever
    (  (readStream stream >>= \case
         [] -> pure ()
         xs -> fireMidi xs
       )
    >> threadDelay 500
    )


makeTick :: IO (AddHandler (), Handler () ,ThreadId)
makeTick = pure (undefined, undefined, undefined)

-- makeTick :: IO (AddHandler (), Handler () ,ThreadId)
-- makeTick = do
--   (addTickEvent, tick) <- newAddHandler

--   tid <- forkIO . forever $ do
--     threadDelay 1000000
--     tick ()

--   pure (addTickEvent, tick, tid)
