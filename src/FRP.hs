{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}

module FRP where

import           Util
import           Midi
import           Notes
import           DataTypes
import           Animations
import           Beats

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


makeNetworkDescription
  :: AddHandler () -> AddHandler [MidiMessage] -> PM.PMStream -> MomentIO ()
makeNetworkDescription addTickEvent addMidiEvent outputStream = do
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
  putStrLn "Exited"


makeTick :: IO (AddHandler (), Handler () ,ThreadId)
makeTick = do
  (addTickEvent, tick) <- newAddHandler

  tid <- forkIO . forever $ do
    threadDelay 100_000
    tick ()

  pure (addTickEvent, tick, tid)

-- makeTickEvent
--   :: Int -- Initial delay
--   -> Event (Int -> Int) -- Change delay
--   -> MomentIO (Event ())
