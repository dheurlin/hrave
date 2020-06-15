{-# LANGUAGE LambdaCase #-}

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


makeNetworkDescription
  :: AddHandler () -> AddHandler [MidiMessage] -> MomentIO ()
makeNetworkDescription addTickEvent addMidiEvent = do
  eMidi      <- fromAddHandler addMidiEvent
  held       <- accumB [] $ updateHeldMany <$> eMidi
  eHeld      <- changes held
  eTick      <- fromAddHandler addTickEvent
  -- By having an Integer counter, we should never get an overflow
  eCtr       <- accumE (-1 :: Integer) $ eTick $> (+ 1)
  -- eFrame     <- makeFrameEvent eCtr testAnimation
  eFrame     <- makeFrameEvent eCtr (toAbsAnimation ta)

  reactimate'
    $ fmap (putStrLn <$> ("Held notes: " <>) . show . mapMaybe midiToPianoNote)
    <$> eHeld

  -- reactimate $ print <$> eCtr
  reactimate $ putStrLn <$> eFrame


setup :: IO ()
setup = withPM $ do
  stream                    <- openInputStream
  (addMidiEvent, fireMidi)  <- newAddHandler
  (addTickEvent, tick, tid) <- makeTick
  network <- compile (makeNetworkDescription addTickEvent addMidiEvent)
  actuate network

  -- Close stream and kill tick thread on interrupt
  SIG.installHandler SIG.sigINT
                     (SIG.Catch $ PM.close stream >> killThread tid)
                     Nothing

  forever
    (  (readStream stream >>= \case
         [] -> pure ()
         xs -> fireMidi xs
       )
    >> threadDelay 500
    )


makeTick :: IO (AddHandler (), Handler () ,ThreadId)
makeTick = do
  (addTickEvent, tick) <- newAddHandler

  tid <- forkIO . forever $ do
    threadDelay 1000000
    tick ()

  pure (addTickEvent, tick, tid)

