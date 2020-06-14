{-# LANGUAGE LambdaCase #-}

module FRP where

import           Util
import           Midi
import           Notes
import           DataTypes

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
  :: AddHandler () -> AddHandler [MidiMessage] -> MomentIO ()
makeNetworkDescription addTickEvent addMidiEvent = do
  eMidi      <- fromAddHandler addMidiEvent
  held       <- accumB [] $ updateHeldMany <$> eMidi
  eHeld      <- changes held
  eTick      <- fromAddHandler addTickEvent
  -- By having an Integer counter, we should never get an overflow
  eCtr       <- accumE (-1 :: Integer) $ eTick $> (+ 1)
  eFrame     <- makeFrameEvent eCtr testAnimation

  reactimate'
    $ fmap (putStrLn <$> ("Held notes: " <>) . show . mapMaybe midiToPianoNote)
    <$> eHeld


  -- reactimate' $ fmap putStrLn <$> eFrame
  reactimate $ print <$> eCtr
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

type TimeStamp   = Integer
type Frame a     = (TimeStamp, a)
type Animation a = [Frame a]


-- | Advances the animation by one step if the correct point in time has been
-- reached, and returns (currentFrame, rest). If the correct point in time
-- has not been reached, returns (Nothing, rest)
stepAnim :: TimeStamp -> (Maybe a, Animation a) -> (Maybe a, Animation a)
stepAnim now (_, [])                  = (Nothing, [])
stepAnim now (_, anim@((time, a):xs))
  | now == time = (Just a, xs)
  | otherwise   = (Nothing, anim)

makeFrameEvent :: Event TimeStamp -> Animation a -> MomentIO (Event a)
makeFrameEvent eCounter anim = do
  eAnimation <- accumE (Nothing, anim) $ stepAnim <$> eCounter
  pure $ fromJust . fst <$> filterE (isJust . fst) eAnimation


testAnimation :: Animation String
testAnimation =
  [ (0, "Det var en gång en bajsman")
  , (1, "Han hette bajsmannen")
  , (3, "En gång skulle han bajsa...")
  , (6, "Han gjorde det i NC")
  ]
