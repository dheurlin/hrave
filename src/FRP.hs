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
  eCtr       <- accumE (-2) $ eTick $> (+ 1)
  eAnimation <- accumE (Right testAnimation) $ stepAnim' <$> eCtr
  let eFrame = snd . head <$> filterE
        (not . null)
        (either id id <$> filterE isRight eAnimation)

  reactimate'
    $ fmap (putStrLn <$> ("Held notes: " <>) . show . mapMaybe midiToPianoNote)
    <$> eHeld

  -- reactimate' $ fmap putStrLn <$> eFrame
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

-- Right means the animation has advanced since the previous timestep,
-- Left means it has not
type AnimationStep a = Either (Animation a) (Animation a)

stepAnim' :: TimeStamp -> AnimationStep a -> AnimationStep a
stepAnim' now a = case either id id a of
  anim@((time, _):xs) -> case time of
                           t | t == now -> Right xs
                             | t == 0   -> Right anim
                           _            -> Left anim
  []                  -> Left []


animStart :: Animation a -> (a, Animation a)
animStart anim = (snd . head $ anim, tail anim)

testAnimation :: Animation String
testAnimation =
  [ (0, "Det var en gång en bajsman")
  , (1, "Han hette bajsmannen")
  , (3, "En gång skulle han bajsa...")
  , (6, "Han gjorde det i NC")
  ]
