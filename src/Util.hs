module Util where

import           System.IO.Error

import           Control.Monad                  ( void )
import           Data.Function                  ( (&) )

import qualified Sound.PortMidi                as PM
import           Reactive.Banana                ( Event
                                                , Behavior
                                                , (<@>)
                                                )


withPM :: IO () -> IO ()
withPM io =
  void $ catchIOError (void $ PM.initialize >> io >> PM.terminate) $ \e -> do
    putStrLn $ "An error occured: " <> show e
    void PM.terminate

-- | Integer division which rounds up
(//) :: Integral a => a -> a -> a
a // b = (a + b - 1) `div` b


(<~>) :: Event (a -> b) -> Behavior a -> Event b
e <~> b = (&) <$> b <@> e

infixl 4 <~>

splitTup :: Event (a, b) -> (Event a, Event b)
splitTup eTup = (fst <$> eTup, snd <$> eTup)
