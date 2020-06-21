module Util where

import           Control.Monad                  ( void )
import           System.IO.Error
import qualified Sound.PortMidi                as PM


withPM :: IO () -> IO ()
withPM io =
  void $ catchIOError (void $ PM.initialize >> io >> PM.terminate) $ \e -> do
    putStrLn $ "An error occured: " <> show e
    void PM.terminate

-- | Integer division which rounds up
(//) :: Integral a => a -> a -> a
a // b = (a + b - 1) `div` b;
