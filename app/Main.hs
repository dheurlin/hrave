{-# LANGUAGE BlockArguments #-}

module Main where

import           Midi                           ( readDevice
                                                , getDevice
                                                )
import           Util                           ( withPM )

import           Control.Monad
import           Control.Monad.Trans.Except

import           System.Posix.Signals           ( installHandler
                                                , Handler(..)
                                                , sigINT
                                                )
import qualified Sound.PortMidi                as PM

main :: IO ()
main = withPM $ do
  device <- getDevice
  stream <- PM.openInput device

  s <- case stream of
    Left  e -> ioError $ userError $ "Could not open midi stream: " <> show e
    Right s -> do
      -- Close the stream if interrupted
      installHandler sigINT (Catch $ void $ PM.close s) Nothing
      pure s

  void $ runExceptT $ readDevice s


