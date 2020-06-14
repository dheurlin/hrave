{-# LANGUAGE BlockArguments #-}

module Main where

import           Midi                           ( readDevice
                                                , getInputDevice
                                                , getOutputDevice
                                                , printDevices
                                                )
import           Util                           ( withPM )

import           Control.Monad
import           Control.Monad.Trans.Except

import           System.Posix.Signals           ( installHandler
                                                , Handler(..)
                                                , sigINT
                                                )
import qualified Sound.PortMidi                as PM

-- main :: IO ()
-- main = withPM $ do
--   mapM_ (print . snd) =<< listDevices

main :: IO ()
main = withPM $ do
  output <- getOutputDevice
  input  <- getInputDevice

  outputStream <- PM.openOutput 0 output
  inputStream  <- PM.openInput    input
  -- let outputStream = Right undefined

  (is, os) <- case (inputStream, outputStream) of
    (Right is, Right os) -> do
      -- Close the stream if interrupted
      installHandler sigINT (Catch $ void $ PM.close is >> PM.close os) Nothing
      pure(is, os)
    _    -> ioError $ userError "Could not open midi stream:"

  printDevices

  void $ runExceptT $ readDevice is os


