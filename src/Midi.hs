{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE NumericUnderscores #-}

module Midi where

import Notes

import qualified Sound.PortMidi                as PM

import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Concurrent             ( threadDelay )

import           Data.Bits

readDevice :: PM.PMStream -> ExceptT PM.PMError IO ()
readDevice stream = do

  forever $ do
    liftIO . mapM_ printMessage =<< ExceptT (PM.readEvents stream)
    liftIO $ threadDelay 500

  void $ liftIO $ PM.close stream

 where
  printMessage = putStr . showEvent . PM.decodeMsg . PM.message

-- The input MIDI device is hardcoded to my MID controller atm
getDevice :: IO PM.DeviceID
getDevice = do
  devices <- mapM PM.getDeviceInfo =<< zeroTill <$> PM.countDevices
  pure $ fst . head $ filter (deviceSelector . snd) $ zip [0 ..] devices
 where
  deviceSelector d = take 5 (PM.name d) == "CASIO" && PM.input d
  zeroTill n = [0 .. n - 1]

showEvent :: PM.PMMsg -> String
showEvent (PM.PMMsg status data1 data2)
  | status .&. 0xF0 == 0b1000_0000
    = "Noteoff: " <> maybe "" show (midiToPianoNote $ fromIntegral data1) <> "\n"
  | status .&. 0xF0 == 0b1001_0000
    = "Noteon: " <> maybe "" show (midiToPianoNote $ fromIntegral data1) <> "\n"
  | otherwise = ""
