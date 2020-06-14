{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE NumericUnderscores #-}

module Midi where

import Notes
import DataTypes

import qualified Sound.PortMidi                as PM
import           Control.Event.Handler

import           Data.Maybe
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Except
import           Control.Concurrent             ( threadDelay )
import           Foreign.C.Types                ( CLong )
import           System.IO.Error

import           Data.Bits

readDevice :: PM.PMStream -> PM.PMStream -> ExceptT PM.PMError IO ()
readDevice input output = do

  forever $ do
    msgs <- ExceptT (PM.readEvents input)

    liftIO $ mapM_ printMessage msgs
    liftIO $ PM.writeEvents output $ filter isNoteOn msgs
    -- liftIO . mapM_ printMessage =<< ExceptT (PM.readEvents stream)
    liftIO $ threadDelay 500

  void $ liftIO $ PM.close input

 where
  printMessage = putStr . showEvent . PM.decodeMsg . PM.message

readStream :: PM.PMStream -> IO [MidiMessage]
readStream stream = do
  res <- PM.readEvents stream
  msgs <- case res of
      Right ms -> pure $ mapMaybe (toMessage . PM.decodeMsg . PM.message) ms
      _        -> ioError $ userError "Could not read MIDI stream"
  threadDelay 500
  pure msgs


listDevices :: IO [(Int, PM.DeviceInfo)]
listDevices =
  zip [0 ..] <$> (mapM PM.getDeviceInfo =<< zeroTill <$> PM.countDevices)
  where zeroTill n = [0 .. n - 1]

printDevices :: IO ()
printDevices = mapM_ (print . snd) =<< listDevices

getDevice :: (PM.DeviceInfo -> Bool) -> IO PM.DeviceID
getDevice selector =
  fromIntegral . fst . head . filter (selector . snd) <$> listDevices

-- The input MIDI device is hardcoded to my MIDI controller atm
getInputDevice :: IO PM.DeviceID
getInputDevice = getDevice $ \d -> take 5 (PM.name d) == "CASIO" && PM.input d

getOutputDevice :: IO PM.DeviceID
getOutputDevice = do
  d <- getDevice $ \d -> (PM.name d == "VirMIDI 0-1") && PM.input d
  putStrLn $  "output device: " <> show d
  pure d



openInputStream :: IO PM.PMStream
openInputStream = do
  input       <- getInputDevice
  inputStream <- PM.openInput input

  case inputStream of
    (Right is) -> pure is
    _          -> ioError $ userError "Could not open midi stream:"


isNoteOn :: PM.PMEvent -> Bool
isNoteOn (PM.PMEvent msg _) = status .&. 0xF0 == 0b1001_0000
  where (PM.PMMsg status _ _) = PM.decodeMsg msg



toMessage :: PM.PMMsg -> Maybe MidiMessage
toMessage (PM.PMMsg status data1 data2)
  | status .&. 0xF0 == 0b1000_0000
  = Just $ MidiMessage (status .&. 0xF) (NoteOff data1 data2)
  | status .&. 0xF0 == 0b1001_0000
  = Just $ MidiMessage (status .&. 0xF) (NoteOn data1 data2)
  | otherwise
  = Nothing

updateHeld :: MidiMessage -> [Note] -> [Note]
updateHeld (MidiMessage _ (NoteOn n _))  held = n : held
updateHeld (MidiMessage _ (NoteOff n _)) held = filter (/= n) held
updateHeld _ held = held

updateHeldMany :: [MidiMessage] -> [Note] -> [Note]
updateHeldMany = flip $ foldl (flip updateHeld)

showEvent :: PM.PMMsg -> String
showEvent (PM.PMMsg status data1 data2)
  | status .&. 0xF0 == 0b1000_0000
    = "Noteoff: " <> maybe "" show (midiToPianoNote $ fromIntegral data1) <> "\n"
  | status .&. 0xF0 == 0b1001_0000
    = "Noteon: " <> maybe "" show (midiToPianoNote $ fromIntegral data1) <> "\n"
  | otherwise = ""
