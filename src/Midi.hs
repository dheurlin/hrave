{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}

module Midi where

import Notes
import DataTypes

import qualified Sound.PortMidi                as PM
import           Control.Event.Handler

import           Data.Maybe
import           Control.Monad
import           Control.Concurrent             ( threadDelay )
import           Foreign.C.Types                ( CLong )
import           System.IO.Error

import           Data.Bits

isNoteOn :: PM.PMEvent -> Bool
isNoteOn (PM.PMEvent msg _) = status .&. 0xF0 == 0b1001_0000
  where (PM.PMMsg status _ _) = PM.decodeMsg msg


fromMessage :: MidiMessage -> PM.PMMsg
fromMessage (MidiMessage channel (NoteOff note velocity))
  = PM.PMMsg (0b1000_0000 .|. channel) note velocity
fromMessage (MidiMessage channel (NoteOn note velocity))
  = PM.PMMsg (0b1001_0000 .|. channel) note velocity

toMessage :: PM.PMMsg -> Maybe MidiMessage
toMessage (PM.PMMsg status data1 data2)
  | status .&. 0xF0 == 0b1000_0000
  = Just $ MidiMessage (status .&. 0xF) (NoteOff data1 data2)
  | status .&. 0xF0 == 0b1001_0000
  = Just $ MidiMessage (status .&. 0xF) (NoteOn data1 data2)
  | otherwise
  = Nothing

toEvent :: MidiMessage -> IO PM.PMEvent
toEvent midiMsg = PM.PMEvent (PM.encodeMsg $ fromMessage midiMsg) <$> PM.time

toEvents :: [MidiMessage] -> IO [PM.PMEvent]
toEvents midiMsgs = do
  time <- PM.time
  pure [PM.PMEvent (PM.encodeMsg $ fromMessage msg) time | msg <- midiMsgs ]

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

--------------- MIDI I/O ------------------------------------------------------

readStream :: PM.PMStream -> IO [MidiMessage]
readStream stream = do
  res  <- PM.readEvents stream
  msgs <- case res of
    Right ms -> pure $ mapMaybe (toMessage . PM.decodeMsg . PM.message) ms
    _        -> ioError $ userError "Could not read MIDI stream"
  threadDelay 500
  pure msgs

writeStream :: PM.PMStream -> [MidiMessage] -> IO ()
writeStream stream msgs =
  (PM.writeEvents stream =<< toEvents msgs) >>= \case
    Right _ -> pure ()
    Left  _ -> ioError $ userError "Could not write MIDI stream"

------ Printing and choosing devices ------------------------------------------

listDevices :: IO [(Int, PM.DeviceInfo)]
listDevices =
  zip [0 ..] <$> (mapM PM.getDeviceInfo =<< zeroTill <$> PM.countDevices)
  where zeroTill n = [0 .. n - 1]

printDevices :: IO ()
printDevices = mapM_ (print . snd) =<< listDevices

openInputStream :: PM.DeviceID -> IO PM.PMStream
openInputStream input = do
  inputStream <- PM.openInput input

  case inputStream of
    (Right is) -> pure is
    _          -> ioError $ userError "Could not open midi input stream:"

openOutputStream :: PM.DeviceID -> IO PM.PMStream
openOutputStream output = do
  outputStream <- PM.openOutput 0 output

  case outputStream of
    (Right os) -> pure os
    _          -> ioError $ userError "Could not open midi ouput stream:"

pickDevices :: IO (PM.DeviceID, PM.DeviceID)
pickDevices = do
  devices <- listDevices
  let inputDevices  = filter (PM.input  . snd) devices
      outputDevices = filter (PM.output . snd) devices
      inputIds      = map fst inputDevices
      outputIds     = map fst outputDevices

  putStrLn ""
  putStrLn "Pick an input device:"
  printDevs inputDevices

  selectedInput <- readLn :: IO PM.DeviceID
  if selectedInput `elem` inputIds
    then pure ()
    else ioError $ userError $ show selectedInput <> "is not a valid device"

  putStrLn ""
  putStrLn "Pick an output device:"
  printDevs outputDevices

  selectedOutput <- readLn :: IO PM.DeviceID
  if selectedOutput `elem` outputIds
    then pure (selectedInput, selectedOutput)
    else ioError $ userError $ show selectedInput <> "is not a valid device"

 where
  printDevs ds = sequence_
    [ putStrLn $ "  " <> show num <> ") " <> PM.name device
    | (num, device) <- ds
    ]



---------- Specialized device picking functions for my setup ------------------
--
getDevice :: (PM.DeviceInfo -> Bool) -> IO PM.DeviceID
getDevice selector =
  fromIntegral . fst . head . filter (selector . snd) <$> listDevices


getInputDevice :: IO PM.DeviceID
getInputDevice = getDevice $ \d -> take 5 (PM.name d) == "CASIO" && PM.input d

getOutputDevice :: IO PM.DeviceID
getOutputDevice = getDevice $ \d -> (PM.name d == "VirMIDI 0-1") && PM.input d
