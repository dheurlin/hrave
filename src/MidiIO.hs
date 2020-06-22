{-# LANGUAGE LambdaCase #-}

module MidiIO where

import           Midi

import           Data.Maybe
import qualified Sound.PortMidi                as PM

readStream :: PM.PMStream -> IO [MidiMessage]
readStream stream = do
  res  <- PM.readEvents stream
  case res of
    Right ms -> pure $ mapMaybe (toMessage . PM.decodeMsg . PM.message) ms
    _        -> ioError $ userError "Could not read MIDI stream"

writeStream :: PM.PMStream -> [MidiMessage] -> IO ()
writeStream stream msgs =
  (PM.writeEvents stream =<< toEvents msgs) >>= \case
    Right _ -> pure ()
    Left  e -> ioError $ userError $ "Could not write MIDI stream: " <> show e

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
  outputStream <- PM.openOutput output 3

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

getDevice :: (PM.DeviceInfo -> Bool) -> IO PM.DeviceID
getDevice selector =
  fromIntegral . fst . head . filter (selector . snd) <$> listDevices

getInputDevice :: IO PM.DeviceID
getInputDevice = getDevice $ \d -> take 5 (PM.name d) == "CASIO" && PM.input d

getOutputDevice :: IO PM.DeviceID
-- getOutputDevice = getDevice $ \d -> (PM.name d == "VirMIDI 0-1") && PM.input d
getOutputDevice = getDevice $ \d -> (PM.name d == "Midi Through Port-0") && PM.input d


