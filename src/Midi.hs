{-# LANGUAGE BinaryLiterals     #-}
{-# LANGUAGE NumericUnderscores #-}

module Midi where

import           DataTypes

import           Data.Bits
import           Data.List

import qualified Sound.PortMidi                as PM

data MsgType = NoteOn  Note Velocity
             | NoteOff Note Velocity
             | AllNotesOff
  deriving (Show)

data MidiMessage = MidiMessage { msgChannel :: MidiChannel
                               , msgType    :: MsgType }
  deriving (Show)

noteOnMsg :: Note -> MidiChannel -> Velocity -> MidiMessage
noteOnMsg n c v = MidiMessage c $ NoteOn n v

noteOffMsg :: Note -> MidiChannel -> Velocity -> MidiMessage
noteOffMsg n c _ = MidiMessage c $ NoteOff n 0

noteOffAll :: MidiChannel -> [ MidiMessage ]
noteOffAll c = [ MidiMessage c AllNotesOff ]
-- noteOffAll c = [ MidiMessage c $ NoteOff note 100 | note <- [0 .. 127] ]

isNoteOn :: PM.PMEvent -> Bool
isNoteOn (PM.PMEvent msg _) = status .&. 0xF0 == 0b1001_0000
  where (PM.PMMsg status _ _) = PM.decodeMsg msg

-- | Splits messages into those that are below and above the given threshold
splitMidi :: Note -> [MidiMessage] -> ([MidiMessage], [MidiMessage])
splitMidi t = partition isBelow
  where
    isBelow (MidiMessage _ (NoteOn  n _)) = n < t
    isBelow (MidiMessage _ (NoteOff n _)) = n < t
    isBelow _ = True
    -- TODO Where to put other message types?

changeChannel :: MidiChannel -> MidiMessage -> MidiMessage
changeChannel newChan (MidiMessage c m) = MidiMessage newChan m

fromMessage :: MidiMessage -> PM.PMMsg
fromMessage (MidiMessage channel (NoteOff note velocity))
  = PM.PMMsg (0b1000_0000 .|. channel) note velocity
fromMessage (MidiMessage channel (NoteOn note velocity))
  = PM.PMMsg (0b1001_0000 .|. channel) note velocity
fromMessage (MidiMessage channel AllNotesOff)
  = PM.PMMsg (0b1011_0000 .|. channel) 123 0

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
-- updateHeld _ held = held

updateHeldMany :: [MidiMessage] -> [Note] -> [Note]
updateHeldMany = flip $ foldl (flip updateHeld)

-- showEvent :: PM.PMMsg -> String
-- showEvent (PM.PMMsg status data1 data2)
--   | status .&. 0xF0 == 0b1000_0000
--     = "Noteoff: " <> maybe "" show (midiToPianoNote $ fromIntegral data1) <> "\n"
--   | status .&. 0xF0 == 0b1001_0000
--     = "Noteon: " <> maybe "" show (midiToPianoNote $ fromIntegral data1) <> "\n"
--   | otherwise = ""
