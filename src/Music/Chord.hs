module Music.Chord (Chord (..), fromMidi) where

import Data.Word (Word8)
import Music.Midi (MidiNote, MidiSet, Octave (OctaveNeg1), Pitch, fromPitches, remove, unMidiNote)

data Chord = Chord
  { root :: MidiNote,
    set :: MidiSet
  }
  deriving (Show)

fromMidi :: MidiNote -> MidiSet -> Chord
fromMidi r set = Chord r (remove r set)

-- | Calculate the intervals between a root note and a list of pitches.
intervals :: MidiNote -> [Pitch] -> [Word8]
intervals root pitches = map go (fromPitches pitches OctaveNeg1)
  where
    r = unMidiNote root
    go note = fromIntegral $ r - unMidiNote note
