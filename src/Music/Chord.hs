module Music.Chord ( Chord (..), fromMidi ) where

import Music.Midi ( MidiSet, MidiNote, remove )

data Chord = Chord
  { root :: MidiNote,
    set :: MidiSet
  }
  deriving (Show)

fromMidi :: MidiNote -> MidiSet -> Chord
fromMidi r set = Chord r (remove r set)
