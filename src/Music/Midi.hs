module Music.Midi
  ( Pitch (..),
    Octave (..),
    MidiNote,
    midiNote,
    pitch,
    octave,
    interval,
    MidiSet,
    contains,
    insert,
    remove,
    notes,
  )
where

import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import Data.WideWord (Word128)
import Data.Word (Word8)

data Pitch = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B
  deriving (Show, Enum, Bounded)

data Octave = OctaveNeg1 | Octave0 | Octave1 | Octave2 | Octave3 | Octave4 | Octave5 | Octave6 | Octave7
  deriving (Enum)

instance Show Octave where
  show octave = show . fromIntegral $ fromEnum octave - 1

-- | Midi note from 0 to 127
newtype MidiNote = MidiNote Word8

instance Show MidiNote where
  show note = show (pitch note) ++ show (octave note)

-- | Create a midi note from a pitch and octave.
midiNote :: Pitch -> Octave -> MidiNote
midiNote pitch octave =
  MidiNote $
    fromIntegral (fromEnum octave)
      * (fromIntegral (fromEnum (maxBound :: Pitch)) + 1)
      + fromIntegral (fromEnum pitch)

-- | Calculate the pitch of the given midi note.
pitch :: MidiNote -> Pitch
pitch (MidiNote note) =
  toEnum . fromIntegral $
    fromIntegral note `mod` (fromEnum (maxBound :: Pitch) + 1)

-- | Calculate the octave of the given midi note.
octave :: MidiNote -> Octave
octave (MidiNote note) =
  toEnum $
    fromIntegral
      (note `div` fromIntegral (fromEnum (maxBound :: Pitch) + 1))

-- | Bitset of midi notes.
newtype MidiSet = MidiSet Word128

-- | Calculate the interval between two midi notes.
interval :: MidiNote -> MidiNote -> Word8
interval (MidiNote lhs) (MidiNote rhs) = fromInteger . abs $ toInt lhs - toInt rhs
  where
    toInt x = fromIntegral x :: Integer

instance Show MidiSet where
  show set = show $ notes set

-- | Insert a midi note into the set.
insert :: MidiNote -> MidiSet -> MidiSet
insert (MidiNote note) (MidiSet set) =
  MidiSet $ set .|. (1 `shiftL` fromIntegral note)

-- | Remove a midi note from the set.
remove :: MidiNote -> MidiSet -> MidiSet
remove (MidiNote note) (MidiSet set) =
  MidiSet $ set .&. complement (1 `shiftL` fromIntegral note)

-- | Returns `True` if the set contains the given midi note.
contains :: MidiSet -> MidiNote -> Bool
contains (MidiSet set) (MidiNote note) = set .&. (1 `shiftR` fromIntegral note) == 1

-- | List the midi notes in the set.
notes :: MidiSet -> [MidiNote]
notes (MidiSet w) = map MidiNote (go w 0)
  where
    go 0 _ = []
    go n pos
      | n .&. 1 == 1 = pos : go (n `shiftR` 1) (pos + 1)
      | otherwise = go (n `shiftR` 1) (pos + 1)
