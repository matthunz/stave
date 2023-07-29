module MyLib (someFunc) where

import Data.Bits
import Data.WideWord.Word128 (Word128)
import Data.Word (Word8)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Pitch = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B
  deriving (Show, Enum, Bounded)

data Octave = OctaveNeg1 | Octave0 | Octave1 | Octave2 | Octave3 | Octave4 | Octave5 | Octave6 | Octave7
  deriving (Enum)

instance Show Octave where
  show octave = show . fromIntegral $ fromEnum octave - 1

newtype MidiNote = MidiNote Word8

instance Show MidiNote where
  show note = show (pitch note) ++ show (octave note)

midiNote :: Pitch -> Octave -> MidiNote
midiNote pitch octave =
  MidiNote $
    fromIntegral (fromEnum octave)
      * (fromIntegral (fromEnum (maxBound :: Pitch)) + 1)
      + fromIntegral (fromEnum pitch)

pitch :: MidiNote -> Pitch
pitch (MidiNote note) = toEnum . fromIntegral $ fromIntegral note `mod` (fromEnum (maxBound :: Pitch) + 1)

octave :: MidiNote -> Octave
octave (MidiNote note) = toEnum (fromIntegral (note `div` fromIntegral (fromEnum (maxBound :: Pitch) + 1)))

newtype MidiSet = MidiSet Word128

interval :: MidiNote -> MidiNote -> Word8
interval (MidiNote lhs) (MidiNote rhs) = fromInteger . abs $ toInt lhs - toInt rhs
  where
    toInt x = fromIntegral x :: Integer

instance Show MidiSet where
  show set = show $ notes set

insert :: MidiNote -> MidiSet -> MidiSet
insert (MidiNote note) (MidiSet set) = MidiSet $ set .|. (1 `shiftL` fromIntegral note)

remove :: MidiNote -> MidiSet -> MidiSet
remove (MidiNote note) (MidiSet set) = MidiSet $ set .&. complement (1 `shiftL` fromIntegral note)

notes :: MidiSet -> [MidiNote]
notes (MidiSet w) = map MidiNote (go w 0)
  where
    go 0 _ = []
    go n pos
      | n .&. 1 == 1 = pos : go (n `shiftR` 1) (pos + 1)
      | otherwise = go (n `shiftR` 1) (pos + 1)

data Chord = Chord
  { root :: MidiNote,
    set :: MidiSet
  }
  deriving (Show)

fromMidi :: MidiNote -> MidiSet -> Chord
fromMidi r set = Chord r (remove r set)
