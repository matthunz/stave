module MyLib (someFunc) where

import Data.Bits
import Data.WideWord.Word128 (Word128)
import Data.Word (Word8)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Pitch = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B
  deriving (Show, Enum, Bounded)

data Octave = OctaveNeg1 | Octave0 | Octave1 | Octave2 | Octave3 | Octave4 | Octave5 | Octave6 | Octave7
  deriving (Enum, Show)

newtype MidiNote = MidiNote Word8

midiNote :: Pitch -> Octave -> MidiNote
midiNote pitch octave =
  MidiNote $
    (fromIntegral (fromEnum octave) + 1)
      * (fromIntegral (fromEnum (maxBound :: Pitch)) + 1)
      + fromIntegral (fromEnum pitch)

pitch :: MidiNote -> Pitch
pitch (MidiNote note) = toEnum . fromIntegral $ fromIntegral note `mod` (fromEnum (maxBound :: Pitch) + 1)

newtype MidiSet = MidiSet Word128

insert :: MidiNote -> MidiSet -> Word128
insert (MidiNote note) (MidiSet set) = set .|. (1 `shiftL` fromIntegral note)

notes :: MidiSet -> [MidiNote]
notes (MidiSet w) = map MidiNote (go w 0)
  where
    go 0 _ = []
    go n pos
      | n .&. 1 == 1 = pos : go (n `shiftR` 1) (pos + 1)
      | otherwise = go (n `shiftR` 1) (pos + 1)
