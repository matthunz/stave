module MyLib (someFunc) where

import Data.Bits
import Data.WideWord.Word128 (Word128)
import Data.Word (Word8)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype MidiNote = MidiNote Word8

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
