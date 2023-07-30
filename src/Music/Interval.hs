module Music.Interval
  ( Quality (..),
    Interval,
    quality,
    number,
    fromSemitones,
    showFunction
  )
where

import Data.Word (Word8)

data Quality = Perfect | Minor | Major | Diminished deriving (Eq)

instance Show Quality where
  show q = case q of
    Perfect -> "P"
    Minor -> "m"
    Major -> "M"
    Diminished -> "d"

data Interval = Interval Quality Word8

instance Show Interval where
  show (Interval q n) = show q ++ show n

-- | Display the interval as a chord function.
showFunction :: Interval -> String
showFunction (Interval q n) =
  case q of
    Minor -> "b"
    Diminished -> "b"
    _ -> ""
    ++ show n

quality :: Interval -> Quality
quality (Interval q _) = q

number :: Interval -> Word8
number (Interval _ n) = n

-- | Create a new interval from its semitones.
fromSemitones :: Word8 -> Interval
fromSemitones interval = Interval quality (interval `div` 12 * 7 + num)
  where
    (quality, num) = case interval `mod` 12 of
      0 -> (Perfect, 1)
      1 -> (Minor, 2)
      2 -> (Major, 2)
      3 -> (Minor, 3)
      4 -> (Major, 3)
      5 -> (Perfect, 4)
      6 -> (Diminished, 5)
      7 -> (Perfect, 5)
      8 -> (Minor, 6)
      9 -> (Major, 6)
      10 -> (Minor, 7)
      11 -> (Major, 7)
