module Music.Note where

import Music.Natural (Natural)
import qualified Music.Pitch as Pitch

data Accidental = Natural | Flat | DoubleFlat | Sharp | DoubleSharp
  deriving (Eq)

instance Show Accidental where
  show a = case a of
    Natural -> ""
    Flat -> "b"
    DoubleFlat -> "bb"
    Sharp -> "#"
    DoubleSharp -> "##"

data Note = Note Natural Accidental deriving (Eq)

instance Show Note where
  show (Note natural accidental) = show natural ++ show accidental
