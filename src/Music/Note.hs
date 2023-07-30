module Music.Note where

data Natural = C | D | E | F | G | A | B
  deriving (Bounded, Enum, Eq, Show)

data Accidental = Natural | Flat | DoubleFlat | Sharp | DoubleSharp
  deriving (Eq)

instance Show Accidental where
  show a = case a of
    Natural -> ""
    Flat -> "b"
    DoubleFlat -> "bb"
    Sharp -> "#"
    DoubleSharp -> "##"

data Note = Note Natural Accidental deriving (Eq, Show)
