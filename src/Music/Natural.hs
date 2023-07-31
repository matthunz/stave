module Music.Natural where

import Data.Char (isLetter, isSpace, toUpper)
import qualified Music.Pitch as Pitch

data Natural = C | D | E | F | G | A | B
  deriving (Bounded, Enum, Eq, Show)

instance Read Natural where
  readsPrec _ (c : rest) =
    let n = case toUpper c of
          'C' -> C
          'D' -> D
          'E' -> E
          'F' -> F
          'G' -> G
          'A' -> A
          'B' -> B
     in [(n, rest)]

fromPitch :: Pitch.Pitch -> Natural
fromPitch pitch = case pitch of
  Pitch.C -> C
  Pitch.D -> D
  Pitch.E -> E
  Pitch.F -> F
  Pitch.G -> G
  Pitch.A -> A
  Pitch.B -> B
