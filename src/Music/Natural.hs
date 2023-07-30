module Music.Natural where

import qualified Music.Pitch as Pitch

data Natural = C | D | E | F | G | A | B
  deriving (Bounded, Enum, Eq, Show)

fromPitch :: Pitch.Pitch -> Natural
fromPitch pitch = case pitch of
  Pitch.C -> C
  Pitch.D -> D
  Pitch.E -> E
  Pitch.F -> F
  Pitch.G -> G
  Pitch.A -> A
  Pitch.B -> B
