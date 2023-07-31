module Music.Note where

import Music.Natural (Natural)
import qualified Music.Natural as Natural
import Music.Pitch (Pitch)
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



fromSharp :: Pitch -> Note
fromSharp pitch = case pitch of
  Pitch.C -> Note Natural.C Natural
  Pitch.CSharp -> Note Natural.C Sharp
  Pitch.D -> Note Natural.D Natural
  Pitch.DSharp -> Note Natural.D Sharp
  Pitch.E -> Note Natural.E Natural
  Pitch.F -> Note Natural.F Natural
  Pitch.FSharp -> Note Natural.F Sharp
  Pitch.G -> Note Natural.G Natural
  Pitch.GSharp -> Note Natural.G Sharp
  Pitch.A -> Note Natural.A Natural
  Pitch.ASharp -> Note Natural.A Sharp
  Pitch.B -> Note Natural.B Natural

fromFlat :: Pitch -> Note
fromFlat pitch = case pitch of
  Pitch.C -> Note Natural.C Natural
  Pitch.CSharp -> Note Natural.D Flat
  Pitch.D -> Note Natural.D Natural
  Pitch.DSharp -> Note Natural.E Flat
  Pitch.E -> Note Natural.E Natural
  Pitch.F -> Note Natural.F Natural
  Pitch.FSharp -> Note Natural.G Flat
  Pitch.G -> Note Natural.G Natural
  Pitch.GSharp -> Note Natural.A Flat
  Pitch.A -> Note Natural.A Natural
  Pitch.ASharp -> Note Natural.B Flat
  Pitch.B -> Note Natural.B Natural
