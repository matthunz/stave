module Main where

import Music.Natural (Natural)
import Music.Note (Note)
import Options.Applicative
import Text.Read (readEither)

main :: IO ()
main = greet =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )

greet :: App -> IO ()
greet (App notes) = putStrLn $ concatMap (\n -> show n ++ " ") notes

newtype App = App
  { notes :: [Natural]
  }

sample :: Parser App
sample =
  App <$> many (argument auto (metavar "NOTES..."))
