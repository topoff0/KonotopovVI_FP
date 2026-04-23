module Game.Command (parseCommand) where

import Core.Types (Command (..))

parseCommand :: String -> Command
parseCommand input =
  case words input of
    ["look"] -> Look
    ["exit"] -> Exit
    ["q"] -> Exit
    ["--help"] -> Help
    ("go" : xs) -> Go (unwords xs)
    _ -> Unknown input
