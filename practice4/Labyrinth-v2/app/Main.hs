module Main (main) where

import Config.Config
import Game.Engine (runGame)
import IO.Console (chooseLabyrinth)
import IO.FileLoader (loadFile)
import Parser.LabyrinthParser (parseLabyrinth)

main :: IO ()
main = do
  file <- chooseLabyrinth labyrinthFiles
  text <- loadFile file
  runGame (parseLabyrinth text)
