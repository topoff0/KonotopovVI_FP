module Main (main) where

import Config.Config
import IO.Console
import IO.FileLoader
import Parser.LabyrinthParser (parseLabyrinth)

main :: IO ()
main = do
  file <- chooseLabyrinth labyrinthFiles
  text <- loadFile file
  let temp = parseLabyrinth text
  return ()
