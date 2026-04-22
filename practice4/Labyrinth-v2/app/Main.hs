module Main (main) where

import Config.Config
import IO.Console
import IO.FileLoader

main :: IO ()
main = do
  file <- chooseLabyrinth labyrinthFiles
  content <- loadFile file
  return ()
