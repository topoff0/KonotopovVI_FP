module Main where

import Config.Config
import IO.Console

main :: IO ()
main = do
  file <- chooseLabyrinth labyrinthFiles
  return ()
