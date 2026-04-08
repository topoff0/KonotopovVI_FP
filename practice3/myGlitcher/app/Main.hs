module Main (main) where

import Core.Glitcher
import IO.File
import System.Environment (getArgs)
import Core.Glitcher (runGlitcher)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args

  img <- readImage fileName
  glitched <- runGlitcher img

  let out = "glitched_" ++ fileName
  writeImage out glitched

  putStrLn "Done"
