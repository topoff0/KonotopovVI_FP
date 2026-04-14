module Main (main) where

import Core.Glitcher
import qualified Data.ByteString.Char8 as BC
import IO.File
import System.Environment (getArgs)
import System.FilePath (dropExtension, takeFileName)

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args

  img <- readImage fileName

  applyAndSave 3 fileName img

  putStrLn "Done"

applyAndSave :: Int -> String -> BC.ByteString -> IO ()
applyAndSave 0 _ _ = return ()
applyAndSave n fileName img = do
  glitched <- runGlitcher img
  let outName = "glitched_img/glitched_" ++ show (4 - n) ++ "_" ++ (dropExtension (takeFileName fileName)) ++ ".jpg"
  writeImage outName glitched
  applyAndSave (n - 1) fileName img
