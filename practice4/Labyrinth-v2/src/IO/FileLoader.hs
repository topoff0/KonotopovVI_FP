module IO.FileLoader (loadFile) where

loadFile :: FilePath -> IO String
loadFile path = readFile path
