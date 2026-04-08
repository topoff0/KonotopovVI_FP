module IO.File where

import qualified Data.ByteString as BC
import qualified Data.ByteString.Char8 as BC

readImage :: FilePath -> IO BC.ByteString
readImage = BC.readFile

writeImage :: FilePath -> BC.ByteString -> IO ()
writeImage = BC.writeFile
