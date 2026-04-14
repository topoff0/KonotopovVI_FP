module IO.File where

import qualified Data.ByteString as BC

readImage :: FilePath -> IO BC.ByteString
readImage = BC.readFile

writeImage :: FilePath -> BC.ByteString -> IO ()
writeImage = BC.writeFile
