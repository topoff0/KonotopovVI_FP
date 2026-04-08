module Core.Glitcher where

import Control.Monad (foldM)
import Core.Random (randomReplaceByte, randomSortSection)
import qualified Data.ByteString.Char8 as BC

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions =
  [ randomReplaceByte,
    randomSortSection,
    randomReplaceByte,
    randomSortSection,
    randomReplaceByte
  ]

runGlitcher :: BC.ByteString -> IO BC.ByteString
runGlitcher bytes =
  foldM (\b f -> f b) bytes glitchActions
