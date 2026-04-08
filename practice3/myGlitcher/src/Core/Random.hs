module Core.Random where

import Core.Pure (replaceByte, sortSection)
import qualified Data.ByteString.Char8 as BC
import System.Random (randomRIO)

randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let bytesLength = BC.length bytes
  location <- randomRIO (1, bytesLength)
  chV <- randomRIO (0, 255)
  return (replaceByte location chV bytes)

randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let sectionSize = 25
  let bytesLength = BC.length bytes
  start <- randomRIO (0, bytesLength - sectionSize)
  return (sortSection start sectionSize bytes)
