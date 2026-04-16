module Main (main) where

import Config.RoomNames (startRoom)
import Control.Monad.RWS (runRWS)
import Core.Labyrinth (labyrinth)
import Logger.Logger (logStart)
import Searcher.Search (findPath)

main :: IO ()
main =
  let (result, _, logs) = runRWS (logStart >> findPath startRoom) labyrinth []
   in mapM_ putStrLn logs
        >> print result
