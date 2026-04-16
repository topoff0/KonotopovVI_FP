module Searcher.Search where

import Config.RoomNames (finishRoom)
import Control.Monad.RWS
import Core.Types
import Logger.Logger

isFinish :: Room -> Bool
isFinish room = room == finishRoom

isVisited :: Room -> Visited -> Bool
isVisited room visited =
  room `elem` visited

visit :: Room -> Search ()
visit room =
  get >>= \visited ->
    put (room : visited)

getNext :: Room -> Search [Room]
getNext room =
  ask >>= \labyrinth ->
    case lookup room labyrinth of
      Nothing -> return []
      Just xs -> return xs

finishResult :: Room -> Search (Maybe [Room])
finishResult room =
  logMove room
    >> logFinish
    >> return (Just [room])

findPath :: Room -> Search (Maybe [Room])
findPath room =
  get >>= \visited ->
    if isFinish room
      then finishResult room
      else
        if isVisited room visited
          then return Nothing
          else
            visit room
              >> logMove room
              >> getNext room
              >>= \nextRooms ->
                searchList nextRooms >>= \result ->
                  case result of
                    Nothing ->
                      logDeadEnd room
                        >> return Nothing
                    Just path ->
                      return (Just (room : path))

searchList :: [Room] -> Search (Maybe [Room])
searchList [] =
  return Nothing
searchList (r : rs) =
  findPath r >>= \result ->
    case result of
      Nothing ->
        searchList rs
      Just path ->
        return (Just path)
