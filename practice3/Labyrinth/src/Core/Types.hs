module Core.Types (Room, Labyrinth, Log, Visited, Search) where

import Control.Monad.RWS

type Room = String

type Labyrinth = (Room, [Room])

type Log = [String]

type Visited = [Room]

type Search = RWS Log Labyrinth Visited
