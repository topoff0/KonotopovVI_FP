module Core.Types
  ( Room,
    Labyrinth,
    Log,
    Visited,
    Search,
  )
where

import Control.Monad.Reader
import Control.Monad.Writer
import Core.MyStateT

type Room = String

type Labyrinth = [(Room, [Room])]

type Log = [String]

type Visited = [Room]

type Search m a =
  MyStateT
    Visited
    ( WriterT
        Log
        (ReaderT Labyrinth m)
    )
    a
