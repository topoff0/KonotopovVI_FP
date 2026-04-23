module Core.Types
  ( Room,
    Labyrinth,
    Log,
    Game,
    Command (..),
  )
where

import Control.Monad.Reader
import Control.Monad.Writer
import Core.MyStateT

type Room = String

type Labyrinth = [(Room, [Room])]

type Log = [String]

data Command
  = Go Room
  | Look
  | Exit
  | Help
  | Unknown String

type Game m a =
  MyStateT
    Room
    ( WriterT
        Log
        (ReaderT Labyrinth m)
    )
    a
