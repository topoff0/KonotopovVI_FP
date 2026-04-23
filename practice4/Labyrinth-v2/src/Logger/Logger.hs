module Logger.Logger
  ( logStart,
    logFinish,
    logDeadEnd,
    logMove,
  )
where

import Control.Monad.Writer
import qualified Core.MyStateT as MST
import Core.Types

logStart :: (Monad m) => Game m ()
logStart =
  MST.lift $ tell ["Начало пути!"]

logFinish :: (Monad m) => Game m ()
logFinish =
  MST.lift $ tell ["Достигнут финиш!"]

logDeadEnd :: (Monad m) => Room -> Game m ()
logDeadEnd room =
  MST.lift $ tell ["Тупик: " ++ room]

logMove :: (Monad m) => Room -> Game m ()
logMove room =
  MST.lift $ tell ["Переход в: " ++ room]
