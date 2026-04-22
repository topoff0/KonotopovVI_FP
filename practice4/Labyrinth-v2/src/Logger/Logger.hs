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

logStart :: (Monad m) => Search m ()
logStart =
  MST.lift $ tell ["Начало пути!"]

logFinish :: (Monad m) => Search m ()
logFinish =
  MST.lift $ tell ["Достигнут финиш!"]

logDeadEnd :: (Monad m) => Room -> Search m ()
logDeadEnd room =
  MST.lift $ tell ["Тупик: " ++ room]

logMove :: (Monad m) => Room -> Search m ()
logMove room =
  MST.lift $ tell ["Переход в: " ++ room]
