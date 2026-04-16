module Logger.Logger where

import Control.Monad.RWS
import Core.Types

logStart :: Search ()
logStart =
  tell ["Начало пути!"]

logFinish :: Search ()
logFinish =
  tell ["Достигнут финиш!"]

logDeadEnd :: Room -> Search ()
logDeadEnd room =
  tell ["Тупик: " ++ room]

logMove :: Room -> Search ()
logMove room =
  tell ["Переход в: " ++ room]
