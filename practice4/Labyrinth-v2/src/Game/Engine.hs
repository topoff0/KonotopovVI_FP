module Game.Engine (runGame) where

import Config.Config (finishRoom, startRoom)
import Control.Monad.Reader
import Control.Monad.Writer
import qualified Core.MyStateT as MST
import Core.Types
import Game.Actions
import IO.Console
import Logger.Logger

liftIOGame :: IO a -> Game IO a
liftIOGame = MST.lift . lift . lift

runGame :: Labyrinth -> IO ()
runGame lab = do
  ((_, _), l) <-
    runReaderT
      (runWriterT (MST.runMyStateT gameLoop startRoom))
      lab
  printLog l

gameLoop :: Game IO ()
gameLoop = do
  logStart
  current <- getCurrent
  showCurrent current
  showNeighbors current
  loop

loop :: Game IO ()
loop = do
  current <- getCurrent
  showCurrent current

  cmd <- liftIOGame askCommand
  handleCommand cmd current

showCurrent :: Room -> Game IO ()
showCurrent room =
  liftIOGame $
    putStrLn $
      "\nВы в комнате: " ++ room

handleCommand :: Command -> Room -> Game IO ()
handleCommand cmd current =
  case cmd of
    Look -> do
      showNeighbors current
      loop
    Go room ->
      handleMove current room
    Exit ->
      return ()
    Help ->
      loop
    Unknown _ ->
      loop

showNeighbors :: Room -> Game IO ()
showNeighbors room = do
  neighbors <- getNeighbors room
  liftIOGame $ printNeighbors neighbors

isValidMove :: Room -> [Room] -> Bool
isValidMove = elem

handleDeadEnd :: Room -> Game IO ()
handleDeadEnd room = do
  logDeadEnd room
  liftIOGame $
    putStrLn "Вы попали в тупик! Возврат в начало..."
  setCurrent startRoom
  logStart
  loop

moveToRoom :: Room -> Game IO ()
moveToRoom room = do
  setCurrent room
  logMove room

checkCurrentRoom :: Room -> Game IO ()
checkCurrentRoom room = do
  if room == finishRoom
    then logFinish
    else do
      next <- getNeighbors room
      if null next
        then handleDeadEnd room
        else loop

handleMove :: Room -> Room -> Game IO ()
handleMove current room = do
  neighbors <- getNeighbors current

  if isValidMove room neighbors
    then do
      moveToRoom room
      checkCurrentRoom room
    else do
      liftIOGame $
        putStrLn "Нельзя перейти в эту комнату"
      loop
