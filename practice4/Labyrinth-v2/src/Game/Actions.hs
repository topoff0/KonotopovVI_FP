module Game.Actions where

import Control.Monad.Reader
import qualified Core.MyStateT as MST
import Core.Types

getCurrent :: (Monad m) => Game m Room
getCurrent = MST.get

setCurrent :: (Monad m) => Room -> Game m ()
setCurrent = MST.put

getNeighbors :: (Monad m) => Room -> Game m [Room]
getNeighbors room =
  MST.lift (lift ask) >>= \lab ->
    case lookup room lab of
      Nothing -> return []
      Just xs -> return xs
