module Config.Config (startRoom, finishRoom, labyrinthFiles, roomSep, neighborsSep) where

startRoom :: String
startRoom = "start"

finishRoom :: String
finishRoom = "finish"

roomSep :: Char
roomSep = ':'

neighborsSep :: Char
neighborsSep = '|'

labyrinthFiles :: [FilePath]
labyrinthFiles =
  [ "labyrinths/labyrinth1.txt",
    "labyrinths/labyrinth2.txt"
  ]
