module Config.Config (startRoom, finishRoom, labyrinthFiles) where

startRoom :: String
startRoom = "start"

finishRoom :: String
finishRoom = "finish"

labyrinthFiles :: [FilePath]
labyrinthFiles =
  [ "labyrinths/labyrinth1.txt"
  -- "labyrinths/labyrinth2.txt"
  ]
