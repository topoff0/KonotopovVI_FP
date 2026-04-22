module Config.Config (startRoom, finishRoom, labyrinthFiles) where

startRoom :: String
startRoom = "start"

finishRoom :: String
finishRoom = "finish"

labyrinthFiles :: [FilePath]
labyrinthFiles =
  [ "labyrinth1.txt",
    "labyrinth2.txt"
  ]
