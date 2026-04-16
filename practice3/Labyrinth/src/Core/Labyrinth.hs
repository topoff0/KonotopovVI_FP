module Core.Labyrinth (labyrinth) where

import Config.RoomNames
import Core.Types (Labyrinth)

labyrinth :: Labyrinth
labyrinth =
  [ (startRoom, [roomA1, roomA2, roomA3]),
    (roomA1, [roomB1]),
    (roomA2, [roomB2, roomB3]),
    (roomA3, [roomB3, roomB4, roomB5]),
    (roomB1, [roomC1]),
    (roomB2, [roomB3]),
    (roomB3, [roomC2]),
    (roomB4, [roomC1]),
    (roomB5, [roomC2]),
    (roomC1, [roomD1, roomD2, roomD3]),
    (roomC2, [roomD4]),
    (roomD1, [roomE1]),
    (roomD2, [roomD3]),
    (roomD3, [roomD4]),
    (roomD4, [roomE1]),
    (roomE1, [deadRoom, roomF1, roomF2, roomF3, roomF4, roomF5]),
    (roomF1, [roomF2]),
    (roomF2, [roomF3]),
    (roomF3, [finishRoom]),
    (roomF4, [roomF3]),
    (roomF5, [roomF4]),
    (deadRoom, []),
    (finishRoom, [])
  ]
