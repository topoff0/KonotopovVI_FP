module Parser.LabyrinthParser
  ( parseLabyrinth,
  )
where

import Config.Config (neighborsSep, roomSep)
import Core.Types
import Data.Char (isSpace)

skipSpaces :: String -> String
skipSpaces = f . f
  where
    f = reverse . dropWhile isSpace

splitBySymbol :: String -> [String]
splitBySymbol [] = []
splitBySymbol s =
  let (x, rest) = break (== neighborsSep) s
   in skipSpaces x : case rest of
        [] -> []
        (_ : xs) -> splitBySymbol xs

splitOnce :: Char -> String -> (String, String)
splitOnce _ [] = ("", "")
splitOnce c (x : xs)
  | x == c = ("", xs)
  | otherwise =
      let (left, right) = splitOnce c xs
       in (x : left, right)

filterNotEmpty :: [String] -> [String]
filterNotEmpty [] = []
filterNotEmpty (x : xs)
  | x == "" = filterNotEmpty xs
  | otherwise = x : filterNotEmpty xs

parseLine :: String -> (Room, [Room])
parseLine line =
  (skipSpaces currentRoom, parseNeighbors rest)
  where
    (currentRoom, rest) = splitOnce roomSep line

parseNeighbors :: String -> [Room]
parseNeighbors [] = []
parseNeighbors (_ : xs) =
  filterNotEmpty (map skipSpaces (splitBySymbol xs))

parseLabyrinth :: String -> Labyrinth
parseLabyrinth text =
  map parseLine (lines text)
