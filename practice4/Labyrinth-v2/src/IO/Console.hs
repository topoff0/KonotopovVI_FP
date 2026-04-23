module IO.Console
  ( printNeighbors,
    askCommand,
    printLog,
    chooseLabyrinth,
  )
where

import Core.Types (Command (..))
import Game.Command

printNeighbors :: [String] -> IO ()
printNeighbors rooms =
  putStrLn $ "Соседние комнаты: " ++ show rooms

printHelp :: IO ()
printHelp = do
  putStrLn "\nДоступные команды:"
  putStrLn "  go <room>   — перейти в указанную комнату"
  putStrLn "  look        — показать соседние комнаты"
  putStrLn "  exit | q    — выйти из игры"
  putStrLn "  --help      — показать это сообщение\n"

askCommand :: IO Command
askCommand = do
  putStrLn "Введите команду:"
  input <- getLine
  case parseCommand input of
    Help -> do
      printHelp
      askCommand
    Unknown txt -> do
      putStrLn $ "Неизвестная команда '" ++ txt ++ "'"
      putStrLn "Используйте --help для получения списка команд."
      askCommand
    cmd -> return cmd

printLog :: [String] -> IO ()
printLog logs = do
  putStrLn "\nЛОГ:"
  mapM_ putStrLn logs

chooseLabyrinth :: [FilePath] -> IO FilePath
chooseLabyrinth files = do
  putStrLn "Выберите лабиринт:"
  mapM_ printOption (zip [1 ..] files)
  input <- getLine
  case reads input of
    [(n, _)]
      | n > 0 && n <= length files ->
          return (files !! (n - 1))
    _ -> do
      putStrLn "Неверный выбор, попробуйте снова"
      chooseLabyrinth files

printOption :: (Int, FilePath) -> IO ()
printOption (i, name) =
  putStrLn $ show i ++ ". " ++ name
