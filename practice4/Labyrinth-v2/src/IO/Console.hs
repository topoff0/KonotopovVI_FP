module IO.Console
  ( printNeighbors,
    askCommand,
    printLog,
    chooseLabyrinth,
  )
where

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

askCommand :: IO String
askCommand = do
  putStrLn "Введите команду:"
  input <- getLine

  case words input of
    ["--help"] -> do
      printHelp
      askCommand
    ["look"] -> return input
    ["exit"] -> return input
    ["q"] -> return input
    ["go", _] -> return input
    _ -> do
      putStrLn $ "Неизвестная команда '" ++ input ++ "'"
      putStrLn "Используйте --help для получения списка команд."
      askCommand

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
      putStrLn "Неверный выбор, попробуйте снова."
      chooseLabyrinth files

printOption :: (Int, FilePath) -> IO ()
printOption (i, name) =
  putStrLn $ show i ++ ". " ++ name
