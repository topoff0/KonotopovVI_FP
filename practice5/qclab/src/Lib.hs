module Lib
  ( addMod,
    reverseWords,
  )
where

addMod :: Int -> Int -> Int -> Int
addMod x y m
  | m <= 0 = error "Модуль должен быть положительным"
  | (x + y) < m && (x + y) > 0 = x + y
  | (x + y) < 0 = addMod (x + m) y m
  | otherwise = addMod (x - m) y m

reverseWords :: String -> String
reverseWords text = rev text "" ""
  where
    rev [] word result = putWord word result
    rev (' ' : xs) word result = rev xs "" (putWord word result)
    rev (x : xs) word result = rev xs (word ++ [x]) result

    putWord "" result = result
    putWord word "" = word
    putWord word result = word ++ " " ++ result
