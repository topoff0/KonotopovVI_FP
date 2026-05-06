module Lib
  ( addMod,
    reverseWords,
  )
where

addMod :: Int -> Int -> Int -> Int
addMod x y m
  | m == 0 = error "Модуль не может быть равен 0"
  | otherwise = normalize (x + y)
  where
    normalize value
      | m > 0 && value < 0 = normalize (value + m)
      | m > 0 && value >= m = normalize (value - m)
      | m < 0 && value > 0 = normalize (value + m)
      | m < 0 && value <= m = normalize (value - m)
      | otherwise = value

reverseWords :: String -> String
reverseWords text = rev text "" ""
  where
    rev [] word result = putWord word result
    rev (' ' : xs) word result = rev xs "" (putWord word result)
    rev (x : xs) word result = rev xs (word ++ [x]) result

    putWord "" result = result
    putWord word "" = word
    putWord word result = word ++ " " ++ result
