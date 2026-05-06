module Lib
  ( addMod,
  )
where

addMod :: Int -> Int -> Int -> Int
addMod x y m
  | m <= 0 = error "Модуль должен быть положительным"
  | (x + y) < m && (x + y) > 0 = x + y
  | (x + y) < 0 = addMod (x + m) y m
  | otherwise = addMod (x - m) y m
