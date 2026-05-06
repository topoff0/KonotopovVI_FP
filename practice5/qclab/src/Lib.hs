module Lib
  ( addMod,
  )
where

addMod :: Int -> Int -> Int -> Int
addMod x y m
  | m <= 0 = error "Модуль должен быть положительным"
  | (x + y) < m = x + y
  | otherwise = addMod (x - m) y m
