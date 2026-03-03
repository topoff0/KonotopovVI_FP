-------------------------------------
-- Практические задание 1. Часть 1 --
-------------------------------------

module Pr01_1 where

{-

Напишите реализацию функций myFST, mySND, myTHRD для кортежа (a,b,c)

Напишите реализацию стандартных функции для работы со списками:
-- myHead - определение (через сопоставление с образцом) функции отделения головы списка
-- myTail - функция отделения хвоста списка
-- myTake - взять первые n элементов списка
-- myDrop - отбросить первые n элементов списка
-- myProduct - перемножить все элементы списка
-- myZip - попарное объединение двух списков в список пар, длина итогового списка по длине самого короткого из входных списков
-- myZip3 объединение трех списков в список троек
-- myUnzip - разделение списка пар на пару списков (2 реализции)

Напишите реализацию стандартных функции высшего порядка для работы со списками:
-- myFilter - применение предиката к каждому элементу списка (две реализации: с использованием охранных выражений и if-then-else)
-- myMap - применение функции одного аргумента к каждому элементу списка
-- myZipWith - применение функции двух аргументов к двум спискам
-- myZipWith3 - применение функции трех аргументов к трем спискам
-- myAll - проверяет удовлетворяют ли все элементы списка предикату
-- myAny - проверяет удовлетворяют ли хотя бы один элемент списка предикату
-- myComposition - композиция двух функций (.)

-}

myFST :: (a, b, c) -> a
myFST (x, y, z) = x

mySND :: (a, b, c) -> b
mySND (x, y, z) = y

myTHRD :: (a, b, c) -> c
myTHRD (x, y, z) = z


myHead :: [a] -> a
myHead (x:xs) = x 


myTail :: [a] -> [a]
myTail (x:xs) = xs


myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n _ | n <= 0 = []
myTake n (x:xs) = x : myTake (n-1) xs


myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n xs | n <= 0 = xs
myDrop n (x:xs) = myDrop (n-1) xs


myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs


myZip :: [a] -> [b] -> [(a, b)]
myZip [] xs = []
myZip xs [] = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys


myZip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3 [] _ _ = []
myZip3 _ [] _ = []
myZip3 _ _ [] = []
myZip3 (x:xs) (y:ys) (z:zs) = (x, y, z) : myZip3 xs ys zs


myUnzip :: [(a,b)] -> ([a], [b])
myUnzip [] = ([], [])
myUnzip ((x, y):xys) = (x:xs, y:ys)
    where (xs, ys) = myUnzip xys

-- NOTE: myUnzip second implementation
-- myUnzip :: [(a,b)] -> ([a], [b])
-- myUnzip [] = ([], [])
-- myUnzip xs  = (map fst xs, map snd xs)



myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs)
    | f x = x : myFilter f xs
    | otherwise = myFilter f xs


-- NOTE: myFilter 'if' implementation
-- myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter _ [] = []
-- myFilter f (x:xs) = 
--     if f x
--         then x : myFilter f xs
--         else myFilter f xs


myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs


myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys


myZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
myZipWith3 _ [] _ _ = []
myZipWith3 _ _ [] _ = []
myZipWith3 _ _ _ [] = []
myZipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : myZipWith3 f xs ys zs


myAll :: (a -> Bool) -> [a] -> Bool
myAll _ [] = True
myAll f (x:xs) = f x && myAll f xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myComposition :: (b -> c) -> (a -> b) -> a -> c
myComposition f2 f1 x = f2 (f1 x)

