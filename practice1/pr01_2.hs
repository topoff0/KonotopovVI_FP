-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------

module Pr01_2 where

{-

Напишите реализацию функций:
-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток
-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)
-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
-- myTakeWhile - реализовать с использованием сверток
-- mySpan - реализовать с использованием сверток
-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции
-- myUnFoldr - развертка (операция обратная к свертке)

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-}

myZipSave :: [a] -> [b] -> [(Maybe a, Maybe b)]
myZipSave [] [] = []
myZipSave (x:xs) [] = (Just x, Nothing) : myZipSave xs []
myZipSave [] (y:ys) = (Nothing, Just y) : myZipSave [] ys
myZipSave (x:xs) (y:ys) = (Just x, Just y) : myZipSave xs ys


myUnzipSave :: [(Maybe a, Maybe b)] -> ([a], [b])
myUnzipSave [] = ([], [])
myUnzipSave ((Just a, Nothing):xs) = let (as, bs) = myUnzipSave xs in (a:as, bs)
myUnzipSave ((Nothing, Just b):xs) = let (as, bs) = myUnzipSave xs in (as, b:bs)
myUnzipSave ((Just a, Just b):xs) = let (as, bs) = myUnzipSave xs in (a:as, b:bs)


myReverse :: [a] -> [a]
myReverse xs = foldl (\b x -> x : b) [] xs 


myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 f (x:xs) = foldl f x xs

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 f (x:xs) = foldr f x xs


myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f xs = foldr (\x b -> if f x then x : b else []) [] xs


mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan f xs = foldr (\x (as,bs) -> if f x then (x:as, bs) else ([], x:as ++ bs)) ([], []) xs


myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe y _ Nothing = y
myMaybe _ f (Just x) = f x


data MyList a = MyEmpty | MyCons a (MyList a) deriving Show
myMap :: (a -> b) -> MyList a -> MyList b
myMap _ MyEmpty = MyEmpty
myMap f (MyCons x xs) = MyCons (f x) (myMap f xs)


myUnFoldr :: (b -> Maybe(a,b)) -> b -> [a]
myUnFoldr f x = ff (f x)
    where
        ff Nothing = []
        ff (Just (y,z)) = y : myUnFoldr f z
-- for testing
unfld1 = myUnFoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
unfld2 = take 10 $ myUnFoldr (\(x, y) -> Just (x, (y, x + y))) (0, 1)


