module Main (main) where

import Lib
import Test.QuickCheck

main :: IO ()
main = do
  putStrLn "Running QuickCheck...(addMod)"
  quickCheck prop_addModMatchesMod
  quickCheck prop_addModNeutral
  quickCheck prop_addModCommutative
  putStrLn "done!"

  putStrLn "Running QuickCheck...(reverseWords)"
  quickCheck prop_reverseWordsEmpty
  quickCheck prop_reverseWordsOneWord
  quickCheck prop_reverseWordsSeveralWords
  quickCheck prop_reverseWordsTwice
  putStrLn "done!"

prop_addModMatchesMod :: Int -> Int -> Int -> Bool
prop_addModMatchesMod x y m =
  addMod x y (makeModule m) == (x + y) `mod` (makeModule m)

prop_addModNeutral :: Int -> Int -> Bool
prop_addModNeutral x m =
  addMod x 0 (makeModule m) == x `mod` (makeModule m)

prop_addModCommutative :: Int -> Int -> Int -> Bool
prop_addModCommutative x y m =
  addMod x y (makeModule m) == addMod y x (makeModule m)

prop_reverseWordsEmpty :: Bool
prop_reverseWordsEmpty =
  reverseWords "" == ""

prop_reverseWordsOneWord :: String -> Bool
prop_reverseWordsOneWord text =
  reverseWords word == word
  where
    word = makeWord text "word"

prop_reverseWordsSeveralWords :: String -> String -> String -> Bool
prop_reverseWordsSeveralWords text1 text2 text3 =
  reverseWords text == expected
  where
    word1 = makeWord text1 "one"
    word2 = makeWord text2 "two"
    word3 = makeWord text3 "three"
    text = word1 ++ " " ++ word2 ++ " " ++ word3
    expected = word3 ++ " " ++ word2 ++ " " ++ word1

prop_reverseWordsTwice :: String -> String -> String -> Bool
prop_reverseWordsTwice text1 text2 text3 =
  reverseWords (reverseWords text) == text
  where
    word1 = makeWord text1 "one"
    word2 = makeWord text2 "two"
    word3 = makeWord text3 "three"
    text = word1 ++ " " ++ word2 ++ " " ++ word3

makeModule :: Int -> Int
makeModule m =
  if m == 0
    then m + 2
    else m

makeWord :: String -> String -> String
makeWord text defaultWord =
  if cleanWord == ""
    then defaultWord
    else cleanWord
  where
    cleanWord = removeSpaces text

removeSpaces :: String -> String
removeSpaces [] = []
removeSpaces (' ' : xs) = removeSpaces xs
removeSpaces (x : xs) = x : removeSpaces xs
