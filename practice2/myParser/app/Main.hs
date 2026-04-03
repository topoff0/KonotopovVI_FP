module Main (main) where

import Lib
import qualified MyParsers.MyAttoparsec as AP
import qualified MyParsers.MyParsec as PP
import qualified MyParsers.MyParser as MP
import MyTypes.MyEither
import MyTypes.MyMaybe
import MyTypes.MyTree

main :: IO ()
main = do
  putStrLn "MyParser:"
  putStrLn $ show (MP.runParser MP.plusOrMult "12*345dsf")
  putStrLn $ show (MP.runParser MP.plusOrMult "12+345dsf")

  putStrLn "Parsec:"
  putStrLn $ show (PP.runParser PP.plusOrMultParsec "12*345dsf")
  putStrLn $ show (PP.runParser PP.plusOrMultParsec "12+345dsf")

  putStrLn "Attoparsec:"
  putStrLn $ show (AP.runParser AP.plusOrMultAttoparsec "12*345dsf")
  putStrLn $ show (AP.runParser AP.plusOrMultAttoparsec "12+345dsf")
