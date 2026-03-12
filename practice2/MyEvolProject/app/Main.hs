module Main (main) where
import MyEvolModule 
import Data.List (sort)

main :: IO ()
main = putStrLn $ show $ sort ([Humans, LUCA, Trilobite, Dimetrodon] :: [MyEvolution])
