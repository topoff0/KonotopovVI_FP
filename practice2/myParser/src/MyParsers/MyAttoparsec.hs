module MyParsers.MyAttoparsec where

import Data.Attoparsec.Text
import Data.Char (digitToInt)
import Control.Applicative ((<|>))
import qualified Data.Text as T

myDigit :: Parser Int
myDigit = digitToInt <$> digit

digits :: Parser Int
digits = fmap (foldl(\x b -> x * 10 + b) 0) (many1 myDigit)

multAttoparsec :: Parser Int
multAttoparsec = (*) <$> digits <* char '*' <*> digits

plusAttoparsec :: Parser Int
plusAttoparsec = (+) <$> digits <* char '+' <*> digits

plusOrMultAttoparsec :: Parser Int
plusOrMultAttoparsec = plusAttoparsec <|> multAttoparsec

runParser :: Parser a -> String -> Maybe a
runParser p xs = case parseOnly p (T.pack xs) of
  Left _ -> Nothing
  Right x -> Just x

