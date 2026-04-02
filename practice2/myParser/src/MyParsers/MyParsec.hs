module MyParsers.MyParsec where

import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Char (digitToInt)

myDigit :: Parser Int
myDigit = digitToInt <$> digit

digits :: Parser Int
digits = fmap (foldl (\x b -> x * 10 + b) 0) (many1 myDigit)

multParsec :: Parser Int
multParsec = (*) <$> digits <* char '*' <*> digits

plusParsec :: Parser Int
plusParsec = (+) <$> digits <* char '+' <*> digits

plusOrMultParsec :: Parser Int
-- Тут падает, так как parsec нет откатывает процесс парсинга в самое начало
-- (начнет парсить "*..." (например, *123), а будет ждать digits => Nothing
-- plusOrMultParsec = plusParsec <|> multParsec
plusOrMultParsec = try plusParsec <|> multParsec

runParser:: Parser a -> String -> Maybe a
runParser p xs = case parse p "" xs of
  Left _ -> Nothing
  Right x -> Just x
