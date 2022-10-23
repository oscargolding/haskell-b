module MyInt where

import Control.Applicative
import Data.Char (digitToInt)
import Data.Monoid
import Text.Trifecta
  ( CharParsing (char),
    Parser,
    Parsing (try),
    integer,
    letter,
    natural,
    noneOf,
  )

parseDigit :: Parser Char
parseDigit =
  char '0' <|> char '1' <|> char '2' <|> char '3'
    <|> char '4'
    <|> char '5'
    <|> char '6'
    <|> char '7'
    <|> char '8'
    <|> char '9'

stringToInteger :: String -> Int
stringToInteger nums = foldr go 0 zippedList
  where
    go :: (Int, Int) -> Int -> Int
    go (index, number) accum = (number * 10 ^ index) + accum

    listLength :: Int
    listLength = length nums

    zippedList :: [(Int, Int)]
    zippedList = zip (reverse [0 .. listLength - 1]) realNums

    realNums :: [Int]
    realNums = digitToInt <$> nums

base10Integer :: Parser Int
base10Integer = do
  digits <- some parseDigit
  return $ stringToInteger digits

base10Integer' :: Parser Int
base10Integer' = do
  _ <- char '-'
  digits <- some parseDigit
  return $ (-1) * stringToInteger digits

finalBase10 :: Parser Int
finalBase10 = do
  base10Integer' <|> base10Integer