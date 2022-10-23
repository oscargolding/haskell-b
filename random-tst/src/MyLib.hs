{-# LANGUAGE QuasiQuotes #-}

module MyLib (someFunc) where

import Control.Applicative (Alternative (many, some, (<|>)))
import Data.Ratio ((%))
import Text.RawString.QQ (r)
import Text.Trifecta
  ( CharParsing (char),
    Parser,
    Parsing (skipMany),
    decimal,
    integer,
    letter,
    oneOf,
    parseString,
    try,
  )

---- Types
type NumberOrString = Either Integer String

type FractionOrDecimal = Either Rational Double

----

---- Data
eitherOr :: String
eitherOr =
  [r|
123
abc+24
456
def|]

fracDecimal :: String
fracDecimal =
  [r|
4/12
3.21234
34/12|]

a = "blah"

b = "123"

c = "123blah789"

----

---- Parsers
parseNos :: Parser NumberOrString
parseNos =
  skipMany (oneOf "\n")
    >> (Left <$> integer) <|> (Right <$> some letter)

parseFractionOrDecimal :: Parser FractionOrDecimal
parseFractionOrDecimal =
  skipMany (oneOf "\n")
    >> try (Left <$> parseFraction) <|> (Right <$> parseDecimal)

parserWithTry =
  skipMany (oneOf "\n")
    >> try (Left <$> parseFraction) <|> (Right <$> parseDecimal)

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = do
  first <- decimal
  char '.'
  afterPoint <- decimal
  return (fromIntegral first + decimalValue afterPoint)
  where
    decimalValue :: Integer -> Double
    decimalValue 0 = 0
    decimalValue a = fromIntegral a / fromIntegral (powerLevel a)
      where
        powerLevel :: Integer -> Int
        powerLevel num = 10 ^ length (digits' num)

        digits' :: Integer -> [Integer]
        digits' 0 = []
        digits' number = r : digits' q
          where
            (q, r) = quotRem number 10

----

---- Utility functions
someFunc :: IO ()
someFunc = do
  let p f i = parseString f mempty i
  print "1"
  print $ p (some letter) a
  print "2"
  print $ p integer b
  print "3"
  print $ p parseNos a
  print "4"
  print $ p parseNos b
  print $ p (many parseNos) c
  print $ p (some parseNos) c
  print $ p parseNos eitherOr
  print $ parseString parseDecimal mempty "3.2134"
  print $ parseString parseFraction mempty "1/4"
  print $ p parseDecimal "3.2134"
  print $ p parseFractionOrDecimal "3.2134"
  print $ p parseFractionOrDecimal "3/24"
  print $ p (some parseFractionOrDecimal) fracDecimal
  print $ p (some parseNos) eitherOr
