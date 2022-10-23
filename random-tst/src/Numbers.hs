module Numbers where

import Control.Applicative
import Data.Char (digitToInt)
import Data.Monoid
import Text.Parser.Char (digit)
import Text.Trifecta
  ( CharParsing (char),
    Parser,
    Parsing (try),
    count,
    integer,
    letter,
    natural,
    noneOf,
  )

type NumberingPlanArea = Int

type Exchange = Int

type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone = do
  plan <- parseNumberingPlanArea
  exchange <- parseExchangeArea
  PhoneNumber plan exchange <$> parseLineNumber

digits :: Int -> Parser Int
digits n = read <$> count n digit

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea = do
  try (char '(' *> digits 3 <* char ')')
    <|> try (char '1' *> char '-' *> digits 3)
    <|> digits 3

parseExchangeArea :: Parser Exchange
parseExchangeArea = do
  try (char ' ' *> digits 3) <|> try (char '-' *> digits 3) <|> digits 3

parseLineNumber :: Parser LineNumber
parseLineNumber = do
  try (char ' ' *> digits 4) <|> try (char '-' *> digits 4) <|> digits 4
