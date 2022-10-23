module Semver where

import Control.Applicative
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

data NumberOrString = NOSS String | NOSI Integer deriving (Show, Eq, Ord)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq)

customComparison :: Release -> Release -> Ordering
customComparison [] [] = EQ
customComparison (_ : _) [] = LT
customComparison [] (_ : _) = GT
customComparison (NOSI x : xs) (NOSI y : ys) = if x == y then customComparison xs ys else compare x y
customComparison (NOSS x : xs) (NOSS y : ys) = if x == y then customComparison xs ys else compare x y
customComparison (NOSI x : xs) _ = LT
customComparison (NOSS x : xs) _ = GT

instance Ord SemVer where
  compare (SemVer major minor patch release _) (SemVer major' minor' patch' release' _) = compare major major' <> compare minor minor' <> compare patch patch' <> customComparison release release'

dotNumOrString :: Parser NumberOrString
dotNumOrString = do
  char '.' *> numOrString

allowedchar :: Parser Char
allowedchar = do
  (char '-') <|> letter

numOrString :: Parser NumberOrString
numOrString = do
  (NOSS <$> some allowedchar) <|> (NOSI <$> natural)

parseNumOrString :: Parser NumberOrString
parseNumOrString =
  dotNumOrString <|> numOrString

parseSpecificRelease :: Parser [NumberOrString]
parseSpecificRelease = do
  _ <- char '-'
  some parseNumOrString

parseMetadata :: Parser [NumberOrString]
parseMetadata = do
  _ <- char '+'
  some parseNumOrString

parseRelease :: Parser [NumberOrString] -> Parser [NumberOrString]
parseRelease a = do
  a <|> return []

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  release <- parseRelease parseSpecificRelease
  metadata <- parseRelease parseMetadata
  return (SemVer major minor patch release metadata)