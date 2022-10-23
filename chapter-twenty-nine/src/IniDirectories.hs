{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module IniDirectories where

import Control.Applicative (Alternative (some, (<|>)))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as DB
import Data.Char (isAlpha)
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.Directory (getDirectoryContents)
import Text.RawString.QQ (r)
import Text.Trifecta
  ( CharParsing (char),
    Parser,
    Parsing (skipMany),
    Result (Success),
    TokenParsing (token),
    digit,
    letter,
    noneOf,
    oneOf,
    parseByteString,
    parseString,
  )

-- The Types

newtype Header = Header String deriving (Eq, Ord, Show)

type Name = String

type Value = String

type Assignments = Map Name Value

data Section = Section Header Assignments deriving (Eq, Show)

newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

-- The parsing functions

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL
  return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany
    ( do
        _ <- char ';' <|> char '#'
        skipMany (noneOf "\n")
        skipEOL
    )

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) = M.insert h a

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections = foldr rollup M.empty sections
  return (Config mapOfSections)

-- This is a test of what to do here
maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

-- Examples of what to parse
sectionEx'' :: ByteString
sectionEx'' =
  [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

-- IO Section

-- Results of doing the search over the directories
type Results = M.Map String Config

dir :: String
dir = "/home/oscar/repos/haskell/haskell-test/chapter-twenty-nine/src/examples"

handleFiles :: Results -> FilePath -> IO (FilePath, Config)
handleFiles maps filePath = do
  fileRead <- DB.readFile $ dir ++ "/" ++ filePath
  let m = parseByteString parseIni mempty fileRead
  case m of
    Success x -> return (filePath, x)
    _anyOtherResult -> error "failed to parse the file"

readFiles :: [FilePath] -> IO Results
readFiles files = do
  let usingMap = M.empty
  resultOf <- mapM (handleFiles usingMap) files
  return $ M.fromList resultOf

mainSearch :: IO ()
mainSearch = do
  -- Search the specified directory and get the resulting files
  resultsOfSearch <- getDirectoryContents dir
  let filteredResults = filter (isSuffixOf ".ini") resultsOfSearch
  results <- readFiles filteredResults
  print results
  return ()