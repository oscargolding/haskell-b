{-# LANGUAGE OverloadedStrings #-}

module Bt where

import Control.Applicative (Alternative ((<|>)))
import Data.Attoparsec.ByteString (parseOnly)
import qualified Data.Attoparsec.ByteString as A
import Data.ByteString (ByteString)
import Text.Parsec (Parsec, parseTest)
import Text.Trifecta (CharParsing (char), Parser, parseString, try, (<?>))

-- helper
trifP :: Show a => Parser a -> String -> IO ()
trifP p i = print $ parseString p mempty i

parsecP :: (Show a) => Parsec String () a -> String -> IO ()
parsecP = parseTest

attoP :: Show a => A.Parser a -> ByteString -> IO ()
attoP p i = print $ parseOnly p i

nobackParse :: (Monad f, CharParsing f) => f Char
nobackParse = (char '1' >> char '2') <|> char '3'

tryParse :: (Monad f, CharParsing f) => f Char
tryParse = try (char '1' >> char '2') <|> char '3'

tryAnnot :: (Monad f, CharParsing f) => f Char
tryAnnot =
  (try (char '1' >> char '2') <?> "Tried 12")
    <|> (char '3' <?> "Tried 3")

btMain :: IO ()
btMain = do
  trifP nobackParse "13"
  trifP tryParse "13"
  parsecP nobackParse "13"
  parsecP tryParse "13"
  attoP nobackParse "13"
  attoP tryParse "13"