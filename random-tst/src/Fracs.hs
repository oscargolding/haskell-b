{-# LANGUAGE OverloadedStrings #-}

module Fracs where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta (CharParsing (char), TokenParsing, decimal, parseString)

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

parseFraction :: (Monad m, TokenParsing m, MonadFail m) => m Rational
parseFraction = do
  numerator <- decimal
  _ <- char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

fracsMain :: IO ()
fracsMain = do
  let attoP = parseOnly parseFraction
  print $ attoP badFraction
  print $ attoP shouldWork
  print $ attoP shouldAlsoWork
  print $ attoP alsoBad

  let p f i = parseString f mempty i
  print $ p parseFraction badFraction
  print $ p parseFraction shouldWork
  print $ p parseFraction shouldAlsoWork
  print $ p parseFraction alsoBad