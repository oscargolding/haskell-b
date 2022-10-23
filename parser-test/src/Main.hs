{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta
  ( CharParsing (char, string),
    Parser,
    Parsing (eof, unexpected),
    decimal,
    integer,
    parseString,
  )

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1' >>= (\x -> eof >> return x)

integer' :: Parser Integer
integer' = integer >>= (\x -> eof >> return x)

one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2' >>= (\x -> eof >> return x)

oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

stringParser :: Parser String -> IO ()
stringParser p = print $ parseString p mempty "123"

oneStr :: Parser String
oneStr = string "1"

oneTwoStr :: Parser String
oneTwoStr = string "12"

oneTwoThreeStr :: Parser String
oneTwoThreeStr = string "123"

allThree :: Parser String
allThree = oneStr >> oneTwoStr >> oneTwoThreeStr

customString :: String -> String -> Parser String
customString [] s = return s
customString (x : xs) found = char x >>= (\y -> customString xs (found ++ [y]))

string' word = customString word ""

oneStr' = string' "1"

oneTwoStr' = string' "12"

oneTwoThreeStr' = string' "123"

final' = string' "12345"

pNL s = putStrLn ('\n' : s)

badFraction = "1/0"

alsoBad = "10"

shouldWork = "1/2"

shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

testA = parseString integer mempty "123abc"

main :: IO ()
main = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testParse one
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testParse oneTwo
  pNL "oneTwo':"
  testParse oneTwo'
  pNL "oneStr:"
  stringParser oneStr
  pNL "oneTwoStr:"
  stringParser oneTwoStr
  pNL "oneTwoThreeStr:"
  stringParser oneTwoThreeStr
  pNL "unexpected:"
  stringParser stop
  pNL "oneStrC:"
  stringParser oneStr'
  pNL "oneStTwo':"
  stringParser oneTwoStr'
  pNL "oneTwoThrssStr'"
  stringParser oneTwoThreeStr'
  pNL "final"
  stringParser final'
  let virtuousFraction' = parseString virtuousFraction mempty
  print $ virtuousFraction' badFraction
  print $ virtuousFraction' alsoBad
  print $ virtuousFraction' shouldWork
  print $ virtuousFraction' shouldAlsoWork