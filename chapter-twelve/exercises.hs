module Main where

import Control.Monad
import Data.Char
import System.Exit
import System.IO

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  if newLine line1 == reverse (newLine line1)
    then do
      putStrLn "It's a palindrome"
      exitSuccess
    else putStrLn "Nope!"
  where
    newLine line = map toLower $ filter isAlpha line

main :: IO ()
main = palindrome

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
    Left $
      PersonInvalidUnknown $
        "Name was: " ++ show name
          ++ " Age was: "
          ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStrLn "Please enter a name: "
  name <- getLine
  putStrLn "Please enter an age: "
  age <- getLine
  case mkPerson name (fromInteger (read age :: Integer)) of
    (Right p) ->
      putStrLn
        ( "Yay! Successfully got a person: "
            ++ show p
        )
    (Left error) -> putStrLn ("error found: " ++ show error)
