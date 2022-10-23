module Aspattern where

import Data.Char
import Data.List
import qualified Data.Maybe

f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x : _) = x : xs

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = True
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf first@(x : xs) (y : ys) =
  if x == y
    then isSubseqOf xs ys
    else isSubseqOf first ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords =
  map
    ( \word@(x : xs) -> (word, toUpper x : xs)
    )
    . words

capitalizeWord :: String -> String
capitalizeWord (x : xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph word =
  foldr
    (\x b -> capitalizeFirstChar x ++ "." ++ b)
    ""
    (stringSplit word '.')

capitalizeFirstChar :: String -> String
capitalizeFirstChar [] = []
capitalizeFirstChar (x : xs) =
  if isAlpha x
    then toUpper x : xs
    else x : capitalizeFirstChar xs

stringSplit :: String -> Char -> [String]
stringSplit word char = go [] [] word char
  where
    go list result (x : xs) char
      | x /= char = go (list ++ [x]) result xs char
      | x == char = result ++ [list] ++ go [] result xs char
    go list result [] char = []

type Presses = Int

type Digit = Char

type Characters = String

data Key = Key Digit Characters

newtype DaPhone = DaPhone [Key]

daphone =
  DaPhone
    [ Key '1' "1",
      Key '2' "ABC2",
      Key '3' "DEF3",
      Key '4' "GHI4",
      Key '5' "JKL5",
      Key '6' "MNO6",
      Key '7' "PQRS7",
      Key '8' "TUV8",
      Key '9' "WXYZ9",
      Key '*' "^",
      Key '0' "+_0",
      Key '#' ".,"
    ]

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone keys) char
  | isUpper char = formTap '^' : [formTap $ toUpper char]
  | char == ' ' = [formTap '_']
  | otherwise = [formTap $ toUpper char]
  where
    formTap char = case findKey char of
      Just (Key numeral chars) ->
        ( numeral,
          1
            + Data.Maybe.fromMaybe 0 (elemIndex char chars)
        )
      Nothing -> ('\0', 0)
    findKey char = find (\(Key _ chars) -> char `elem` chars) keys

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concatMap (reverseTaps phone)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

mostPopularLetter :: Eq a => [a] -> a
mostPopularLetter message =
  fst $
    maximumBy
      (\(_, x) (_, y) -> if x < y then LT else GT)
      $ map (\x -> (x, length $ filter (x ==) message)) message

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopularLetter . concatMap words