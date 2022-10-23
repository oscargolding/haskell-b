module WordNumber where

import Data.List (intercalate, intersperse)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = ""

digits :: Int -> [Int]
digits n = go n []
  where
    go n list
      | div n 10 == 0 = n : list
      | otherwise = go (div n 10) $ mod n 10 : list

wordNumber :: Int -> String
wordNumber n = (intercalate "-" . map digitToWord) $ digits n