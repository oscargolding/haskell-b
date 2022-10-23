module Eg where

import Data.Char

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail [x] = Nothing
safeTail (_ : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool _ _ = []

eft :: (Ord a, Enum a) => a -> a -> [a]
eft start end
  | end < start = []
  | otherwise = go start end []
  where
    go start end list
      | start == end = list ++ [start]
      | otherwise = go (succ start) end (list ++ [start])

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

myWords :: String -> [String]
myWords string = go string []
  where
    go string list
      | takeSpace string == dropSpace string = list ++ [string]
      | otherwise = go (next string) (list ++ [getWord string])
    takeSpace = takeWhile (/= ' ')
    dropSpace = dropWhile (== ' ')
    takeWord = takeWhile (/= ' ')
    getWord = takeWord . dropSpace
    next = dropSpace . dropWhile (/= ' ')

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

myNewZip = myZipWith (,)

filterUpper = filter isUpper

capitalise :: String -> String
capitalise (x : xs) = toUpper x : xs

newCapitalise :: String -> String
newCapitalise [] = []
newCapitalise (x : xs) = toUpper x : newCapitalise xs

firstCapitalise :: String -> Char
firstCapitalise string = toUpper $ head string

pointFree :: String -> Char
pointFree = toUpper . head