module Cipher where

import Data.Char

cipher :: String -> Int -> (Int -> Int -> Int) -> String
cipher [] rot fn = []
cipher (x : xs) rot fn
  | isLower x = chr (perform x 97 rot) : cipher xs rot fn
  | isUpper x = chr (perform x 65 rot) : cipher xs rot fn
  | otherwise = x : cipher xs rot fn
  where
    perform x loc rotation = loc + shift x loc rotation
    shift x loc shift = mod (rotate x loc shift) 26
    rotate x loc = fn (base x loc)
    base x loc = ord x - loc

encrypt word rot = cipher word rot (+)

decrypt word rot = cipher word rot (-)

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x : xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = True
myAny fn (x : xs) = fn x || myAny fn xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem el (x : xs) = x == el || myElem el xs

myAnyElem :: Eq a => a -> [a] -> Bool
myAnyElem a list = any (== a) list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap fn (x : xs) = fn x ++ squishMap fn xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy fn (x : xs) = if fn x res == GT then x else res
  where
    res = myMaximumBy fn xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy fn (x : xs) = if fn x res == LT then x else res
  where
    res = myMinimumBy fn xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
