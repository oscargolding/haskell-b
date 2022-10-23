module Folds where

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny fn = foldr ((||) . fn) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr ((||) . (== a)) False

myElem' a = any (== a)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap fn = foldr ((:) . fn) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter fn = foldr (\x b -> if fn x then x : b else b) []

mySquish :: [[a]] -> [a]
mySquish = foldr (++) []

mySquishMap :: (a -> [b]) -> [a] -> [b]
mySquishMap fn = foldr ((++) . fn) []

squishAgain :: [[a]] -> [a]
squishAgain = mySquishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy fn list =
  foldl
    ( \x b -> case fn x b of
        GT -> x
        _ -> b
    )
    (head list)
    list

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy fn list =
  foldl
    ( \x b -> case fn x b of
        LT -> x
        _ -> b
    )
    (head list)
    list