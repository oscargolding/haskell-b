{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

x = "Julie"

y = " <3 "

z = "Haskell"

f = x ++ y ++ z

functionH :: [a] -> a
functionH (x : _) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if x > y then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

i :: a -> a
i a = a

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' b a = b

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r first = [head first]

co :: (b -> c) -> (a -> b) -> a -> c
co bc ab a = bc $ ab a

a :: (a -> c) -> a -> a
a aToC a = a

a' :: (a -> b) -> a -> b
a' aTob a = aTob a