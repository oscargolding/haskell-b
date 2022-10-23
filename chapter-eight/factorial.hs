module Factorial where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

type Numerator = Integer

type Denominator = Integer

type Quotient = Integer

data DividedResult
  = Result (Integer, Integer)
  | DividedByZero

answer :: Integral a => a -> DividedResult
answer first = Result (0, 0)

dividedBy num denom = go (abs num) (abs denom) 0 (num > 0) (denom > 0)
  where
    go n d count posNum posDenom
      | d == 0 = DividedByZero
      | n == 0 = Result (0, 0)
      | n < d && posNum && posDenom = Result (count, n)
      | n < d && posNum && not posDenom = Result (-count, n)
      | n < d && not posNum && not posDenom = Result (count, -n)
      | n < d && not posNum && posDenom = Result (-count, -n)
      | otherwise = go (abs n - abs d) (abs d) (count + 1) posNum posDenom

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ (n + 11)

-- fill in the types
flippy = flip cattyConny

appedCatty = cattyConny "woops"

frappe = flippy "haha"

numberSum :: (Eq a, Num a) => a -> a
numberSum 1 = 1
numberSum n = numberSum (n - 1) + n

recursiveMult :: (Integral a) => a -> a -> a
recursiveMult x 0 = 0
recursiveMult x y = x + recursiveMult x (y - 1)

data DividedResult'
  = Result' Integer
  | DividedByZero'
  deriving (Show)

dividedBy' :: Integral a => a -> a -> DividedResult'
dividedBy' num denom = go num denom 0
  where
    go n d count
      | d == 0 = DividedByZero'
      | d < 0 = case dividedBy' n (-d) of
        DividedByZero' -> DividedByZero'
        Result' r -> Result' (-r)
      | n < 0 = case dividedBy' (-n) d of
        DividedByZero' -> DividedByZero'
        Result' r -> Result' (-r)
      | n < d = Result' count
      | otherwise = go (n - d) d (count + 1)