module DifferenceList where

import Criterion.Main

newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a = DL (a :)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList dList = unDL dList []
{-# INLINE toList #-}

infixr 9 `cons`

cons :: a -> DList a -> DList a
cons x xs = DL ((x :) . unDL xs)
{-# INLINE cons #-}

infixl 9 `snoc`

snoc :: DList a -> a -> DList a
snoc xs x = xs `append` singleton x
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append firstList secondList = DL (firstListFn . secondListFn)
  where
    firstListFn = unDL firstList
    secondListFn = unDL secondList
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) ([n, n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) ((singleton n `append` singleton n) `append` xs)

main :: IO ()
main =
  defaultMain
    [ bench "concat list" $ whnf schlemiel 123456,
      bench "concat dlist" $ whnf constructDlist 123456
    ]