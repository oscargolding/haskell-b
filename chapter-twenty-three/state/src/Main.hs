{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import GHC.Base (VecElem (Int16ElemRep))
import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    -- Use 'error' extremely sparingly
    x -> error $ "intToDie got non 1-6 integer: " ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _) = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

-- refactor to have the number of rolls be a function argument
rollsToGetN :: StdGen -> Int -> Int
rollsToGetN g limit = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= limit = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit g = go 0 0 [] g
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sum count list gen
      | sum >= limit = (count, list)
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) (intToDie die : list) nextGen

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

-- Remember to do the state updates !!!!! (otherwise what is the point?)

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi
      ( \s ->
          let (a, s') = g s
           in (f a, s')
      )

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi (\s -> (a, s))

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) =
    Moi
      ( \s ->
          let (aToB, s') = f s
              (a, s'') = g s'
           in (aToB a, s'')
      )

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g =
    Moi
      ( \s ->
          let (a, s') = f s
              (b, s'') = (runMoi $ g a) s'
           in (b, s'')
      )

getMoi :: Moi s s
getMoi = Moi (\s -> (s, s))

putMoi :: s -> Moi s ()
putMoi s = Moi (const ((), s))

execMoi :: Moi s a -> s -> s
execMoi (Moi sa) s =
  let (_, s') = sa s
   in s'

evalMoi :: Moi s a -> s -> a
evalMoi (Moi sa) = fst . sa

modifyMoi :: (s -> s) -> Moi s ()
modifyMoi fn = Moi (\s -> ((), fn s))

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo start end = fizzbuzzList [end, pred end .. start]

main :: IO ()
main = do
  mapM_ putStrLn $ fizzbuzzFromTo 3 30
