{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative (Applicative (liftA2))
import Data.Char (toUpper)

boop = (* 2)

doop = (+ 10)

bip :: Integer -> Integer
bip = boop . boop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . reverse

fmapped :: [Char] -> [Char]
fmapped = fmap cap reverse

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

tupled' :: String -> (String, String)
tupled' = do
  x <- cap
  y <- rev
  return (x, y)

tupled'' :: String -> (String, String)
tupled'' x = ("", x) >>= (\y -> (cap y, rev y))

answer = cap >>= \x -> rev >>= \y -> return (x, y)

newtype Reader r a = Reader {runReader :: r -> a}

ask :: Reader a a
ask = Reader id

newtype HumanName = HumanName String deriving (Eq, Show)

newtype DogName = DogName String deriving (Eq, Show)

newtype Address = Address String deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName,
    dogsAddress :: Address
  }
  deriving (Eq, Show)

pers :: Person
pers =
  Person
    (HumanName "Big Bird")
    (DogName "Barkley")
    (Address "Sesame Street")

chris :: Person
chris =
  Person
    (HumanName "Chris Allen")
    (DogName "Papu")
    (Address "Austin")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

getDogR' = liftA2 Dog dogName address

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 fn fa fb = fn <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks f = Reader f

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\_ -> a)
  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> (runReader $ aRb (ra r)) r

getDogRM :: Reader Person Dog
getDogRM = do
  name <- asks dogName
  addy <- asks address
  return $ Dog name addy

main :: IO ()
main = do
  putStrLn "hey"