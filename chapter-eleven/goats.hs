{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Goats where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany (Int, String) where
  tooMany (n, _) = tooMany n

instance TooMany (Int, Int) where
  tooMany (n1, n2) = tooMany $ n1 + n2

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (n1, n2) = tooMany $ n1 + n2

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

data Person = Person {name :: String, age :: Int} deriving (Eq, Show)

jm = Person "julie" 108

ca = Person "chris" 16

data Fiction = Fiction deriving (Show)

data Nonfiction = Nonfiction deriving (Show)

data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving (Show)

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pfirst :: a,
    psecond :: b
  }
  deriving (Eq, Show)

newtype NumCow = NewCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
