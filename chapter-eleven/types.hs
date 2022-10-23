{-# LANGUAGE LambdaCase #-}

module Types where

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data PlaneSize = Small | Medium | Large deriving (Eq, Show)

data Example = MakeExample Int deriving (Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer Price
  | Plane Airline PlaneSize
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir Small

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars =
  map
    ( \case
        (Car _ _) -> True
        _ -> False
    )

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x

class TooMany a where
  tooMany :: a -> Bool

newtype Goats = Goats Int deriving (Show)

instance TooMany Goats where
  tooMany (Goats n) = n > 43