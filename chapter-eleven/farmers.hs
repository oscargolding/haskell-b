newtype Name = Name String deriving (Show)

newtype Acres = Acres Int deriving (Show)

data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving (Show)

data Farmer = Farmer Name Acres FarmerType deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec
  { name :: Name,
    acres :: Acres,
    farmerType :: FarmerType
  }
  deriving (Show)

-- checking the actual name
isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
  DairyFarmer -> True
  _ -> False

data Quantum = Yes | No | Both deriving (Eq, Show)

quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both
