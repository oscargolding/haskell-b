-- matchingTuples1.hs
module TupleFunctions where

-- These have to be the same type because
addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = fst tup + snd tup

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

k (x, y) = x

k2 = k ("three", 1 + 2)

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))

funcZ x = case x + 1 == 1 of
  True -> "AWESOME"
  False -> "wut"

functionC x y = case x > y of
  True -> x
  False -> y

ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

data Employee = Coder | Manager | Veep | CEO deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = EQ
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

employeeRank ::
  (Employee -> Employee -> Ordering) ->
  Employee ->
  Employee ->
  IO ()
employeeRank f e e' = case f e e' of
  GT -> reportBoss e e'
  EQ -> putStrLn "Neither employee is the boss"
  LT -> (flip reportBoss) e e'

-- Types not provided
-- try filling them in yourself

dodgy x y = x + y * 10

oneIsOne = dodgy 1

oneIsTwo = (flip dodgy) 2

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where
    y = x / 100

pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

tst :: (Ord a, Num a) => a -> Bool
tst x = x < 5