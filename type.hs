module Type where

import Data.List

newtype TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn integer) (TisAn integer') = integer == integer'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two first second) (Two first' second') =
    first == first' && second == second'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt first) (TisAnInt second) = first == second
  (==) (TisAString first) (TisAString second) = first == second
  (==) _ _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a

instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne b) = a == b
  (==) (ThatOne a) (ThatOne b) = a == b
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello b) = a == b
  (==) (Goodbye a) (Goodbye b) = a == b
  (==) _ _ = False

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Eq, Show)

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer
  defaultNumber :: a

-- pretend newtype is data for now
newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
  fromNumber = Age
  toNumber (Age n) = n
  defaultNumber = Age 65

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
  fromNumber = Year
  toNumber (Year n) = n
  defaultNumber = Year 1988

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfAPrime = toNumber a'
    summed = integerOfA + integerOfAPrime

-- data Person = Person Bool deriving (Show)

-- printPerson :: Person -> IO ()
-- printPerson person = putStrLn (show $ Person True)

data Mood = Blah | Woot deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

type Subject = String

type Verb = String

type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

truth = Papu (Rocks "chomskydoz") (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

f :: RealFrac a => a
f = 1.0

freud :: Int -> Int
freud x = x

jung :: [Int] -> Int
jung xs = head (sort xs)

young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = b == aToB a

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB int a = aToB a + fromInteger int