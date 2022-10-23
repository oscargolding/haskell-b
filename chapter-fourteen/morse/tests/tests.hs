{-# LANGUAGE DeriveGeneric #-}

-- tests/tests.hs

module Main where

import Data.Char
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Monoid as DM
import GHC.Generics
import Libraries
  ( decrypt,
    decryptCaesar,
    digitToWord,
    digits,
    encrypt,
    encryptCaesar,
    wordNumber,
  )
import Morse (Morse, charToMorse, letterToMorse, morseToChar)
import Test.Hspec
import Test.QuickCheck

allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
  forAll
    charGen
    ( \c ->
        (charToMorse c >>= morseToChar)
          == Just c
    )

half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t) = (Just y, t)
    go y (Just x, _) = (Just y, x >= y)

plusAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Eq a, Num a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

multAssociative :: (Eq a, Num a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Eq a, Num a) => a -> a -> Bool
multCommutative x y = x * y == y * x

powerAssociative :: Integer -> Integer -> Bool
powerAssociative x y = x ^ y == y ^ x

prop_check :: Gen Bool
prop_check = do
  x <- choose (5, 99999) :: Gen Integer
  y <- choose (5, 99999) :: Gen Integer
  return $ quot x y * y + rem x y == x

prop_check_div_mod :: Gen Bool
prop_check_div_mod = do
  x <- choose (5, 99999) :: Gen Integer
  y <- choose (5, 99999) :: Gen Integer
  return $ div x y * y + mod x y == x

reverseTwice :: [a] -> [a]
reverseTwice = reverse . reverse

testDollar :: Fun Double Int -> Double -> Bool
testDollar (Fn f) s = (f $ s) == f s
testDollar _ _ = False

firstEqual :: [Int] -> [Int] -> Bool
firstEqual list sd = foldr (:) sd list == (++) sd list

checkConcat :: [[Int]] -> Bool
checkConcat list = foldr (++) [] list == concat list

-- No not always
hmmIsSo :: Int -> [Int] -> Bool
hmmIsSo n xs = length (take n xs) == n

roundt :: Int -> Bool
roundt x = read (show x) == x

twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord (x : xs) = toUpper x : xs
capitalizeWord [] = []

first :: String -> Bool
first x =
  (capitalizeWord x == twice capitalizeWord x)
    && (capitalizeWord x == fourTimes capitalizeWord x)

second :: [Int] -> Bool
second x = (sort x == twice sort x) && (sort x == fourTimes sort x)

data Fool = Fulse | Frue deriving (Eq, Show)

genFool :: Gen Fool
genFool = elements [Fulse, Frue]

genFrequency :: Gen Fool
genFrequency = frequency [(2, return Fulse), (1, return Frue)]

testVigenere :: PrintableString -> PrintableString -> Bool
testVigenere word key =
  decrypt (encrypt usingWord usingKey) usingKey
    == usingWord
  where
    usingWord = show word
    usingKey = show key

testCaesar :: PrintableString -> Int -> Bool
testCaesar word num = decryptCaesar (encryptCaesar strWord num) num == strWord
  where
    strWord = show word

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance Semigroup (First' a) where
  (First' Nada) <> empty = empty
  empty <> (First' Nada) = empty
  (First' (Only x)) <> (First' (Only _)) = First' (Only x)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

type FstId = First' String -> Bool

-- The identities --
monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = mempty <> a == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- --

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    res <- arbitrary
    frequency
      [ (1, return $ First' Nada),
        (1, return $ First' $ Only res)
      ]

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- q1
data Trivial = Trivial deriving (Eq, Show)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

-- q2
newtype Identity' a = Identity' a deriving (Show, Eq)

type IdentityAssoc' = Identity' String -> Identity' String -> Identity' String -> Bool

instance (Monoid a) => Monoid (Identity' a) where
  mempty = Identity' mempty
  mappend = (<>)

instance (Semigroup a) => Semigroup (Identity' a) where
  (Identity' x) <> (Identity' y) = Identity' (x <> y)

instance Arbitrary a => Arbitrary (Identity' a) where
  arbitrary = do
    Identity' <$> arbitrary

-- q3
data Two a b = Two a b deriving (Eq, Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    Two x <$> arbitrary

type TwoAssoc =
  Two String (DM.Product Int) ->
  Two String (DM.Product Int) ->
  Two String (DM.Product Int) ->
  Bool

-- q4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    Three x y <$> arbitrary

type ThreeAssoc =
  Three String (DM.Product Int) (DM.Sum Int) ->
  Three String (DM.Product Int) (DM.Sum Int) ->
  Three String (DM.Product Int) (DM.Sum Int) ->
  Bool

-- q5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a s d f) <> (Four a' s' d' f') =
    Four
      (a <> a')
      (s <> s')
      (d <> d')
      (f <> f')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    Four x y z <$> arbitrary

type FourAssoc =
  Four String (DM.Product Int) (DM.Sum Int) (DM.Sum Int) ->
  Four String (DM.Product Int) (DM.Sum Int) (DM.Sum Int) ->
  Four String (DM.Product Int) (DM.Sum Int) (DM.Sum Int) ->
  Bool

-- q6
newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Semigroup BoolConj where
  (BoolConj False) <> _ = BoolConj False
  _ <> (BoolConj False) = BoolConj False
  _ <> _ = BoolConj True

instance Arbitrary BoolConj where
  arbitrary =
    frequency
      [ (1, return $ BoolConj True),
        (1, return $ BoolConj False)
      ]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- q7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _ = BoolDisj True
  _ <> (BoolDisj True) = BoolDisj True
  _ <> _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary =
    frequency
      [ (1, return $ BoolDisj True),
        (1, return $ BoolDisj False)
      ]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- q8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd a) <> _ = Snd a
  _ <> (Snd a) = Snd a
  _ <> y = y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Fst x), (1, return $ Snd y)]

type OrAssoc = Or String Int -> Or String Int -> Or String Int -> Bool

-- q9
newtype Combine a b = Combine {unCombine :: a -> b}

instance (Monoid b) => Monoid (Combine a b) where
  mempty = Combine mempty
  mappend = (<>)

instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f1) <> (Combine f2) = Combine (f1 <> f2)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    Combine <$> arbitrary

instance Show (Combine a b) where
  show (Combine _) = "Combine"

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Property

funEquality ::
  (Arbitrary a, Show a, Eq b, Show b) =>
  Combine a b ->
  Combine a b ->
  Property
funEquality (Combine f) (Combine g) = property $ \a -> f a == g a

combineAssoc :: (Arbitrary a, Show a, Eq b, Show b, Semigroup b) => CombineAssoc a b
combineAssoc f g h = ((f <> g) <> h) `funEquality` (f <> (g <> h))

------ or you do not have to check the functions and can uncombine
type CombAssoc a b = Combine a b -> Combine a b -> Combine a b -> a -> Bool

combSemigroupAssoc ::
  (Eq b, Semigroup b) =>
  Combine a b ->
  Combine a b ->
  Combine a b ->
  a ->
  Bool
combSemigroupAssoc f g h a =
  unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

combMonoidLeftId :: (Eq m, Monoid m) => Combine a m -> a -> Bool
combMonoidLeftId f a = unCombine (f <> mempty) a == unCombine f a

combMonoidRightId :: (Eq m, Monoid m) => Combine a m -> a -> Bool
combMonoidRightId f a = unCombine (mempty <> f) a == unCombine f a

-- q10
newtype Comp a = Comp {unComp :: a -> a}

instance (Monoid a) => Monoid (Comp a) where
  mempty = Comp mempty
  mappend = (<>)

instance (Semigroup a) => Semigroup (Comp a) where
  (Comp x) <> (Comp y) = Comp (x <> y)

instance Show (Comp a) where
  show (Comp _) = "Comp"

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    Comp <$> arbitrary

type CompAssoc a = Comp a -> Comp a -> Comp a -> a -> Bool

compSemigroupAssoc ::
  (Eq a, Semigroup a) =>
  Comp a ->
  Comp a ->
  Comp a ->
  a ->
  Bool
compSemigroupAssoc f g h a =
  unComp (f <> (g <> h)) a
    == unComp ((f <> g) <> h) a

compMonoidLeftId :: (Eq m, Monoid m) => Comp m -> m -> Bool
compMonoidLeftId f a = unComp (f <> mempty) a == unComp f a

compMonoidRightId :: (Eq m, Monoid m) => Comp m -> m -> Bool
compMonoidRightId f a = unComp (mempty <> f) a == unComp f a

-- instance Semigroup (First' a) where

-- q11
data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (Success' x) <> _ = Success' x
  _ <> (Success' x) = Success' x
  (Failure' a) <> (Failure' b) = Failure' (a <> b)

instance (Arbitrary a, Arbitrary b, Semigroup a) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Failure' y), (1, return $ Success' x)]

type ValidationAssoc =
  Validation String Int ->
  Validation String Int ->
  Validation String Int ->
  Bool

-- final question
newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem (\a -> (mempty, a))
  mappend = (<>)

beerFn :: (Semigroup a) => s -> Mem s a -> Mem s a -> Mem s a
beerFn a (Mem f) (Mem g) = Mem (const (a' <> a'', s''))
  where
    (a', s') = f a
    (a'', s'') = g s'

instance Semigroup a => Semigroup (Mem s a) where
  (Mem x) <> (Mem x') =
    Mem
      ( \a ->
          ( fst (x a) <> fst (x' a),
            snd $ x' $ snd $ x a
          )
      )

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Mem s a) where
  arbitrary = Mem <$> arbitrary

instance Show (Mem a b) where
  show (Mem _) = "Mem"

memorySemigroupAssoc ::
  (Eq a, Eq b, Semigroup b) =>
  Mem a b ->
  Mem a b ->
  Mem a b ->
  a ->
  Bool
memorySemigroupAssoc f g h a =
  runMem (f <> (g <> h)) a
    == runMem ((f <> g) <> h) a

type MemAssoc a b = Mem a b -> Mem a b -> Mem a b -> a -> Bool

type MemId a b = Mem a b -> a -> Bool

memMonoidLeftId :: (Eq a, Monoid m, Eq m) => Mem a m -> a -> Bool
memMonoidLeftId f a = runMem (mempty <> f) a == runMem f a

memMonoidRightId :: (Eq a, Monoid m, Eq m) => Mem a m -> a -> Bool
memMonoidRightId f a = runMem (f <> mempty) a == runMem f a

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = fmap g (fmap f x) == fmap (g . f) x

fId :: [Int] -> Bool
fId x = functorIdentity x

fCompose :: [Int] -> Bool
fCompose x = functorCompose (+ 1) (* 2) x

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  fmap (g . f) x
    == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

type IntFc = [Int] -> IntToInt -> IntToInt -> Bool

-- q1
newtype IdentityF a = IdentityF a deriving (Eq, Show)

instance Functor IdentityF where
  fmap f (IdentityF a) = IdentityF (f a)

instance (Arbitrary a) => Arbitrary (IdentityF a) where
  arbitrary = do
    IdentityF <$> arbitrary

idF :: IdentityF Int -> Bool
idF x = functorIdentity x

type Q1F = IdentityF Int -> IntToInt -> IntToInt -> Bool

-- q2
data PairF a = PairF a a deriving (Eq, Show)

instance Functor PairF where
  fmap f (PairF a x) = PairF (f a) (f x)

instance (Arbitrary a) => Arbitrary (PairF a) where
  arbitrary = do
    x <- arbitrary
    PairF x <$> arbitrary

idPairF :: PairF Int -> Bool
idPairF x = functorIdentity x

type Q2F = PairF Int -> IntToInt -> IntToInt -> Bool

-- q3
data TwoF a b = TwoF a b deriving (Eq, Show)

instance Functor (TwoF a) where
  fmap f (TwoF a b) = TwoF a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (TwoF a b) where
  arbitrary = do
    x <- arbitrary
    TwoF x <$> arbitrary

idTwoF :: TwoF [Int] Int -> Bool
idTwoF x = functorIdentity x

type Q3F = TwoF [Int] Int -> IntToInt -> IntToInt -> Bool

-- q4
data ThreeF a b c = ThreeF a b c deriving (Eq, Show)

instance Functor (ThreeF a b) where
  fmap f (ThreeF a b c) = ThreeF a b (f c)

instance
  (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (ThreeF a b c)
  where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    ThreeF x y <$> arbitrary

idThreeF :: ThreeF Int [Int] [Int] -> Bool
idThreeF x = functorIdentity x

type Q4F = ThreeF Int Int Int -> IntToInt -> IntToInt -> Bool

-- q5
data ThreeF' a b = ThreeF' a b b deriving (Eq, Show)

instance Functor (ThreeF' a) where
  fmap f (ThreeF' x y z) = ThreeF' x (f y) (f z)

instance (Arbitrary a, Arbitrary b) => Arbitrary (ThreeF' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    ThreeF' x y <$> arbitrary

idThreeF' :: ThreeF' Int [Int] -> Bool
idThreeF' x = functorIdentity x

type Q5F = ThreeF' Int Int -> IntToInt -> IntToInt -> Bool

-- q6
data FourF a b c d = FourF a b c d deriving (Eq, Show)

instance Functor (FourF a b c) where
  fmap f (FourF x y z l) = FourF x y z (f l)

instance
  (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (FourF a b c d)
  where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    FourF x y z <$> arbitrary

idFourF :: FourF Int [Int] Int [Int] -> Bool
idFourF x = functorIdentity x

type Q6F = FourF Int [Int] Int Int -> IntToInt -> IntToInt -> Bool

-- q7
data FourF' a b = FourF' a a a b deriving (Eq, Show)

instance Functor (FourF' a) where
  fmap f (FourF' x1 x2 x3 x4) = FourF' x1 x2 x3 (f x4)

instance (Arbitrary a, Arbitrary b) => Arbitrary (FourF' a b) where
  arbitrary = do
    x1 <- arbitrary
    x2 <- arbitrary
    x3 <- arbitrary
    FourF' x1 x2 x3 <$> arbitrary

idFourF' :: FourF' Int [Int] -> Bool
idFourF' x = functorIdentity x

type Q7F = FourF' Int Int -> IntToInt -> IntToInt -> Bool

data Sum' b a = FirstF a | SecondF b

instance Functor (Sum' e) where
  fmap f (FirstF a) = FirstF (f a)
  fmap _ (SecondF b) = SecondF b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- f' :: Mem Integer [Char]
-- f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  -- let rmzero = runMem mempty 0
  -- rmleft = runMem (f' <> mempty) 0
  -- rmright = runMem (mempty <> f') 0
  -- print $ rmleft
  -- print $ rmright
  -- print $ (rmzero :: (String, Int))
  -- print $ rmleft == runMem f' 0
  -- print $ rmright == runMem f' 0
  tests

tests :: IO ()
tests = hspec $ do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"
  describe "digits" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]
  describe "wordNumber" $ do
    it "one-zero-zero given 100" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "nine-zero-zer-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"
  describe "half" $ do
    it "halfIdentity" $ do
      property $ \x -> halfIdentity x == (x :: Double)
  describe "sorted" $ do
    it "isSorted" $ do
      property $ \x -> listOrdered $ sort (x :: [Int])
  describe "addition with associate / commutative" $ do
    it "hasAssociative" $ do
      property $ \x y z -> plusAssociative (x :: Int) y z
    it "hasCommutative" $ do
      property $ \x y -> plusCommutative (x :: Int) y
  describe "multiplication with associative / commutative" $ do
    it "hasAssociative" $ do
      property $ \x y z -> multAssociative (x :: Int) y z
    it "hasCommutative" $ do
      property $ \x y -> multCommutative (x :: Int) y
  describe "quot / rem && div / mod" $ do
    it "quot rem" $ do
      property prop_check
    it "div mod" $ do
      property prop_check_div_mod
  describe "reverse reverse" $ do
    it "reverses twice" $ do
      property $ \x -> reverseTwice x == (x :: [Int])
  describe "$" $ do
    it "does dollar" $ do
      property testDollar
    it "does dot" $ do
      property $ \x -> (id . id) x == (\l -> id (id l)) (x :: Int)
  describe "list equal" $ do
    it "first one" $ do
      property checkConcat
  describe "roundTrip" $ do
    it "checks is so" $ do
      property roundt
  describe "idempotent" $ do
    it "checks first" $ do
      property first
  describe "check the second" $ do
    it "checks the second property" $ do
      property second
  describe "test the vigenere" $ do
    it "should go there an back again" $ do
      property testVigenere
  describe "test the Caesar" $ do
    it "should test the caesar" $ do
      property testCaesar
  describe "monoid properties" $ do
    it "firstmappend" $ do
      property (monoidAssoc :: FirstMappend)
    it "left identity" $ do
      property (monoidLeftIdentity :: FstId)
    it "right identity" $ do
      property (monoidRightIdentity :: FstId)
  describe "semigroup exercises" $ do
    it "question one" $ do
      property (semigroupAssoc :: TrivAssoc)
    it "question one lid" $ do
      property (monoidLeftIdentity :: Trivial -> Bool)
    it "question one rid" $ do
      property (monoidRightIdentity :: Trivial -> Bool)
    it "question two" $ do
      property (semigroupAssoc :: IdentityAssoc')
    it "question two lid" $ do
      property (monoidLeftIdentity :: Identity' String -> Bool)
    it "question two rid" $ do
      property (monoidRightIdentity :: Identity' String -> Bool)
    it "question three" $ do
      property (semigroupAssoc :: TwoAssoc)
    it "question three lid" $ do
      property (monoidLeftIdentity :: Two String String -> Bool)
    it "question three rid" $ do
      property (monoidRightIdentity :: Two String String -> Bool)
    it "question four" $ do
      property (semigroupAssoc :: ThreeAssoc)
    it "question five" $ do
      property (semigroupAssoc :: FourAssoc)
    it "question six" $ do
      property (semigroupAssoc :: BoolConjAssoc)
    it "question six lid" $ do
      property (monoidLeftIdentity :: BoolConj -> Bool)
    it "question six rid" $ do
      property (monoidRightIdentity :: BoolConj -> Bool)
    it "question seven" $ do
      property (semigroupAssoc :: BoolDisjAssoc)
    it "question seven lid" $ do
      property (monoidLeftIdentity :: BoolDisj -> Bool)
    it "question seven rid" $ do
      property (monoidRightIdentity :: BoolDisj -> Bool)
    it "question eight" $ do
      property (semigroupAssoc :: OrAssoc)
    it "question nine" $ do
      property
        ( \(Fn f) (Fn g) (Fn h) ->
            (combineAssoc :: CombineAssoc Int (DM.Sum Int))
              (Combine f)
              (Combine g)
              (Combine h)
        )
    it "does it again" $ do
      property (combSemigroupAssoc :: CombAssoc Float (DM.Product Int))
    it "does it again lid" $ do
      property
        ( combMonoidLeftId ::
            Combine Float (DM.Product Int) ->
            Float ->
            Bool
        )
    it "does it again rid" $ do
      property
        ( combMonoidRightId ::
            Combine Float (DM.Product Int) ->
            Float ->
            Bool
        )
    it "question ten" $ do
      property (compSemigroupAssoc :: CompAssoc (DM.Product Int))
    it "question ten lid" $ do
      property
        ( compMonoidLeftId ::
            Comp (DM.Product Int) ->
            DM.Product Int ->
            Bool
        )
    it "question ten rid" $ do
      property
        ( compMonoidRightId ::
            Comp (DM.Product Int) ->
            DM.Product Int ->
            Bool
        )
    it "question eleven" $ do
      property (semigroupAssoc :: ValidationAssoc)
    it "question twelve" $ do
      property (memorySemigroupAssoc :: MemAssoc Int String)
    it "question tweleve lid" $ do
      property (memMonoidLeftId :: MemId Int String)
    it "question twelve rid" $ do
      property (memMonoidRightId :: MemId Int String)
    it "checks the identity for functor" $ do
      property fId
    it "checks the functor compose" $ do
      property fCompose
    it "checks the compose with a new one" $ do
      property (functorCompose' :: IntFc)
    it "checks the first identity" $ do
      property idF
    it "checks the second composition" $ do
      property (functorCompose' :: Q1F)
    it "q2 id" $ do
      property idPairF
    it "q2 second composition" $ do
      property (functorCompose' :: Q2F)
    it "q3 id" $ do
      property idTwoF
    it "q3 second composition" $ do
      property (functorCompose' :: Q3F)
    it "q4 id" $ do
      property idThreeF
    it "q4 second composition" $ do
      property (functorCompose' :: Q4F)
    it "q5 id" $ do
      property idThreeF'
    it "q5 second composition" $ do
      property (functorCompose' :: Q5F)
    it "q6 id" $ do
      property idFourF
    it "q6 second composition" $ do
      property (functorCompose' :: Q6F)
    it "q7 id" $ do
      property idFourF'
    it "q7 second composition" $ do
      property (functorCompose' :: Q7F)

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
  a <- arbitrary
  b <- arbitrary
  return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, return $ First a), (1, return $ Second b)]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

data Bool' = True' | False' deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' arbitrary