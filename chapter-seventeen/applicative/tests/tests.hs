module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend = (<>)

instance EqProp Bull where (=-=) = eq

-- List exercises
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap fn list = fold (\x b -> append (Cons (fn x) Nil) b) Nil list

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  ops <*> list = flatMap (`fmap` list) ops

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    frequency [(1, return Nil), (2, Cons <$> arbitrary <*> arbitrary)]

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Eq a => EqProp (List a) where
  (=-=) = eq

take' :: Int -> List a -> List a
take' = go 0
  where
    go current limit (Cons x xs)
      | current >= limit = Nil
      | otherwise = Cons x (go (current + 1) limit xs)
    go _ _ Nil = Nil

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap fn list = concat' $ fn <$> list

-- now perform the ziplist one
newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 3000 l
      ys' =
        let (ZipList' l) = ys
         in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

zipCons :: List (a -> b) -> List a -> List b
zipCons Nil _ = Nil
zipCons _ Nil = Nil
zipCons (Cons f fs) (Cons x xs) = Cons (f x) (zipCons fs xs)

instance Applicative ZipList' where
  pure a = ZipList' as
    where
      as = Cons a as
  ZipList' Nil <*> _ = ZipList' Nil
  _ <*> ZipList' Nil = ZipList' Nil
  ZipList' fList <*> ZipList' yList = ZipList' $ zipCons fList yList

data Validation e a = Failure' e | Success' a deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Validation a b) where
  (=-=) = eq

-- same as Either
instance Functor (Validation e) where
  fmap _ (Failure' x) = Failure' x
  fmap fn (Success' x) = Success' (fn x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Failure' x), (1, return $ Success' y)]

instance Monoid e => Applicative (Validation e) where
  pure x = Success' x
  (Success' fn) <*> (Success' x) = Success' (fn x)
  (Success' _) <*> (Failure' y) = Failure' y
  (Failure' x) <*> (Success' _) = Failure' x
  (Failure' x1) <*> (Failure' x2) = Failure' (x1 <> x2)

-- q1
data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
  fmap fn (Pair x1 x2) = Pair (fn x1) (fn x2)

instance Applicative Pair where
  pure x = Pair x x
  (Pair fn1 fn2) <*> (Pair x1 x2) = Pair (fn1 x1) (fn2 x2)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    x <- arbitrary
    Pair x <$> arbitrary

instance (Eq a) => EqProp (Pair a) where
  (=-=) = eq

-- q2
data Two a b = Two a b deriving (Show, Eq)

instance Functor (Two a) where
  fmap fn (Two x1 x2) = Two x1 (fn x2)

instance (Monoid b) => Applicative (Two b) where
  pure x = Two mempty x
  (Two l fn) <*> (Two r x) = Two (l <> r) (fn x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    Two x <$> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- q3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x1 x2 x3) = Three x1 x2 (f x3)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three l1 l2 fn) <*> (Three r1 r2 x) = Three (l1 <> r1) (l2 <> r2) (fn x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    Three x y <$> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- q4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x1 x2 x3) = Three' x1 (f x2) (f x3)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' l1 f1 f2) <*> (Three' l2 x1 x2) = Three' (l1 <> l2) (f1 x1) (f2 x2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    Three' x y <$> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- q5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four x1 x2 x3 x4) = Four x1 x2 x3 (f x4)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (Four l1 l2 l3 fn) <*> (Four r1 r2 r3 x) =
    Four
      (l1 <> r1)
      (l2 <> r2)
      (l3 <> r3)
      (fn x)

instance
  (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (Four a b c d)
  where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    Four x y z <$> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- q6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' x1 x2 x3 x4) = Four' x1 x2 x3 (f x4)

instance (Monoid a) => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (Four' l1 l2 l3 fn) <*> (Four' r1 r2 r3 x) =
    Four'
      (l1 <> r1)
      (l2 <> r2)
      (l3 <> r3)
      (fn x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    Four' x y z <$> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

main :: IO ()
main = hspec $ do
  describe "List" $ do
    it "Functor" $ do
      property $
        quickBatch
          (functor (undefined :: List (Int, Float, String)))
    it "applicative" $ do
      property $
        quickBatch
          (applicative (undefined :: List (Int, Float, String)))
  describe "ZipList'" $ do
    it "Functor" $ do
      property $
        quickBatch
          (functor (undefined :: ZipList' (Int, Float, String)))
    it "applicative" $ do
      property $
        quickBatch
          (applicative (undefined :: ZipList' (Int, Float, String)))
  describe "Validation" $ do
    it "Functor" $ do
      property $
        quickBatch
          (functor (undefined :: Validation String (Int, Float, String)))
    it "Applicative" $ do
      property $
        quickBatch
          (applicative (undefined :: Validation String (Int, Float, String)))
  describe "Pair" $ do
    it "Functor" $ do
      property $
        quickBatch
          (functor (undefined :: Pair (Int, Float, String)))
    it "Applicative" $ do
      property $
        quickBatch
          (applicative (undefined :: Pair (Int, Float, String)))
  describe "Two" $ do
    it "Functor" $ do
      property $
        quickBatch
          (functor (undefined :: Two String (Int, Float, String)))
    it "Applicative" $ do
      property $
        quickBatch
          (applicative (undefined :: Two String (Int, Float, String)))
  describe "Three" $ do
    it "Functor" $ do
      property $
        quickBatch
          ( functor
              ( undefined ::
                  Three String String (Int, Float, String)
              )
          )
    it "Applicative" $ do
      property $
        quickBatch
          ( applicative
              (undefined :: Three String String (Int, Float, String))
          )
  describe "Three'" $ do
    it "Functor" $ do
      property $
        quickBatch
          (functor (undefined :: Three' String (Int, Float, String)))
    it "Applicative" $ do
      property $
        quickBatch
          ( applicative
              (undefined :: Three' String (Int, Float, String))
          )
  describe "Four" $ do
    it "Functor" $ do
      property $
        quickBatch
          ( functor
              (undefined :: Four String String String (Int, Float, String))
          )
    it "Applicative" $ do
      property $
        quickBatch
          ( applicative
              (undefined :: Four String String String (Int, Float, String))
          )
  describe "Four'" $ do
    it "Functor" $ do
      property $
        quickBatch (functor (undefined :: Four' String (Int, Float, String)))
    it "Applicative" $ do
      property $
        quickBatch (applicative (undefined :: Four' String (Int, Float, String)))