{-# LANGUAGE FlexibleContexts #-}

module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- Constant
newtype Constant' a b = Constant' {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant' a) where
  fmap _ (Constant' c) = Constant' c

instance Foldable (Constant' a) where
  foldMap _ (Constant' _) = mempty

instance Traversable (Constant' a) where
  traverse _ (Constant' x) = pure (Constant' x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant' a b) where
  arbitrary = Constant' <$> arbitrary

instance Eq a => EqProp (Constant' a b) where
  (=-=) = eq

-- Maybe
data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep x) = Yep $ f x

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep x) = f x

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep x) = Yep <$> f x

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return Nada), (1, return $ Yep x)]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

append' :: List a -> List a -> List a
append' ys Nil = ys
append' ys (Cons x xs) = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap _ Nil = Nil
flatMap fn list = concat' $ fn <$> list

instance Functor List where
  fmap _ Nil = Nil
  fmap fn list = fold (\x b -> append (Cons (fn x) Nil) b) Nil list

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap fn (Cons x xs) = fn x <> foldMap fn xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse fn (Cons item xs) =
    fmap (\x -> append (Cons x Nil)) (fn item) <*> traverse fn xs

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    frequency
      [ (1, return Nil),
        (5, Cons <$> arbitrary <*> arbitrary)
      ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- Three
data Three a b c = Three a b c deriving (Eq, Show, Ord)

instance Functor (Three a b) where
  fmap f (Three x1 x2 x3) = Three x1 x2 (f x3)

instance Foldable (Three a b) where
  foldMap fn (Three _ _ x) = fn x

instance Traversable (Three a b) where
  traverse fn (Three x1 x2 x3) = Three x1 x2 <$> fn x3

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    Three x y <$> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Pair
data Pair a b = Pair a b deriving (Eq, Show, Ord)

instance Functor (Pair a) where
  fmap f (Pair x1 x2) = Pair x1 (f x2)

instance Foldable (Pair a) where
  foldMap fn (Pair _ x) = fn x

instance Traversable (Pair a) where
  traverse fn (Pair x1 x2) = Pair x1 <$> fn x2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    x <- arbitrary
    Pair x <$> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- Big
data Big a b = Big a b b deriving (Eq, Show, Ord)

instance Functor (Big a) where
  fmap f (Big x1 x2 x3) = Big x1 (f x2) (f x3)

instance Foldable (Big a) where
  foldMap fn (Big _ x1 x2) = fn x1 <> fn x2

instance Traversable (Big a) where
  traverse fn (Big x1 x2 x3) = (Big x1 <$> fn x2) <*> fn x3

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    Big x y <$> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- Bigger
data Bigger a b = Bigger a b b b deriving (Eq, Show, Ord)

instance Functor (Bigger a) where
  fmap f (Bigger x1 x2 x3 x4) = Bigger x1 (f x2) (f x3) (f x4)

instance Foldable (Bigger a) where
  foldMap fn (Bigger _ x1 x2 x3) = fn x1 <> fn x2 <> fn x3

instance Traversable (Bigger a) where
  traverse fn (Bigger x1 x2 x3 x4) =
    (Bigger x1 <$> fn x2) <*> fn x3 <*> fn x4

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    Bigger x y z <$> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- S
data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance
  (Applicative n, Testable (n Property), Eq a, Eq (n a), EqProp a) =>
  EqProp (S n a)
  where
  (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S fa a) = S (f <$> fa) (f a)

instance Foldable n => Foldable (S n) where
  foldMap fn (S x1 x2) = foldMap fn x1 <> fn x2

instance Traversable n => Traversable (S n) where
  traverse fn (S x1 x2) = (S <$> traverse fn x1) <*> fn x2

-- Tree
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap fn (Leaf x) = Leaf (fn x)
  fmap fn (Node l1 x r1) = Node (fmap fn l1) (fn x) (fmap fn r1)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap fn (Leaf x) = fn x
  foldMap fn (Node l1 x r1) = foldMap fn l1 <> fn x <> foldMap fn r1

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse fn (Leaf x) = fmap Leaf (fn x)
  traverse fn (Node l1 x r1) = Node <$> traverse fn l1 <*> fn x <*> traverse fn r1

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency
      [ (2, return Empty),
        (4, Leaf <$> arbitrary),
        (2, Node <$> arbitrary <*> arbitrary <*> arbitrary)
      ]

instance (Eq a) => EqProp (Tree a) where
  (=-=) = eq

main :: IO ()
main = hspec $ do
  describe "Identity" $ do
    it "Functor" $ do
      property $ quickBatch (functor (undefined :: Identity (Int, Float, String)))
    it "Foldable" $ do
      property $ quickBatch (foldable (undefined :: Identity (Int, Float, String, Int, String)))
    it "Traversable" $ do
      property $ quickBatch (traversable (undefined :: Identity (Int, Float, String)))
  describe "Constant'" $ do
    it "Functor" $ do
      property $ quickBatch (functor (undefined :: Constant' String (Int, Float, String)))
    it "Foldable" $ do
      property $
        quickBatch
          ( foldable
              ( undefined ::
                  Constant'
                    String
                    ( Int,
                      Float,
                      String,
                      Int,
                      String
                    )
              )
          )
    it "Traversable" $ do
      property $
        quickBatch
          ( traversable
              (undefined :: Constant' String (Int, Float, String))
          )
  describe "Maybe" $ do
    it "Functor" $ do
      property $
        quickBatch
          ( functor
              (undefined :: Optional (Int, Float, String))
          )
    it "Foldable" $ do
      property $
        quickBatch
          ( foldable
              (undefined :: Optional (Int, Float, String, Int, Float))
          )
    it "Traversable" $ do
      property $
        quickBatch
          ( traversable
              (undefined :: Optional (Int, Float, String))
          )
  describe "List" $ do
    it "Functor" $ do
      property $
        quickBatch
          (functor (undefined :: List (Int, Float, String)))
    it "Foldable" $ do
      property $
        quickBatch
          (foldable (undefined :: List (Int, Float, String, Int, String)))
    it "Traversable" $
      property
        (quickBatch $ traversable (undefined :: List (Int, Float, String)))
  describe "Three" $ do
    it "Functor" $ do
      property $
        quickBatch
          ( functor
              (undefined :: Three Int String (Int, Float, String))
          )
    it "Foldable" $ do
      property $
        quickBatch
          ( foldable
              (undefined :: Three Int String (Int, Float, String, Int, String))
          )
    it "Traversable" $ do
      property $
        quickBatch
          ( traversable
              (undefined :: Three Int String (Int, Float, String))
          )
  describe "Pair" $ do
    it "Functor" $ do
      property $
        quickBatch
          ( functor
              (undefined :: Pair String (Int, Float, String))
          )
    it "Foldable" $ do
      property $
        quickBatch
          ( foldable
              (undefined :: Pair String (Int, Float, String, Int, String))
          )
    it "Traversable" $ do
      property $
        quickBatch
          ( traversable
              (undefined :: Pair String (Int, Float, String))
          )
  describe "Big" $ do
    it "Functor" $ do
      property $
        quickBatch (functor (undefined :: Big Int (Int, String, Float)))
    it "Foldable" $ do
      property $
        quickBatch
          (foldable (undefined :: Big Int (Int, Float, String, Int, String)))
    it "Traversable" $ do
      property $
        quickBatch
          (traversable (undefined :: Big Int (Int, Float, String)))
  describe "Bigger" $ do
    it "Functor" $ do
      property $
        quickBatch (functor (undefined :: Bigger Int (Int, String, Float)))
    it "Foldable" $ do
      property $
        quickBatch
          (foldable (undefined :: Bigger Int (Int, Float, String, Int, String)))
    it "Traversable" $ do
      property $
        quickBatch
          (traversable (undefined :: Bigger Int (Int, Float, String)))
  describe "S" $ do
    it "Functor" $ do
      property $
        quickBatch $ functor (undefined :: S [] (Int, Float, String))
    it "Foldable" $ do
      property $
        quickBatch $
          foldable
            ( undefined ::
                S
                  []
                  (Int, Float, String, Int, String)
            )
    it "Traversable" $ do
      property $
        quickBatch $
          traversable (undefined :: S [] (Int, Float, String))
  describe "Tree" $ do
    it "Functor" $ do
      property $
        quickBatch $ functor (undefined :: Tree (Int, Float, String))
    it "Foldable" $ do
      property $
        quickBatch
          ( foldable
              (undefined :: Tree (Int, Float, String, Int, String))
          )
    it "Traversable" $ do
      property $
        quickBatch
          (traversable (undefined :: Tree (Int, Float, String)))