module Main where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Show, Eq)

main :: IO ()
main = hspec $ do
  describe "Nope" $ do
    it "Functor" $ do
      property $
        quickBatch (functor (undefined :: Nope (Int, Float, String)))
    it "Applicative" $ do
      property $
        quickBatch (applicative (undefined :: Nope (Int, Float, String)))
    it "Monad" $ do
      property $
        quickBatch (monad (undefined :: Nope (Int, Float, String)))
  describe "PhhhbbtttEither" $ do
    it "Functor" $ do
      property $
        quickBatch
          ( functor
              (undefined :: PhhhbbtttEither String (Int, Float, String))
          )
    it "Applicative" $ do
      property $
        quickBatch
          ( applicative
              (undefined :: PhhhbbtttEither String (Int, Float, String))
          )
    it "Monad" $ do
      property $
        quickBatch
          (monad (undefined :: PhhhbbtttEither String (Int, Float, String)))
  describe "Identity" $ do
    it "Functor" $ do
      property $
        quickBatch (functor (undefined :: Identity (Int, Float, String)))
    it "Applicative" $ do
      property $
        quickBatch
          ( applicative
              (undefined :: Identity (Int, Float, String))
          )
    it "Monad" $ do
      property $
        quickBatch (monad (undefined :: Identity (Int, Float, String)))
  describe "List" $ do
    it "Functor" $ do
      property $
        quickBatch (functor (undefined :: List (Int, Float, String)))
    it "Applicative" $ do
      property $
        quickBatch (applicative (undefined :: List (Int, Float, String)))
    it "Monad" $ do
      property $
        quickBatch (monad (undefined :: List (Int, Float, String)))

-- q1
instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary =
    return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- q2
data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Right' x) = Right' x
  fmap f (Left' x) = Left' (f x)

instance Applicative (PhhhbbtttEither b) where
  pure x = Left' x
  (Left' f) <*> (Left' x) = Left' (f x)
  (Right' x) <*> _ = Right' x
  (Left' _) <*> (Right' x) = Right' x

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Right' x) >>= _ = Right' x
  (Left' x) >>= f = f x

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Left' x), (1, return $ Right' y)]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

-- q3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure x = Identity x
  (Identity x) <*> (Identity l) = Identity $ x l

instance Monad Identity where
  return = pure
  (Identity x) >>= f = f x

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- q4
data List a = Nil | Cons a (List a) deriving (Eq, Show)

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

instance Functor List where
  fmap _ Nil = Nil
  fmap fn list = fold (\x b -> append (Cons (fn x) Nil) b) Nil list

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  ops <*> list = flatMap (`fmap` list) ops

instance Monad List where
  return = pure
  list >>= f = flatMap f list

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    frequency [(1, return Nil), (2, Cons <$> arbitrary <*> arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

---- write the following using the methods provided by Monad and Functor
j :: Monad m => m (m a) -> m a
j arg = arg >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 fn arg = fn <$> arg

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 fn ma mb =
  ma
    >>= \a ->
      mb
        >>= \b ->
          return $ fn a b

a :: Monad m => m a -> m (a -> b) -> m b
a ma mfn = ma >>= \x -> mfn >>= \fn -> return $ fn x

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x : xs) fn = meh xs fn >>= \l -> fn x >>= \b -> return $ l ++ [b]

flipType :: (Monad m) => [m a] -> m [a]
flipType list = meh list id