module Test where

import Data.Monoid

newtype Identity a = Identity a

data Optional a = Nada | Yep a

-- The identity class
instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

-- An optional class
instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

sum' :: (Foldable t, Num a) => t a -> a
sum' structure = getSum $ foldMap Sum structure

product' :: (Foldable t, Num a) => t a -> a
product' structure = getProduct $ foldMap Product structure

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' find structure = getAny $ foldMap (Any . (== find)) structure

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' =
  foldr
    ( \el b -> case b of
        Nothing -> Just el
        Just x -> Just $ min x el
    )
    Nothing

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' =
  foldr
    ( \el b -> case b of
        Nothing -> Just el
        Just x -> Just $ max x el
    )
    Nothing

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ b -> b + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldMap pure

-- Combine the elements as appropriate of a structure as monoid
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' aToM = foldr (\a m -> m <> aToM a) mempty

data Constant a b = Constant b

instance Foldable (Constant a) where
  foldr fn z (Constant x) = fn x z
  foldl fn z (Constant x) = fn z x
  foldMap fn (Constant x) = fn x

data Two a b = Two a b

instance Foldable (Two a) where
  foldr fn z (Two _ x) = fn x z
  foldl fn z (Two _ x) = fn z x
  foldMap fn (Two _ x) = fn x

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ x) = f x z
  foldl f z (Three _ _ x) = f z x
  foldMap fn (Three _ _ x) = fn x

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap fn (Three' _ x y) = fn x <> fn y

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap fn (Four' _ x y z) = fn x <> fn y <> fn z

filterF ::
  (Applicative f, Foldable t, Monoid (f a)) =>
  (a -> Bool) ->
  t a ->
  f a
filterF fn = foldMap (\a -> if fn a then pure a else mempty)