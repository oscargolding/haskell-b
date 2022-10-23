module Functor where

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)

data WhoCares a = ItDoesnt | Matter a | WhatThisIsCalled deriving (Eq, Show)

-- The identity law must be followed
instance Functor WhoCares where
  fmap _ ItDoesnt = ItDoesnt
  fmap _ WhatThisIsCalled = WhatThisIsCalled
  fmap f (Matter a) = Matter (f a)

-- Totes cool
data CountingGood a = Heisenberg Int a deriving (Eq, Show)

instance Functor CountingGood where
  fmap f (Heisenberg n a) = Heisenberg n (f a)

-- data Maybe a = Nothing | Just a
