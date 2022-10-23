module Nonsense where

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + nonsense b

uncurriedFuncion :: (Integer, Bool) -> Integer
uncurriedFuncion (i, b) = i + nonsense b

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + nonsense b

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)

hypothetical :: a -> a -> a
hypothetical fist second = second

newh :: a -> Int -> Int
newh first second = second + 5