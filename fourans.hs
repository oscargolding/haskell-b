module Answers where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f ta tb = ((snd ta, snd tb), (fst ta, fst tb))

x = (+)

func xs = w `x` 1 where w = length xs

identity x = x

f' (a, b) = a