module Answers where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    xLast = fst $ divMod x 10
    d = snd $ divMod xLast 10

hunsD :: Integral a => a -> a
hunsD x = d2
  where
    (xLast, _) = divMod x 100
    (_, d2) = divMod xLast 10

foldBool :: a -> a -> Bool -> a
foldBool x y z
  | not z = x
  | otherwise = y

foldBoolCase :: a -> a -> Bool -> a
foldBoolCase x y z = case not z of
  True -> x
  False -> y

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
