module Arith4 where

roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read (show a)

pfRoundTrip :: (Show a, Read a) => a -> a
pfRoundTrip = read . show

comp :: (b -> c) -> ((a -> b) -> (a -> c))
comp f g x = f (g x)

main = do
  print (roundTrip 4 :: Int)
  print (pfRoundTrip 4)
  print (id 4)