module Reverse where

rvrs :: String -> String
rvrs x = (++) first $ second ++ third
  where
    first = drop 9 x
    middle = take 9 x
    second = drop 5 middle
    third = take 5 middle

main :: IO ()
main = print $ rvrs "Curry is awesome"