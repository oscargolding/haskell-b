-- part2.hs

module Chapter3 where

addEnd x = x ++ "!"

fourth x = [x !! 4]

dropNine = drop 9

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs x = (++) first $ second ++ third
  where
    first = drop 9 x
    middle = take 9 x
    second = drop 5 middle
    third = take 5 middle
