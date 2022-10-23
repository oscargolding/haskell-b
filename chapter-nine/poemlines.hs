module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines string = generic string '\n'

myWords string = generic string ' '

generic :: String -> Char -> [String]
generic string char = go char string []
  where
    go char string list
      | takeUntilChar string == string = list ++ [string]
      | otherwise = go char (nextPart string) (list ++ [takeUntilChar string])
    dropUntilChar = dropWhile (/= char)
    takeUntilChar = takeWhile (/= char)
    nextPart = dropWhile (== char) . dropUntilChar

shouldEqual =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)