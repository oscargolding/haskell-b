module Vigenere where

import Data.Char
import System.IO

vigenere :: String -> String -> (Int -> Int -> Int) -> String
vigenere string keyword fn = go string keyword keyword
  where
    go string keyword [] = go string keyword keyword
    go [] keyword running = []
    go (x : xs) keyword (y : ys)
      | isLower x = chr (perform x 97 $ cipher y) : go xs keyword ys
      | isUpper x = chr (perform x 65 $ cipher y) : go xs keyword ys
      | otherwise = x : go xs keyword (y : ys)
    cipher char = if isUpper char then ord char - 65 else ord char - 97
    perform x loc rotation = loc + shift x loc rotation
    shift x loc shift = mod (rotate x loc shift) 26
    rotate x loc = fn (base x loc)
    base x loc = ord x - loc

encrypt :: String -> String -> String
encrypt word cipher = vigenere word cipher (+)

decrypt :: String -> String -> String
decrypt word cipher = vigenere word cipher (-)

runCipher :: IO ()
runCipher = do
  hSetBuffering stdout NoBuffering
  putStrLn "Please input the cipher text: "
  text <- getLine
  putStrLn "encrypt or decrypt: "
  decision <- getLine
  putStrLn "Keyword: "
  keyword <- getLine
  case decision of
    "encrypt" -> putStrLn $ encrypt text keyword
    _ -> putStrLn $ decrypt text keyword
