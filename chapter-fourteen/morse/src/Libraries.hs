-- src/Libraries
module Libraries
  ( digitToWord,
    digits,
    wordNumber,
    encrypt,
    decrypt,
    encryptCaesar,
    decryptCaesar,
  )
where

import Data.Char
import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"
  | otherwise = ""

digits :: Int -> [Int]
digits n = go n []
  where
    go na list
      | div na 10 == 0 = na : list
      | otherwise = go (div na 10) $ mod na 10 : list

wordNumber :: Int -> String
wordNumber n = (intercalate "-" . map digitToWord) $ digits n

vigenere :: String -> String -> (Int -> Int -> Int) -> String
vigenere string keyword fn = go string keyword keyword
  where
    go str key [] = go str key key
    go [] _ _ = []
    go (x : xs) key (y : ys)
      | isLower x = chr (perform x 97 $ cipher y) : go xs key ys
      | isUpper x = chr (perform x 65 $ cipher y) : go xs key ys
      | otherwise = x : go xs keyword (y : ys)
    cipher char = if isUpper char then ord char - 65 else ord char - 97
    perform x loc rotation = loc + shift x loc rotation
    shift x loc shif = mod (rotate x loc shif) 26
    rotate x loc = fn (base x loc)
    base x loc = ord x - loc

encrypt :: String -> String -> String
encrypt word [] = word
encrypt word cipher = vigenere word cipher (+)

decrypt :: String -> String -> String
decrypt word [] = word
decrypt word cipher = vigenere word cipher (-)

caesarCipher :: String -> Int -> (Int -> Int -> Int) -> String
caesarCipher [] _ _ = []
caesarCipher (x : xs) rot fn
  | isLower x = chr (perform x 97 rot) : caesarCipher xs rot fn
  | isUpper x = chr (perform x 65 rot) : caesarCipher xs rot fn
  | otherwise = x : caesarCipher xs rot fn
  where
    perform char loc rotation = loc + shift char loc rotation
    shift char loc shi = mod (rotate char loc shi) 26
    rotate char loc = fn (base char loc)
    base char loc = ord char - loc

encryptCaesar :: String -> Int -> String
encryptCaesar word rot = caesarCipher word rot (+)

decryptCaesar :: String -> Int -> String
decryptCaesar word rot = caesarCipher word rot (-)