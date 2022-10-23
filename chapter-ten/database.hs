module Database where

import Data.Time

data DatabaseItem
  = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate
      ( UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123)
      ),
    DbNumber 9001,
    DbString "Hello, world",
    DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
  foldr
    ( \x b -> case x of
        DbDate utc -> utc : b
        _ -> b
    )
    []

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber =
  foldr
    ( \x b -> case x of
        DbNumber num -> num : b
        _ -> b
    )
    []

sumDb :: [DatabaseItem] -> Integer
sumDb =
  foldr
    ( \x b -> case x of
        DbNumber num -> num + b
        _ -> b
    )
    0

avgDb :: [DatabaseItem] -> Double
avgDb db =
  foldr
    ( \x b -> case x of
        (pos, num) -> b + (fromInteger num - b) / fromIntegral pos
    )
    0.0
    $ zip
      [1 .. length items]
      items
  where
    items = filterDbNumber db

fibs = takeWhile (< 100) $ 1 : scanl (+) 1 fibs

factorial = scanl (*) 1 [1 ..]

factorialN x = factorial !! x

test [] = 0
test (_ : xa) = 1 + test xa

nouns = ["car", "liz", "oscar", "condom"]

verbs = ["sex", "blowjob", "fucks", "orgasms"]

stopVowelStop stops vowels =
  go stops vowels stops stopSymbol stopSymbol stopSymbol []
  where
    go (sa : xsa) (v : vsa) (sb : xsb) charA charB charC list
      | isEmpty charA && isEmpty charB && isEmpty charC =
        go (sa : xsa) (v : vsa) (sb : xsb) sa charB charC list
          ++ go xsa (v : vsa) (sb : xsb) stopSymbol stopSymbol stopSymbol list
      | isEmpty charB && isEmpty charC =
        go (sa : xsa) (v : vsa) (sb : xsb) charA v stopSymbol list
          ++ go (sa : xsa) vsa (sb : xsb) charA stopSymbol stopSymbol list
      | otherwise =
        (charA, charB, sb) :
        go (sa : xsa) (v : vsa) xsb charA charB stopSymbol list
    go [] _ _ _ _ _ list = list
    go _ [] _ _ _ _ list = list
    go _ _ [] _ _ _ list = list
    isEmpty = (==) stopSymbol
    stopSymbol = ""

stopVowelSimple stops vowels = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFunc' x = sum (map (fromIntegral . length) (words x)) / fromIntegral (length (words x))