{-# LANGUAGE LambdaCase #-}

module Test where

import Data.List
import qualified Data.Maybe

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing

type Name = String

type Age = Integer

data Person = Person Name Age deriving (Show)

-- An example of a smart constructor
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

id' :: a -> a
id' = undefined

notThe :: String -> Maybe String
notThe word = if word == "the" then Nothing else Just word

replaceThe' :: String -> String
replaceThe' =
  strip
    . foldr
      ( (\x b -> x ++ " " ++ b) . Data.Maybe.fromMaybe "a" . notThe
      )
      ""
    . words

strip [] = []
strip [x] = []
strip (x : xs) = x : strip xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel word = go False (words word)
  where
    go isPrevThe (x : xs)
      | isPrevThe && isVowel x = 1 + go False xs
      | x == "the" = 0 + go True xs
      | otherwise = 0 + go False xs
    go _ [] = 0
    isVowel foundWord = head foundWord `elem` "aeiou"

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter isVowel

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord string =
  if consonants string < countVowels string
    then Just $ Word' string
    else Nothing
  where
    consonants x = fromIntegral (length x) - countVowels x

-- As natural as any
-- competitive bodybuilder
data Nat = Zero | Suc Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Suc x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ go x
  where
    go 0 = Zero
    go num = Suc $ go (num - 1)

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee start fn (Just x) = fn x
mayybee b _ Nothing = b

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just x) = x

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a = mayybee a id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes =
  map
    ( \(Just x) -> x
    )
    . filter
      ( \case
          Just x -> True
          _ -> False
      )

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe list =
  if any
    ( \case
        Nothing -> True
        _ -> False
    )
    list
    then Nothing
    else Just (catMaybes list)

lefts' :: [Either a b] -> [a]
lefts' =
  foldr
    ( \x b -> case x of
        Left l -> b ++ [l]
        _ -> b
    )
    []

rights' :: [Either a b] -> [b]
rights' =
  foldr
    ( \x b -> case x of
        Right l -> b ++ [l]
        _ -> b
    )
    []

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' list = (lefts' list, rights' list)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' fn (Right x) = Just $ fn x

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fn _ (Left x) = fn x
either' _ fn (Right x) = fn x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' fn (Right x) = Just $ either' id fn (Right x)

eitherMaybe''' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe''' fn = either' (const Nothing) (Just . fn)

myIterate :: (a -> a) -> a -> [a]
myIterate fn a = res : myIterate fn res
  where
    res = fn a

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr fn b = case fn b of
  Nothing -> []
  Just (x, l) -> x : myUnfoldr fn l

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfoldT :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldT fn a = case fn a of
  Nothing -> Leaf
  Just (first', second, first'') ->
    Node (unfoldT fn first') second (unfoldT fn first'')

treeBuild :: Integer -> BinaryTree Integer
treeBuild limit =
  unfoldT
    ( \b -> if b == limit then Nothing else Just (b + 1, b, b + 1)
    )
    0