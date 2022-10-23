module IPV6 where

import Control.Applicative (Alternative (empty, some, (<|>)))
import Data.Bits (Bits (shiftL, shiftR, (.|.)), (.&.))
import Data.Char (isAlpha, isDigit)
import Data.Word (Word32, Word64)
import IP
import Numeric (readHex, showHex)
import Text.Trifecta
  ( CharParsing (char),
    Parser,
    Parsing (skipMany, try),
    Result (Success),
    anyChar,
    count,
    integer,
    letter,
    manyTill,
    natural,
    noneOf,
    oneOf,
    parseString,
    satisfy,
    string,
  )

data Eighth = Eighth Integer Bool deriving (Eq, Ord, Show)

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

instance Show IPAddress6 where
  show (IPAddress6 upper lower) =
    conversionStack
      (convertToInteger upper lower)
      7
    where
      convertToInteger :: Word64 -> Word64 -> Integer
      convertToInteger first second =
        shiftL (fromIntegral first) 64
          .|. fromIntegral second
      conversionStack :: Integer -> Integer -> String
      conversionStack num 1 = case quotRem num (2 ^ 16) of
        (quot, rem) -> showHex quot "" ++ ":" ++ showHex rem ""
      conversionStack num pos = case quotRem num ((2 ^ 16) ^ pos) of
        (quot, rem) -> showHex quot "" ++ ":" ++ conversionStack rem (pos - 1)

mask :: Integer
mask = case readHex "ffffffffffffffff" of
  (x, _) : _ -> (x :: Integer)
  _ -> error "fundamental flaw in laws of base16"

parseOctet :: Parser Integer
parseOctet = do
  x <- some $ satisfy (\x -> isDigit x || isAlpha x)
  case readHex x of
    (x, _) : _ ->
      if (x :: Integer) > (2 ^ 16)
        then fail "too large octet, cannot be larger than 2 ^ 16"
        else return (x :: Integer)
    _ -> fail "unable to parse the hex digit"

parseEight :: Parser Eighth
parseEight = do
  status <- parseAbbrev <|> parseNotAbbrev
  number <- parseOctet
  return $ Eighth number status
  where
    parseAbbrev :: Parser Bool
    parseAbbrev = do
      _ <- string "::"
      return True
    parseNotAbbrev :: Parser Bool
    parseNotAbbrev = do
      _ <- char ':'
      return False

parseNormalEight :: Parser Eighth
parseNormalEight = do
  number <- parseOctet
  return $ Eighth number False

octetParser :: Parser Eighth
octetParser = do
  parseEight <|> parseNormalEight <|> fail "unexpected input"

tryChar :: Parser Bool
tryChar = do
  anyChar
  fail "unexpected text in IPV6 string"

emptyStringCase :: Parser Bool
emptyStringCase = return True

ipParser :: Parser IPAddress6
ipParser = do
  results <- some octetParser
  _ <- tryChar <|> emptyStringCase
  if isInvalidEight results
    then fail "Invalid IPV6 length/abbreviations"
    else
      return $
        IPAddress6
          (topHalf (finalDecimal results))
          (bottomHalf (finalDecimal results))
  where
    topHalf :: Integer -> Word64
    topHalf wholeNum = fromIntegral $ shiftR wholeNum 64

    bottomHalf :: Integer -> Word64
    bottomHalf wholeNum = fromIntegral $ wholeNum .&. mask

    finalDecimal :: [Eighth] -> Integer
    finalDecimal usingList = foldr go 0 $ toPromote $ indexCombiner usingList

    go :: (Eighth, Int) -> Integer -> Integer
    go (Eighth num _, pos) accum = accum + (power * num)
      where
        power :: Integer
        power = (2 ^ 16) ^ pos
    -- Promoter Functions
    toPromote :: [(Eighth, Int)] -> [(Eighth, Int)]
    toPromote list = fmap updater (updatedList list) ++ notUpdatedList list
      where
        updater :: (Eighth, Int) -> (Eighth, Int)
        updater (a, num) = (a, num + promoteSize list)

        updatedList :: [(Eighth, Int)] -> [(Eighth, Int)]
        updatedList = takeWhile go

        notUpdatedList :: [(Eighth, Int)] -> [(Eighth, Int)]
        notUpdatedList = dropWhile go

        go :: (Eighth, Int) -> Bool
        go (Eighth _ False, _) = True
        go _ = False
    promoteSize :: [a] -> Int
    promoteSize list = 8 - length list

    indexCombiner :: [Eighth] -> [(Eighth, Int)]
    indexCombiner list = zip list (reverse [0 .. (length list - 1)])
    -- Invalid List Checking
    isInvalidEight :: [Eighth] -> Bool
    isInvalidEight list =
      (length list > 8) || (numberAbbreviated list > 1)
        || ((numberAbbreviated list == 0) && (length list < 8))

    numberAbbreviated :: [Eighth] -> Int
    numberAbbreviated list = length $ filter filterFn list

    filterFn :: Eighth -> Bool
    filterFn (Eighth _ True) = True
    filterFn (Eighth _ _) = False

testFn :: IO ()
testFn = do
  print $ parseString ipParser mempty "FE80::0202:B3FF:FE1E:8329"

ipv4Mask :: Integer
ipv4Mask = case readHex "ffff" of
  (x, _) : _ -> (x :: Integer)
  _ -> error "fundamental flaw in laws of base16"

convertIP :: IPAddress -> IPAddress6
convertIP (IPAddress num) = IPAddress6 0 (fromIntegral $ finalNumber num)
  where
    finalNumber :: Word32 -> Integer
    finalNumber num = usingShift .|. fromIntegral num
    usingShift :: Integer
    usingShift = shiftL ipv4Mask 32