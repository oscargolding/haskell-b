module IP where

import Data.Word (Word32)
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
  )

newtype IPAddress = IPAddress Word32 deriving (Eq, Ord)

instance Show IPAddress where
  show (IPAddress word) = firstPart (fromIntegral word)
    where
      firstPart :: Integer -> String
      firstPart num = case quotRem num (256 ^ 3) of
        (quot, rem) -> show quot ++ "." ++ secondPart rem
      secondPart :: Integer -> String
      secondPart num = case quotRem num (256 ^ 2) of
        (quot, rem) -> show quot ++ "." ++ thirdPart rem
      thirdPart :: Integer -> String
      thirdPart num = case quotRem num 256 of
        (quot, rem) -> show quot ++ "." ++ show rem

exampleIPA :: String
exampleIPA = "172.16.254.1"

exampleIPB :: String
exampleIPB = "204.120.0.15"

failed :: String
failed = "381.123.234.12"

parseSpecificBase :: Parser Integer
parseSpecificBase = do
  x <- natural
  if x > 256 then fail "not correct subnet" else return x

convertIP :: Parser IPAddress
convertIP = do
  one <- parseSpecificBase
  _ <- char '.'
  two <- parseSpecificBase
  _ <- char '.'
  three <- parseSpecificBase
  _ <- char '.'
  IPAddress . conversion one two three <$> parseSpecificBase
  where
    conversion :: Integer -> Integer -> Integer -> Integer -> Word32
    conversion a b c d =
      specificConvert a 3 + specificConvert b 2
        + specificConvert c 1
        + specificConvert d 0

    specificConvert :: Integer -> Integer -> Word32
    specificConvert a pow = fromIntegral a * (256 ^ pow)
