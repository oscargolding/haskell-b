{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Log where

import Control.Applicative (Alternative (some, (<|>)))
import Data.Char (digitToInt, isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Map.Strict as Map
import Data.Monoid ()
import Data.Tuple (swap)
import Text.Parser.Char (CharParsing (string), digit)
import Text.RawString.QQ (r)
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

type Activity = String

data HourTime = HourTime Integer Integer deriving (Show)

type Year = Integer

type Month = Integer

type Day = Integer

data LogDate = LogDate Year Month Day deriving (Eq, Show, Ord)

type ActivityItem = (HourTime, Activity)

type RecordedItem a = (a, Activity)

type DayEvents a = Map.Map LogDate [RecordedItem a]

newtype LogFormat a = LogFormat (DayEvents a) deriving (Show)

-- Example Data to Use\
exampleComment :: String
exampleComment =
  [r|

-- this is a comment

|]

exampleHeader :: String
exampleHeader =
  [r|# 2025-02-07 -- dates not necessarily sequential
|]

exampleLineItem :: String
exampleLineItem = "09:00 Bumped head, pas-sed out -- should I try skippin?"

exampleBody :: String
exampleBody =
  [r|08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, pas-sed out
13:36 Wake up, headache
22:00 Sleep
|]

exampleBodyItems :: String
exampleBodyItems =
  [r|# 2025-02-05 -- what is this comment?
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
13:00 Breakfast -- should I try skippin bfast?
|]

newExample :: String
newExample =
  [r|08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
13:00 Breakfast -- should I try skippin bfast?
|]

finalExample :: String
finalExample =
  [r|

-- whee a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
10:17 R&R
14:00 Sleep
17:30 R&R
22:00 Sleep

# 2025-02-07 -- dates not necessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
12:30 Exercising in high-grav gym
22:00 Sleep
|]

-- Parsing Functions
skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments =
  skipMany
    ( do
        _ <- string "--"
        skipMany (noneOf "\n")
        skipEOL
    )

skipWhitspace :: Parser ()
skipWhitspace = skipMany (char ' ' <|> char '\n')

parseHeader :: Parser LogDate
parseHeader = do
  _ <- char '#'
  skipWhitspace
  year <- integer
  _ <- char '-'
  month <- integer
  _ <- char '-'
  day <- integer
  skipWhitspace
  skipComments
  return $ LogDate year month day

parseLineItem :: Parser ActivityItem
parseLineItem = do
  hour <- integer
  _ <- char ':'
  minute <- integer
  val <- try (manyTill (noneOf "\n") (string "--")) <|> getAllUntilEOL
  skipMany (noneOf "\n")
  skipEOL
  return (HourTime hour minute, trim val)
  where
    getAllUntilEOL :: Parser String
    getAllUntilEOL = do
      some (noneOf "\n")

    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace

parseBodyItems :: Parser [ActivityItem]
parseBodyItems = do
  some parseLineItem

parseTimeBlock :: Parser (LogDate, [ActivityItem])
parseTimeBlock = do
  header <- parseHeader
  bodyItems <- parseBodyItems
  skipWhitspace
  skipComments
  return (header, bodyItems)

parseFile :: Parser (LogFormat Integer)
parseFile = do
  skipWhitspace
  skipComments
  blockList <- some parseTimeBlock
  return (LogFormat $ mapToActivityTime $ mapList blockList)
  where
    mapList :: [(LogDate, [ActivityItem])] -> Map.Map LogDate [ActivityItem]
    mapList list = Map.fromList list

    mapToActivityTime ::
      Map.Map LogDate [ActivityItem] ->
      Map.Map LogDate [RecordedItem Integer]
    mapToActivityTime = Map.map mapDayList

    stripListAndReturn :: [ActivityItem] -> [ActivityItem]
    stripListAndReturn (_ : end) = end ++ [(HourTime 24 00, "EndDay")]
    stripListAndReturn [] = error "serious fail in parsing occureed"

    mapDayList :: [ActivityItem] -> [RecordedItem Integer]
    mapDayList ogList =
      zipWith
        (curry getTimeDifference)
        ogList
        (stripListAndReturn ogList)

    getTimeDifference :: (ActivityItem, ActivityItem) -> RecordedItem Integer
    getTimeDifference
      ( (HourTime start' end', activity),
        (HourTime start end, _)
        ) =
        (((start - start') * 60) + (end - end'), activity)

-- The average time spent per activity per day
averagePerDay :: Result (LogFormat Integer) -> Maybe (LogFormat Float)
averagePerDay (Success (LogFormat logFormat)) =
  Just $
    LogFormat $ Map.map goOne logFormat
  where
    goOne :: [RecordedItem Integer] -> [RecordedItem Float]
    goOne items = mapFloatToList $ mapToFloat $ mapTransform items

    mapFloatToList :: Map.Map Activity Float -> [RecordedItem Float]
    mapFloatToList a = map swap $ Map.toAscList a

    mapToFloat :: Map.Map Activity (Integer, Integer) -> Map.Map Activity Float
    mapToFloat map_item = Map.map tupleToFloat map_item

    tupleToFloat :: (Integer, Integer) -> Float
    tupleToFloat (a, b) = fromIntegral a / fromIntegral b

    mapTransform ::
      [RecordedItem Integer] ->
      Map.Map Activity (Integer, Integer)
    mapTransform = foldr go Map.empty

    go ::
      RecordedItem Integer ->
      Map.Map Activity (Integer, Integer) ->
      Map.Map Activity (Integer, Integer)
    go (time, activity) map_item =
      Map.insertWith
        insertFn
        activity
        (time, 1)
        map_item

    insertFn :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
    insertFn (number, one) (sum, count) = (number + sum, one + count)
averagePerDay _ = Nothing

activityTime :: Result (LogFormat Integer) -> Maybe [RecordedItem Integer]
activityTime (Success (LogFormat logFormat)) = Just $ map swap $ Map.toList $ finalMap logFormat
  where
    finalMap :: DayEvents Integer -> Map.Map Activity Integer
    finalMap = Map.foldr listIntoMap Map.empty

    listIntoMap ::
      [RecordedItem Integer] ->
      Map.Map Activity Integer ->
      Map.Map Activity Integer
    listIntoMap list existingMap = foldr go existingMap list

    go :: RecordedItem Integer -> Map.Map Activity Integer -> Map.Map Activity Integer
    go (number, activity) existingMap =
      Map.insertWith (+) activity number existingMap
activityTime _ = Nothing

mainFn :: IO ()
mainFn = do
  print getResult
  print "Average Time Spent Per Activity, Per Day"
  print $ averagePerDay getResult
  print "Sum Time Spent in Each Activity"
  print $ activityTime getResult
  where
    getResult :: Result (LogFormat Integer)
    getResult = parseString parseFile mempty finalExample
