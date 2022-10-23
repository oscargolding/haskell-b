-- mood.hs
module Mood where

data Mood = Blah | Woot deriving (Show)

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

data Love = Oscar | Liz | LizCar deriving (Show)

changeLove Oscar = Liz
changeLove Liz = Oscar
changeLove _ = LizCar