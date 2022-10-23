module Main where

import Hangman
import Test.Hspec
import Test.QuickCheck

alwaysGetFinalRight :: String -> Char -> Bool
alwaysGetFinalRight word character =
  fillInCharacter inputPuzzle character
    == Puzzle newWord (Just character : tail characterList) [character]
  where
    inputPuzzle = Puzzle newWord resultList ""
    newWord = character : word
    characterList = map Just newWord
    resultList = Nothing : tail characterList

-- Note how the property test works for the given, have to reconstruct semantic
testFillInCharacter :: IO ()
testFillInCharacter = hspec $ do
  describe "fill in character" $ do
    it "fills in a character" $ do
      fillInCharacter (Puzzle "ca" [Nothing, Nothing] "") 'c'
        `shouldBe` Puzzle "ca" [Just 'c', Nothing] "c"
    it "does not fill when wrong" $ do
      fillInCharacter (Puzzle "ca" [Nothing, Nothing] "") 'l'
        `shouldBe` Puzzle "ca" [Nothing, Nothing] "l"
    it "properly fills in the right guess" $ do
      property alwaysGetFinalRight

testHandleGuess :: IO ()
testHandleGuess = hspec $ do
  describe "handle guess fn" $ do
    it "should return the same" $ do
      puzzle <- handleGuess (Puzzle "ca" [Nothing, Nothing] "l") 'l'
      puzzle `shouldBe` Puzzle "ca" [Nothing, Nothing] "l"
    it "should return a new puzzle with wrong guess" $ do
      puzzle <- handleGuess (Puzzle "ca" [Nothing, Nothing] "") 'v'
      puzzle `shouldBe` Puzzle "ca" [Nothing, Nothing] "v"
    it "should return a correct guess when correct" $ do
      puzzle <- handleGuess (Puzzle "ca" [Nothing, Nothing] "") 'c'
      puzzle `shouldBe` Puzzle "ca" [Just 'c', Nothing] "c"

main :: IO ()
main = do
  testFillInCharacter
  testHandleGuess
