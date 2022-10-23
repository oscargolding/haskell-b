{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import Hangman (Puzzle (..), fillInCharacter, handleGuess)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) []

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordtoGuess correct guessed) =
  if (length incorrectGuesses) > 7
    then do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordtoGuess
      exitSuccess
    else return ()
  where
    incorrectGuesses = filter (`notElem` guessedWords) guessed
    guessedWords =
      map (\(Just x) -> x) $
        filter
          ( \case
              (Just _) -> True
              _ -> False
          )
          correct

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
    then do
      putStrLn "You win!"
      exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

newtype WordList = WordList [String] deriving (Eq, Show)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String) in l >= minWordLength && l < maxWordLength

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
