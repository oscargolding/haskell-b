module Exercises where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (Reader, ReaderT, ask, mapReader, runReader)
import Control.Monad.Trans.State (StateT, get, put, runStateT)
import Data.Functor.Identity (Identity)
import System.Random (StdGen, mkStdGen, randomR, randomRIO)

rDec :: Num a => Reader a a
rDec = do
  val <- ask
  return $ val - 1

rShow :: Show a => ReaderT a Identity String
rShow = show <$> ask

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  val <- ask
  liftIO $ putStrLn ("Hi: " ++ show val)
  return $ val + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  val <- get
  put (val + 1)
  liftIO $ putStrLn ("Hi: " ++ show val)
  return $ show val

data GameType = PlayerVsAI | PVP deriving (Eq, Show)

data PlayerScores = PlayerScores
  { player :: Integer,
    computer :: Integer,
    gameType :: GameType,
    firstPlayerGuesses :: [Integer]
  }

data GameBoard = GameBoard
  { playerGuess :: Integer,
    playerFinger :: Integer,
    aiGuess :: Integer,
    aiFinger :: Integer
  }

promptGuess :: String -> IO Integer
promptGuess guess = do
  putStrLn guess
  value <- getLine
  let numGuess = read value :: Integer
  if numGuess < 0 || numGuess > 5 then promptGuess guess else return numGuess

smartPicker :: [Integer] -> Maybe Integer
smartPicker guessList =
  if isValid
    then manageList tupleList lastTwoGuesses
    else Nothing
  where
    isValid :: Bool
    isValid = length guessList > 3

    manageList ::
      [(Integer, Integer, Integer)] ->
      (Integer, Integer) ->
      Maybe Integer
    manageList searchList (first, second) = foldr folding Nothing searchList
      where
        folding :: (Integer, Integer, Integer) -> Maybe Integer -> Maybe Integer
        folding (numA, numB, numC) maybeInt = case maybeInt of
          Just a -> Just a
          _ -> if numA == first && numB == second then Just numC else Nothing

    tupleList :: [(Integer, Integer, Integer)]
    tupleList = let x : y : ls = guessList in zip3 (x : y : ls) (y : ls) ls

    lastTwoGuesses :: (Integer, Integer)
    lastTwoGuesses = (head twoList, twoList !! 1)
      where
        twoList :: [Integer]
        twoList = take 2 $ reverse guessList

gameInputs :: StateT PlayerScores IO PlayerScores
gameInputs = do
  oldScores <- get
  playerFinger <- liftIO $ promptGuess "Please enter a finger number: "
  playerGuess <- liftIO $ promptGuess "Please enter a guess number: "

  aiGuess <- liftIO $ case smartPicker $ firstPlayerGuesses oldScores of
    Just a -> do
      putStrLn "Found a smart guess to pick :)"
      return a
    Nothing -> randomRIO (1 :: Integer, 5)

  aiFinger <- liftIO $ randomRIO (1 :: Integer, 5)

  let playerScore = if playerGuess == aiFinger then 1 else 0
  let aiScore = if aiGuess == playerFinger then 1 else 0
  liftIO $ putStrLn "### Reveals ###"
  liftIO $ putStrLn "Player Finger"
  liftIO $ print playerFinger
  liftIO $ putStrLn "AI Finger"
  liftIO $ print aiFinger
  liftIO $ putStrLn "### End Reveals ###"

  liftIO $ putStrLn "### Guesses Shown ###"
  liftIO $ putStrLn . unwords . map show $ firstPlayerGuesses oldScores ++ [playerFinger]
  liftIO $ putStrLn "### ###"

  put
    ( PlayerScores
        { player = player oldScores + playerScore,
          computer = computer oldScores + aiScore,
          gameType = gameType oldScores,
          firstPlayerGuesses = firstPlayerGuesses oldScores ++ [playerFinger]
        }
    )
  return oldScores

gamePVP :: StateT PlayerScores IO PlayerScores
gamePVP = do
  oldScores <- get
  liftIO $ putStrLn "Time for Player One"
  playerFingerOne <- liftIO $ promptGuess "Please enter a finger number: "
  playerGuessOne <- liftIO $ promptGuess "Please enter a guess number: "

  liftIO $ putStr "\ESC[2J"

  liftIO $ putStrLn "Time for Player Two"
  playerFingerTwo <- liftIO $ promptGuess "Please enter a finger number: "
  playerGuessTwo <- liftIO $ promptGuess "Please enter a guess number: "

  let playerScore = if playerGuessOne == playerFingerTwo then 1 else 0
  let playerTwoScore = if playerGuessTwo == playerFingerOne then 1 else 0
  liftIO $ putStrLn "### Reveals ###"
  liftIO $ putStrLn "Player One Finger"
  liftIO $ print playerFingerOne
  liftIO $ putStrLn "Player Two Finger"
  liftIO $ print playerFingerTwo
  liftIO $ putStrLn "### End Reveals ###"

  put
    ( PlayerScores
        { player = player oldScores + playerScore,
          computer = computer oldScores + playerTwoScore,
          gameType = gameType oldScores,
          firstPlayerGuesses = firstPlayerGuesses oldScores ++ [playerGuessOne]
        }
    )
  return oldScores

gameLoop :: StateT PlayerScores IO PlayerScores
gameLoop = do
  currentScores <- get
  if gameType currentScores == PVP then gamePVP else gameInputs
  -- Run it a second time
  currentScores <- get
  let secondPlayer = if gameType currentScores == PVP then "Player 2" else "Computer"
  let bothWin = player currentScores == 3 && computer currentScores == 3
  let playerWin = player currentScores == 3
  let comtuerWin = computer currentScores == 3
  case (bothWin, playerWin, comtuerWin) of
    (True, _, _) -> do
      liftIO $ putStrLn $ secondPlayer ++ " and Player Win!"
      return currentScores
    (_, True, False) -> do
      liftIO $ putStrLn "Player 1 Wins!"
      return currentScores
    (_, False, True) -> do
      liftIO $ putStrLn $ secondPlayer ++ " Wins!"
      return currentScores
    _ -> do
      liftIO $ putStrLn "Player 1 Score: "
      liftIO $ print (player currentScores)
      liftIO $ putStrLn $ secondPlayer ++ " Score: "
      liftIO $ print (computer currentScores)
      gameLoop
      return currentScores

mainMora :: IO ()
mainMora = do
  putStrLn "Welcome to the Mora Game"
  putStrLn "Please enter the game type (PVP for PVP) else anything else AI"
  gameType <- getLine
  let setType = if gameType == "PVP" then PVP else PlayerVsAI
  let startGame =
        PlayerScores
          { player = 0,
            computer = 0,
            gameType = setType,
            firstPlayerGuesses = []
          }
  runStateT gameLoop startGame
  putStrLn "Mora is Finished :)"
  return ()