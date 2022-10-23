{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use putStr" #-}
{-# HLINT ignore "Use getChar" #-}
module Vigenere where

import Control.Monad.IO.Class ()
import Data.Char (chr, isLower, isUpper, ord)
import GHC.IO.Handle (hGetChar, hPutStr, hWaitForInput, isEOF)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr, stdin, stdout)

cipher :: String -> Int -> (Int -> Int -> Int) -> String
cipher [] rot fn = []
cipher (x : xs) rot fn
  | isLower x = chr (perform x 97 rot) : cipher xs rot fn
  | isUpper x = chr (perform x 65 rot) : cipher xs rot fn
  | otherwise = x : cipher xs rot fn
  where
    perform x loc rotation = loc + shift x loc rotation
    shift x loc shift = mod (rotate x loc shift) 26
    rotate x loc = fn (base x loc)
    base x loc = ord x - loc

encrypt :: String -> Int -> String
encrypt word rot = cipher word rot (+)

decrypt :: String -> Int -> String
decrypt word rot = cipher word rot (-)

decryptCipher :: String -> IO ()
decryptCipher contents = do
  hPutStr stdout $ decrypt contents 32

encryptCipher :: String -> IO ()
encryptCipher contents = do
  hPutStr stdout $ encrypt contents 32

processContents :: String -> Int -> IO String
processContents result timeout = do
  isTimedOut <- hWaitForInput stdin timeout
  if isTimedOut
    then putStrLn "Input found."
    else do
      hPutStrLn stderr "No input found."
      exitFailure
  isOver <- isEOF
  if isOver
    then return result
    else do
      char <- hGetChar stdin
      processContents (result ++ [char]) timeout

retrieveFromStdIn :: Int -> (String -> IO ()) -> IO ()
retrieveFromStdIn num fn = do
  results <- processContents "" num
  fn results

cipherMain :: IO ()
cipherMain = do
  args <- getArgs
  case args of
    ["-d", "-t", x] -> retrieveFromStdIn (read x :: Int) decryptCipher
    ["-e", "-t", x] -> retrieveFromStdIn (read x :: Int) encryptCipher
    _otherWise -> error "Unrecognised parameters passed to the program"