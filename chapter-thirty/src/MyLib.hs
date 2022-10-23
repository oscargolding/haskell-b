module MyLib (someFunc) where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
  putStrLn ("Running main caused an error! It was: " ++ show e)
  writeFile "bbb" "hi"

someFunc :: IO ()
someFunc = do
  writeFile "zzz" "hi" `catch` handler
  putStrLn "wrote to file"

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

canICatch :: Exception e => e -> IO (Either SomeException ())
canICatch e = try $ throwIO e
