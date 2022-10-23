module Main where

import qualified Fail (myMain)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Fail.myMain
