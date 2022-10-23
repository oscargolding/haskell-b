module Main where

import HaskellSay (haskellSay)
import qualified MyLib (someFunc)

main :: IO ()
main = do
  haskellSay "welcome to chapter twenty-seven!"
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
