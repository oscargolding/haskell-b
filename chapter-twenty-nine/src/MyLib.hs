module MyLib (someFunc) where

import Control.Concurrent

myData :: IO (MVar Int)
myData = newEmptyMVar

someFunc :: IO ()
someFunc = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv'
  print zero
