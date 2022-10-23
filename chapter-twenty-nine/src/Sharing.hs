module Sharing where

import Debug.Trace

blah :: IO String
blah = return "blah"

blah' = trace "outer trace" blah

woot :: IO String
woot = return (trace "inner trace" "woot")

traceMain :: IO ()
traceMain = do
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w