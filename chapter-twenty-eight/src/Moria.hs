module Moria where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

v :: V.Vector Int
v = V.fromList [1 .. 1000]

u :: U.Vector Int
u = U.fromList [1 .. 1000]

firstTest :: IO ()
firstTest = do
  defaultMain
    [ bench "slicing vector" $ whnf (V.head . V.slice 100 900) v
    ]

secondTest :: IO ()
secondTest = do
  defaultMain
    [ bench "slicing unboxed vector" $ whnf (U.head . U.slice 100 900) u
    ]