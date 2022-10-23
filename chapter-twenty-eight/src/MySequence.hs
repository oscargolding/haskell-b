module MySequence where

import Criterion.Main
import qualified Data.Sequence as S

lists :: [[Int]]
lists = replicate 10 [1 .. 100000]

seqs :: [S.Seq Int]
seqs = replicate 10 (S.fromList [1 .. 100000])

newLists :: [Int]
newLists = [1 .. 100000]

newSeqs :: S.Seq Int
newSeqs = S.fromList [1 .. 100000]

main :: IO ()
main =
  defaultMain
    [ bench "concatenate lists" $ nf mconcat lists,
      bench "concatenate sequences" $ nf mconcat seqs,
      bench "indexing list" $ whnf (\xs -> xs !! 9001) newLists,
      bench "indexing sequence" $ whnf (flip S.index 9001) newSeqs
    ]