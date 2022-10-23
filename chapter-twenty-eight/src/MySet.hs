module MySet where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt :: (Num a, Num b) => (a, b) -> (a, b)
bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where
    stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where
    stream = iterate (+ 1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

insertIntoMap :: Int -> M.Map Int Int
insertIntoMap n = M.insert n n m

insertIntoSet :: Int -> S.Set Int
insertIntoSet n = S.insert n s

mapUnion :: M.Map Int Int -> M.Map Int Int
mapUnion n = M.union n m

setUnion :: S.Set Int -> S.Set Int
setUnion n = S.union n s

main :: IO ()
main =
  defaultMain
    [ bench "members check map" $ whnf membersMap 9999,
      bench "member check set" $ whnf membersSet 9999,
      bench "insert into map" $ whnf insertIntoMap 12000,
      bench "insert into set" $ whnf insertIntoSet 12000,
      bench "union the map" $ whnf mapUnion m,
      bench "union the set" $ whnf setUnion s
    ]