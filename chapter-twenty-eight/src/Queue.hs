module Queue where

import Criterion.Main
import qualified Data.Sequence as DS

data Queue a = Queue {enqueue :: [a], dequeue :: [a]} deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push item queue = Queue (item : enqueueList) dequeueList
  where
    enqueueList = enqueue queue
    dequeueList = dequeue queue

-- removes an item
pop :: Queue a -> Maybe (a, Queue a)
pop queue = case dequeue queue of
  (x : xs) -> Just (x, Queue (enqueue queue) xs)
  [] -> case revEnqueue of
    (y : ys) -> Just (y, Queue [] ys)
    [] -> Nothing
  where
    revEnqueue = reverse $ enqueue queue

testQueue :: Int -> Queue Int
testQueue num = go num (Queue [1 .. num] []) True
  where
    go 0 queue _ = queue
    go num queue True = go (num - 1) (push num queue) False
    go num queue False =
      go
        (num - 1)
        ( case pop queue of
            Just (_, newQueue) -> newQueue
            _ -> Queue [] []
        )
        True

simplePush :: a -> [a] -> [a]
simplePush a list = a : list

simplePop :: [a] -> Maybe (a, [a])
simplePop list = case reverse list of
  (x : xs) -> Just (x, reverse xs)
  _nothing -> Nothing

testSimple :: Int -> [Int]
testSimple num = go num [1 .. num] False
  where
    go 0 _ _ = []
    go num queue True = go (num - 1) (simplePush num queue) False
    go num queue False =
      go
        (num - 1)
        ( case simplePop queue of
            Just (_, newQueue) -> newQueue
            _nothing -> []
        )
        True

pushSeq :: a -> DS.Seq a -> DS.Seq a
pushSeq a seq = (DS.<|) a seq

popSeq :: DS.Seq a -> Maybe (a, DS.Seq a)
popSeq DS.Empty = Nothing
popSeq (xs DS.:|> x) = Just (x, xs)

testSeq :: Int -> DS.Seq Int
testSeq num = go num (DS.fromList [1 .. num]) False
  where
    go 0 seq _ = seq
    go num seq True = go (num - 1) (pushSeq num seq) False
    go num seq False =
      go
        (num - 1)
        ( case popSeq seq of
            Just (_, newQueue) -> newQueue
            _nothing -> DS.empty
        )
        True

-- questionable benchmarks
mainBench :: IO ()
mainBench = do
  defaultMain
    [ bench "testing our queue operations" $ whnf testQueue 123456,
      bench "testing the simple queue" $ whnf testSimple 123456,
      bench "testing the sequence now" $ whnf testSeq 123456
    ]