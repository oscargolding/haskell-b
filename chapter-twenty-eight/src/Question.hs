{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Question where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Data.Text
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

--
-- Complete the 'aVeryBigSum' function below.
--
-- The function is expected to return a LONG_INTEGER.
-- The function accepts LONG_INTEGER_ARRAY ar as parameter.
--

aVeryBigSum :: (Foldable t, Num a) => t a -> a
aVeryBigSum ar = do
  sum ar

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack

rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO ()
main = do
  stdout <- getEnv "OUTPUT_PATH"
  fptr <- openFile stdout WriteMode

  arCountTemp <- getLine
  let arCount = read $ lstrip $ rstrip arCountTemp :: Int

  arTemp <- getLine

  let ar = Data.List.map (read :: String -> Integer) . Data.List.words $ rstrip arTemp

  let result = aVeryBigSum ar

  hPutStrLn fptr $ show result

  hFlush fptr
  hClose fptr