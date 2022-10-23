module MyText where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TLIO
import qualified System.IO as SIO

dictWords :: IO String
dictWords = SIO.readFile "/home/oscar/repos/haskell/haskell-test/chapter-twenty-eight/CHANGELOG.md"

dictWordsT :: IO T.Text
dictWordsT = TIO.readFile "/home/oscar/repos/haskell/haskell-test/chapter-twenty-eight/CHANGELOG.md"

dictWordsTL :: IO TL.Text
dictWordsTL = TLIO.readFile "/home/oscar/repos/haskell/haskell-test/chapter-twenty-eight/CHANGELOG.md"

main :: IO ()
main = do
  replicateM_ 1000 (dictWords >>= print)
  replicateM_ 1000 (dictWordsT >>= TIO.putStrLn)
  replicateM_ 1000 (dictWordsTL >>= TLIO.putStrLn)
