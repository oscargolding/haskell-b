{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT (..), asks)
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans
  ( ActionT,
    ScottyT,
    get,
    html,
    param,
    scottyT,
  )

data Config = Config {counts :: IORef (M.Map Text Integer), prefix :: Text}

newtype ConfigM a = ConfigM
  {runConfigM :: ReaderT Config IO a}

type Scotty = ScottyT Text (ReaderT Config IO)

type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (M.insert k (getValue + 1) m, getValue + 1)
  where
    getValue :: Integer
    getValue = fromMaybe 0 (M.lookup k m)

app :: Scotty ()
app = get "/:key" $ do
  unprefixed <- param "key"
  gotPrefix <- lift $ asks prefix
  let key' = mappend gotPrefix unprefixed
  map <- lift $ asks counts
  readVal <- (lift . lift) $ readIORef map
  let (newMap, num) = bumpBoomp key' readVal
  (lift . lift) $ writeIORef map newMap
  html $
    mconcat
      [ "<h1>Success! Count was: ",
        TL.pack $ show num,
        "</h1>"
      ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config =
        Config
          { counts = counter,
            prefix = TL.pack prefixArg
          }
      runR a = runReaderT a config
  scottyT 3000 runR app