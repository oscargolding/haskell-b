{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import MyLib
import Network.Socket
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

createUsers :: Query
createUsers =
  [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where
    meRow :: UserRow
    meRow =
      ( Null,
        "callen",
        "/bin/zsh",
        "/home/callen",
        "Chris Allen",
        "555-123-4567"
      )

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) =
  BS.concat
    [ "Login: ",
      e username,
      "\t\t\t\t",
      "Name: ",
      e realName,
      "\n",
      "Directory: ",
      e homeDir,
      "\t\t\t",
      "Shell: ",
      e shell,
      "\n"
    ]
  where
    e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      putStrLn ("Couldn't find matching user for username: " ++ (show username))
      return ()
    Just user -> sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name -> returnUser dbConn soc (decodeUtf8 name)

-- Runs forever, with the forever function
handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  close soc

handleAdminConnection :: Connection -> Socket -> IO ()
handleAdminConnection dbConn soc = do
  msg <- recv soc 1024
  putStrLn $ "Admin buffer input: " ++ show msg
  case (A.decode (fromStrict msg) :: Maybe TransferFormat) of
    Nothing -> putStrLn "Could not understand your format, use JSON"
    Just x -> do
      putStrLn "Could understand the format that you are using :)"
      insertAllAdminUsers dbConn x

handleAdminQuery :: Connection -> Socket -> IO ()
handleAdminQuery dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got an admin connection, handling"
  handleAdminConnection dbConn soc
  close soc

forkedAdminThread :: IO ()
forkedAdminThread = do
  putStrLn "This is a new thread present"
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing
      (Just "8080")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  NS.bind sock (addrAddress serveraddr)
  -- The maximum number of queued connections, should at least be one
  listen sock 1
  conn <- open getDBFile
  handleAdminQuery conn sock
  SQLite.close conn
  close sock

main :: IO ()
main = withSocketsDo $ do
  -- We do not really care what happens to the thread of what is forked
  _ <- forkIO forkedAdminThread
  addrinfos <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing
      (Just "79")
  let serveraddr = head addrinfos
  sock <-
    socket
      (addrFamily serveraddr)
      Stream
      defaultProtocol
  NS.bind sock (addrAddress serveraddr)
  listen sock 1
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  close sock
