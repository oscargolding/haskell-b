{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import qualified Data.Aeson as A
import Data.ByteString.Lazy (fromStrict)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types (Null (Null))
import MyLib
import System.Exit (exitSuccess)

handleDBAction :: UserRow -> IO ()
handleDBAction row = do
  conn <- SQLite.open getDBFile
  SQLite.execute conn insertUser row
  rows <- SQLite.query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn

inputSelector :: IO ()
inputSelector = do
  putStrLn "Please provide a username: "
  usernameProvided <- TIO.getLine
  putStrLn "Please provide a shell directory: "
  shellDir <- TIO.getLine
  putStrLn "Please provide a home directory: "
  homeDir <- TIO.getLine
  putStrLn "Please provide a real name: "
  realNameProvided <- TIO.getLine
  putStrLn "Please provide a phone number: "
  phoneNum <- TIO.getLine
  let insertingRow =
        ( Null,
          usernameProvided,
          shellDir,
          homeDir,
          realNameProvided,
          phoneNum
        ) ::
          UserRow
  handleDBAction insertingRow

updateSelector :: User -> IO ()
updateSelector oldUser = do
  putStrLn $ "Provide a new username, old is: " ++ show (username oldUser)
  newUsername <- TIO.getLine
  putStrLn $ "Provide a new shell directory, old is: " ++ show (shell oldUser)
  newShell <- TIO.getLine
  putStrLn $ "Provide a new home directory, old is: " ++ show (homeDirectory oldUser)
  newHome <- TIO.getLine
  putStrLn $ "Provide a new real name, old is: " ++ show (realName oldUser)
  newName <- TIO.getLine
  putStrLn $ "Provide a new phone number, old is: " ++ show (phone oldUser)
  newPhone <- TIO.getLine
  conn <- SQLite.open getDBFile
  let requiredUser =
        User
          { userId = userId oldUser,
            username = T.strip newUsername,
            shell = T.strip newShell,
            homeDirectory = T.strip newHome,
            realName = T.strip newName,
            phone = T.strip newPhone
          }
  updateUserDB conn requiredUser
  rows <- SQLite.query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn

updateUser :: IO ()
updateUser = do
  putStrLn "Please provide the username that you want to update: "
  updatingName <- TIO.getLine
  conn <- SQLite.open getDBFile
  userExists <- getUser conn (T.strip updatingName)
  SQLite.close conn
  case userExists of
    Nothing -> do
      putStrLn "User does not exist, cannot update."
    Just x -> do
      putStrLn ("Found a user to update :" ++ show x)
      updateSelector x

insertAdminUsers :: IO ()
insertAdminUsers = do
  putStrLn "Please enter the input of the users that you want to add: "
  jsonUsers <- TIO.getLine
  conn <- SQLite.open getDBFile
  case (A.decode (fromStrict $ encodeUtf8 jsonUsers) :: Maybe TransferFormat) of
    Nothing -> putStrLn "Could not understand your admin input"
    Just x -> do
      putStrLn "Could understand the format that you are using :)"
      insertAllAdminUsers conn x

main :: IO ()
main = do
  putStrLn "### Welcome to the Finger Interactive Session ###"
  forever $ do
    putStrLn "q to quit, i to insert, u for update or a for admin"
    args <- getLine
    case args of
      "q" -> exitSuccess
      "i" -> inputSelector
      "u" -> updateUser
      "a" -> insertAdminUsers
      _otherwise -> putStrLn "Unrecognised input provided. Try again."