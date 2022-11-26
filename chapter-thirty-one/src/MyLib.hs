{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module MyLib where

import Control.Exception
import Control.Monad (forever)
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import GHC.Generics
import Network.Socket
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

data User = User
  { userId :: Integer,
    username :: Text,
    shell :: Text,
    homeDirectory :: Text,
    realName :: Text,
    phone :: Text
  }
  deriving (Eq, Show)

data UserTransfer = UserTransfer
  { username_a :: Text,
    shell_a :: Text,
    homeDirectory_a :: Text,
    realName_a :: Text,
    phone_a :: Text
  }
  deriving (Generic, Show, Eq)

instance A.FromJSON UserTransfer

instance A.ToJSON UserTransfer

type TransferFormat = [UserTransfer]

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

insertOrReplaceUser :: Query
insertOrReplaceUser = "INSERT OR REPLACE INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"

updateUserQ :: Query
updateUserQ =
  [r|
UPDATE users
SET username = :name,
    shell = :shell,
    homeDirectory = :home,
    realName = :real,
    phone = :phone
WHERE
    id = :id;
|]

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

type DBFile = String

-- The DBFile that is used to power the entire application
getDBFile :: DBFile
getDBFile = "finger.db"

updateUserDB :: Connection -> User -> IO ()
updateUserDB conn user = do
  executeNamed
    conn
    updateUserQ
    [ ":name" := username user,
      ":shell" := shell user,
      ":home" := homeDirectory user,
      ":real" := realName user,
      ":phone" := phone user,
      ":id" := userId user
    ]
  return ()

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

insertAdminUser :: Connection -> UserTransfer -> IO ()
insertAdminUser conn user = do
  execute conn insertOrReplaceUser meRow
  where
    meRow :: UserRow
    meRow =
      ( Null,
        username_a user,
        shell_a user,
        homeDirectory_a user,
        realName_a user,
        phone_a user
      )

insertAllAdminUsers :: Connection -> TransferFormat -> IO ()
insertAllAdminUsers conn list = do
  -- Just insert all of them one by one :)
  mapM_ (insertAdminUser conn) list
  -- Get all the users after this
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])