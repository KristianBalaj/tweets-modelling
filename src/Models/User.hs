{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.User (User (..), insertUsers) where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson
import Data.Int (Int64)
import Data.List.Split (chunksOf)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics

data User = User
  { userId :: Integer,
    userScreenName :: String,
    userName :: String,
    userDescription :: Maybe String,
    followersCount :: Integer,
    friendsCount :: Integer,
    statusesCount :: Integer
  }
  deriving (Generic, Show)

instance FromJSON User where
  parseJSON (Object o) =
    User <$> (o .: "id")
      <*> (o .: "screen_name")
      <*> (o .: "name")
      <*> (o .:? "description")
      <*> (o .: "followers_count")
      <*> (o .: "friends_count")
      <*> (o .: "statuses_count")
  parseJSON _ = mzero

instance ToRow User where
  toRow t =
    [ toField (userId t),
      toField (userScreenName t),
      toField (userName t),
      toField (userDescription t),
      toField (followersCount t),
      toField (friendsCount t),
      toField (statusesCount t)
    ]

insertUsers :: Connection -> [User] -> IO Int64
insertUsers conn users = sum <$> mapM insert (chunksOf 5000 users)
  where
    insert :: [User] -> IO Int64
    insert =
      executeMany
        conn
        "INSERT INTO accounts (id, screen_name, name, description, followers_count, friends_count, statuses_count) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"