{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Models.User
  ( User (..),
  )
where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow (ToRow (toRow))
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
  parseJSON (Object o) = do
    userId <- o .: "id"
    userScreenName <- o .: "screen_name"
    userName <- o .: "name"
    userDescription <- o .:? "description"
    followersCount <- o .: "followers_count"
    friendsCount <- o .: "friends_count"
    statusesCount <- o .: "statuses_count"
    return User {..}
  parseJSON _ = mzero

instance ToJSON User

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