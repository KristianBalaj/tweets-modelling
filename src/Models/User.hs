{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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