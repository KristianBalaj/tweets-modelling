{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.User (User (..)) where

import Data.Aeson
import GHC.Generics

data User = User
  { id :: String,
    screen_name :: String,
    name :: String,
    description :: Maybe String,
    followers_count :: Integer,
    friends_count :: Integer,
    statuses_count :: Integer
  }
  deriving (Generic, Show)

instance FromJSON User where
  parseJSON (Object o) =
    User <$> (o .: "id_str")
      <*> (o .: "screen_name")
      <*> (o .: "name")
      <*> (o .:? "description")
      <*> (o .: "followers_count")
      <*> (o .: "friends_count")
      <*> (o .: "statuses_count")
