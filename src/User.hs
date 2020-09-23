{-# LANGUAGE DeriveGeneric #-}

module User (User (..)) where

import Data.Aeson (FromJSON)
import GHC.Generics

data User = User
  { id :: Integer,
    id_str :: String,
    name :: String,
    screen_name :: String,
    location :: Maybe String,
    derived :: String,
    url :: Maybe String,
    description :: Maybe String,
    protected :: Bool,
    verified :: Bool,
    followers_count :: Integer,
    friends_count :: Integer,
    listed_count :: Integer,
    favourites_count :: Integer,
    statuses_count :: Integer,
    created_at :: String,
    profile_banner_url :: String,
    profile_image_url_https :: String,
    default_profile :: String,
    withheld_in_countries :: [String],
    withheld_scope :: String
  }
  deriving (Generic, Show)

instance FromJSON User
