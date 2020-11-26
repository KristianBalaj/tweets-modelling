{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Models.TweetMention where

import Control.Monad
import Data.Aeson
import GHC.Generics (Generic)

data TweetMention = TweetMention
  { userId :: Integer,
    screenName :: String,
    name :: String
  }
  deriving (Show, Generic)

instance FromJSON TweetMention where
  parseJSON (Object o) = do
    userId <- o .: "id"
    screenName <- o .: "screen_name"
    name <- o .: "name"
    return TweetMention {..}
  parseJSON _ = mzero

instance ToJSON TweetMention