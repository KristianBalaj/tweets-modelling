{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
  parseJSON (Object o) =
    TweetMention <$> (o .: "id")
      <*> (o .: "screen_name")
      <*> (o .: "name")
  parseJSON _ = mzero

instance ToJSON TweetMention