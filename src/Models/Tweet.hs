{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Tweet (Tweet (..), parseTweet) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (MonadPlus (mzero))
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Models.Coordinates
import Models.Country
import Models.User

data Tweet = Tweet
  { id :: String,
    content :: String,
    location :: Maybe Coordinates,
    retweet_count :: Integer,
    favorite_count :: Maybe Integer,
    happened_at :: String,
    author :: User,
    country :: Maybe Country,
    parent_tweet :: Maybe Tweet,
    mentioned_users_ids :: [String],
    hashtags :: [String]
  }
  deriving (Show, Generic)

instance FromJSON Tweet where
  parseJSON (Object o) =
    Tweet <$> (o .: "id_str")
      <*> (o .: "full_text")
      <*> ((o .:? "coordinates") <|> return Nothing)
      <*> (o .: "retweet_count")
      <*> (o .:? "favorite_count")
      <*> (o .: "created_at")
      <*> (o .: "user")
      <*> (o .:? "place")
      <*> (o .:? "retweeted_status")
      <*> ( do
              res <- ((o .: "entities") >>= (.: "user_mentions"))
              mapM (.: "id_str") res
          )
      <*> ( do
              res <- ((o .: "entities") >>= (.: "hashtags"))
              mapM (.: "text") res
          )
  parseJSON _ = mzero

parseTweet :: B.ByteString -> Maybe Tweet
parseTweet = decode