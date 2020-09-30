{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Tweet
  ( Tweet (..),
    parseTweet,
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Models.Coordinates
import Models.Country
import Models.User

data Tweet = Tweet
  { tweetId :: String,
    tweetContent :: String,
    tweetLocation :: Maybe Coordinates,
    retweetCount :: Integer,
    favoriteCount :: Maybe Integer,
    happenedAt :: String,
    tweetAuthor :: User,
    tweetCountry :: Maybe Country,
    parentTweet :: Maybe Tweet,
    mentionedUsersIds :: [Integer],
    tweetHashtags :: [String]
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
      <*> (o .:? "place" <|> return Nothing)
      <*> (o .:? "retweeted_status")
      <*> ( do
              res <- (o .: "entities") >>= (.: "user_mentions")
              mapM (.: "id") res
          )
      <*> ( do
              res <- (o .: "entities") >>= (.: "hashtags")
              mapM (.: "text") res
          )
  parseJSON _ = mzero

parseTweet :: B.ByteString -> Maybe Tweet
parseTweet = decode