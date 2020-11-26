{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Models.TweetMention
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
    mentionedUsers :: [TweetMention],
    tweetHashtags :: [String]
  }
  deriving (Show, Generic)

instance FromJSON Tweet where
  parseJSON (Object o) = do
    tweetId <- o .: "id_str"
    tweetContent <- o .: "full_text"
    tweetLocation <- (o .:? "coordinates") <|> return Nothing
    retweetCount <- o .: "retweet_count"
    favoriteCount <- o .:? "favorite_count"
    happenedAt <- o .: "created_at"
    tweetAuthor <- o .: "user"
    tweetCountry <- o .:? "place" <|> return Nothing
    parentTweet <- o .:? "retweeted_status"
    mentionedUsers <- (o .: "entities") >>= (.: "user_mentions")
    tweetHashtags <- do
      res <- (o .: "entities") >>= (.: "hashtags")
      mapM (.: "text") res
    return Tweet {..}
  -- Not using RecordWildcard syntax version
  -- Tweet <$> (o .: "id_str")
  --   <*> (o .: "full_text")
  --   <*> ((o .:? "coordinates") <|> return Nothing)
  --   <*> (o .: "retweet_count")
  --   <*> (o .:? "favorite_count")
  --   <*> (o .: "created_at")
  --   <*> (o .: "user")
  --   <*> (o .:? "place" <|> return Nothing)
  --   <*> (o .:? "retweeted_status")
  --   <*> ((o .: "entities") >>= (.: "user_mentions"))
  --   <*> ( do
  --           res <- (o .: "entities") >>= (.: "hashtags")
  --           mapM (.: "text") res
  --       )
  parseJSON _ = mzero

parseTweet :: B.ByteString -> Maybe Tweet
parseTweet = decode