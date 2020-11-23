{-# LANGUAGE OverloadedStrings #-}

module Elastic.ElasticTweet (ElasticTweet (..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.Functor
import Models.Coordinates
import Models.Tweet

newtype ElasticTweet = ElasticTweet Tweet

-- |  Similar to `object` function except this removed all the `Null` values.
-- Credits to: https://stackoverflow.com/questions/19665099/data-aeson-encoding-optional-keys#comment43757457_19666176
objectNoNulls :: [Pair] -> Value
objectNoNulls = object . filter (\(_, value) -> value /= Null)

instance ToJSON ElasticTweet where
  toJSON (ElasticTweet tweet) =
    objectNoNulls
      [ "id_str" .= tweetId tweet,
        "tweet_text" .= tweetContent tweet,
        "coordinates"
          .= ( tweetLocation tweet
                 <&> ( \coords ->
                         object ["lat" .= latitude coords, "lon" .= longitude coords]
                     )
             ),
        "retweet_count" .= retweetCount tweet,
        "favorite_count" .= favoriteCount tweet,
        "created_at" .= happenedAt tweet,
        "author" .= tweetAuthor tweet,
        "place" .= tweetCountry tweet,
        "parent_tweet_id" .= (tweetId <$> parentTweet tweet),
        "mentions" .= mentionedUsers tweet,
        "hashtags" .= tweetHashtags tweet
      ]