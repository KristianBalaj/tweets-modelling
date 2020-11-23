{-# LANGUAGE OverloadedStrings #-}

module Elastic.Models.ElasticTweet (ElasticTweet (..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.Functor
import Models.Coordinates
import Models.Tweet

newtype ElasticTweet = ElasticTweet Tweet

instance ToJSON ElasticTweet where
  toJSON (ElasticTweet tweet) =
    objectNoNulls
      [ "id" .= tweetId tweet,
        "tweetContent" .= tweetContent tweet,
        "location"
          .= ( tweetLocation tweet
                 <&> ( \coords ->
                         object ["lat" .= latitude coords, "lon" .= longitude coords]
                     )
             ),
        "retweetCount" .= retweetCount tweet,
        "favoriteCount" .= favoriteCount tweet,
        "createdAt" .= happenedAt tweet,
        "author" .= tweetAuthor tweet,
        "country" .= tweetCountry tweet,
        "parentTweetId" .= (tweetId <$> parentTweet tweet),
        "mentions" .= mentionedUsers tweet,
        "hashtags" .= tweetHashtags tweet
      ]

-- | Similar to `object` function except this removes all the `Null` values from the result JSON.
-- | Credits to: https://stackoverflow.com/questions/19665099/data-aeson-encoding-optional-keys#comment43757457_19666176
objectNoNulls :: [Pair] -> Value
objectNoNulls = object . filter (\(_, value) -> value /= Null)