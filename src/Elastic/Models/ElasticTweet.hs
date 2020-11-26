{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Elastic.Models.ElasticTweet (ElasticTweet (..)) where

import Data.Aeson
import Data.Aeson.Types
import Data.Functor
import Models.Coordinates
import Models.Tweet

newtype ElasticTweet = ElasticTweet Tweet

instance ToJSON ElasticTweet where
  toJSON (ElasticTweet Tweet {tweetId = tweetIndex, ..}) =
    objectNoNulls
      [ "id" .= tweetIndex,
        "tweetContent" .= tweetContent,
        "location"
          .= ( tweetLocation
                 <&> ( \coords ->
                         object ["lat" .= latitude coords, "lon" .= longitude coords]
                     )
             ),
        "retweetCount" .= retweetCount,
        "favoriteCount" .= favoriteCount,
        "createdAt" .= happenedAt,
        "author" .= tweetAuthor,
        "country" .= tweetCountry,
        "parentTweetId" .= (tweetId <$> parentTweet),
        "mentions" .= mentionedUsers,
        "hashtags" .= tweetHashtags
      ]

-- | Similar to `object` function except this removes all the `Null` values from the result JSON.
-- | Credits to: https://stackoverflow.com/questions/19665099/data-aeson-encoding-optional-keys#comment43757457_19666176
objectNoNulls :: [Pair] -> Value
objectNoNulls = object . filter (\(_, value) -> value /= Null)