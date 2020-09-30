{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Models.Tweet where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Int (Int64)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    executeMany,
    returning,
  )
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
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

insertHashtags :: Connection -> [String] -> IO (Map.Map String Int)
insertHashtags conn hashtags = do
  res :: [(String, Int)] <- returning conn "INSERT INTO hashtags (value) VALUES (?) ON CONFLICT DO NOTHING RETURNING value, id" $ map Only hashtags
  return $ Map.fromList res

insertTweets :: Connection -> Map.Map String Int -> [Tweet] -> IO Int64
insertTweets conn countryCodeToIdMap tweets =
  sum <$> mapM insert (chunksOf 5000 tweets)
  where
    insert :: [Tweet] -> IO Int64
    insert xs =
      executeMany
        conn
        "INSERT INTO tweets (id, content, location, retweet_count, favorite_count, happened_at, author_id, country_id, parent_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
        $ map
          ( \x ->
              ( tweetId x,
                tweetContent x,
                tweetLocation x,
                retweetCount x,
                favoriteCount x,
                happenedAt x,
                userId (tweetAuthor x),
                (countryCode <$> tweetCountry x) >>= (`Map.lookup` countryCodeToIdMap),
                tweetId <$> parentTweet x
              )
          )
          xs

insertUserMentions :: Connection -> [Tweet] -> IO Int64
insertUserMentions conn tweets = sum <$> mapM insert insertionChunks
  where
    mentionsTuples :: Tweet -> [(Integer, String)]
    mentionsTuples tweet = map (,tweetId tweet) $ mentionedUsersIds tweet

    insertionChunks :: [[(Integer, String)]]
    insertionChunks = chunksOf 5000 $ join $ map mentionsTuples tweets

    insert :: [(Integer, String)] -> IO Int64
    insert xs = executeMany conn "INSERT INTO tweet_mentions (account_id, tweet_id) VALUES (?, ?)" xs

insertTweetHashtags :: Connection -> Map.Map String Int -> [Tweet] -> IO Int64
insertTweetHashtags conn hashtagsToIds tweets = sum <$> mapM insert insertionChunks
  where
    hashtagsTuples :: Tweet -> [(Int, String)]
    hashtagsTuples tweet =
      let hashtag2Tuple :: String -> Maybe (Int, String)
          hashtag2Tuple hashtag = Map.lookup hashtag hashtagsToIds >>= (\x -> Just (x, tweetId tweet))
       in mapMaybe hashtag2Tuple $ tweetHashtags tweet

    insertionChunks :: [[(Int, String)]]
    insertionChunks = chunksOf 5000 $ join $ map hashtagsTuples tweets

    insert :: [(Int, String)] -> IO Int64
    insert = executeMany conn "INSERT INTO tweet_hashtags (hashtag_id, tweet_id) VALUES (?, ?)"

-- addTweet conn = execute conn "INSERT INTO tweets (id, content) values (?, ?)"

parseTweet :: B.ByteString -> Maybe Tweet
parseTweet = decode