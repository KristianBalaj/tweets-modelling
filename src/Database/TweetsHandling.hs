{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Database.TweetsHandling
  ( insertHashtags,
    insertTweets,
    insertUserMentions,
    insertTweetHashtags,
  )
where

import Control.Monad (join)
import Data.Int (Int64)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe
import Database.Database (insertChunkSize)
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only),
    executeMany,
    returning,
  )
import Models.Country
import Models.Tweet
import Models.User

insertHashtags :: Connection -> [String] -> IO (Map.Map String Int)
insertHashtags conn hashtags = do
  res <- join <$> mapM insert (chunksOf insertChunkSize $ filter (\x -> length x > 0) hashtags)
  return $ Map.fromList res
  where
    insert :: [String] -> IO [(String, Int)]
    insert chunk = returning conn "INSERT INTO hashtags (value) VALUES (?) ON CONFLICT DO NOTHING RETURNING value, id" $ map Only chunk

insertTweets :: Connection -> Map.Map String Int -> [Tweet] -> IO Int64
insertTweets conn countryCodeToIdMap tweets =
  sum
    <$> mapM insert (chunksOf insertChunkSize (allTweets tweets))
  where
    allTweets :: [Tweet] -> [Tweet]
    allTweets (x : xs) = tweetWithParentTweets x ++ allTweets xs
      where
        tweetWithParentTweets :: Tweet -> [Tweet]
        tweetWithParentTweets tweet =
          case parentTweet tweet of
            Nothing -> [tweet]
            Just a -> tweet : tweetWithParentTweets a
    allTweets _ = []

    insert :: [Tweet] -> IO Int64
    insert xs =
      executeMany
        conn
        "INSERT INTO tweets (id, content, location, retweet_count, favorite_count, happened_at, author_id, country_id, parent_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"
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
    insertionChunks = chunksOf insertChunkSize $ join $ map mentionsTuples tweets

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
    insertionChunks = chunksOf insertChunkSize $ join $ map hashtagsTuples tweets

    insert :: [(Int, String)] -> IO Int64
    insert = executeMany conn "INSERT INTO tweet_hashtags (hashtag_id, tweet_id) VALUES (?, ?)"