{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Postgres.Database.TablesInserts
  ( insertHashtags,
    insertTweets,
    insertUserMentions,
    insertTweetHashtags,
    insertCountries,
    insertUsers,
  )
where

import Control.Monad (join)
import Data.Bifunctor (second)
import Data.Coerce (coerce)
import Data.Function
import Data.Int (Int64)
import Data.List (partition)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Maybe
import Database.PostgreSQL.Simple
  ( Connection,
    Only (Only, fromOnly),
    executeMany,
    returning,
  )
import Models.Country
import Models.Tweet
import Models.TweetMention as TweetMention
import Models.User as User
import Postgres.Database.Database (DbEntityId (..), insertEntities, insertEntities_)
import Streaming
import qualified Streaming.Prelude as S

insertHashtags :: MonadIO m => Connection -> TweetsStream m r -> Stream (S.Of (Tweet, DbEntityId Hashtag)) m r
insertHashtags conn tweetsStream =
  tweetsStream
    & S.map (\tweet -> map (tweet,) (tweetHashtags tweet))
    & S.concat
    & insertEntities
      conn
      ( \s ->
          do
            let (tweets, hashtags) = unzip s
            res <- insert hashtags
            return $ zip tweets res
      )
    & S.concat
  where
    insert :: [Hashtag] -> IO [DbEntityId Hashtag]
    insert chunk = fmap coerce ((returning conn "INSERT INTO hashtags (value) VALUES (?) ON CONFLICT DO NOTHING RETURNING id" $ map (\(MkHashtag tag) -> Only tag) chunk) :: IO [Only Int])

insertTweets :: MonadIO m => Connection -> S.Stream (S.Of (Tweet, Maybe (DbEntityId Country))) m r -> m Int64
insertTweets conn tweetsStream =
  sum <$> insertEntities_ conn insert tweetsStream
  where
    insert :: [(Tweet, Maybe (DbEntityId Country))] -> IO Int64
    insert xs =
      executeMany
        conn
        "INSERT INTO tweets (id, content, location, retweet_count, favorite_count, happened_at, author_id, country_id, parent_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"
        $ map
          ( \(tweet, countryId) ->
              ( tweetId tweet,
                tweetContent tweet,
                tweetLocation tweet,
                retweetCount tweet,
                favoriteCount tweet,
                happenedAt tweet,
                User.userId (tweetAuthor tweet),
                coerce countryId :: Maybe Int,
                tweetId <$> parentTweet tweet
              )
          )
          xs

insertUserMentions :: MonadIO m => Connection -> TweetsStream m r -> m Int64
insertUserMentions conn tweetsStream = sum <$> insertEntities_ conn insert userMentionsStream
  where
    userMentionsStream =
      tweetsStream
        & S.map (\tweet -> map ((,tweetId tweet) . TweetMention.userId) $ mentionedUsers tweet)
        & S.concat
    insert :: [(Integer, String)] -> IO Int64
    insert xs = executeMany conn "INSERT INTO tweet_mentions (account_id, tweet_id) VALUES (?, ?)" xs

insertTweetHashtags :: MonadIO m => Connection -> S.Stream (S.Of (Tweet, DbEntityId Hashtag)) m r -> m Int64
insertTweetHashtags conn tweetsStream = sum <$> insertEntities_ conn insert tweetsStream
  where
    insert :: [(Tweet, DbEntityId Hashtag)] -> IO Int64
    insert chunk =
      executeMany conn "INSERT INTO tweet_hashtags (hashtag_id, tweet_id) VALUES (?, ?)" $
        map (\(tweet, MkDbEntityId hashtagId) -> (hashtagId, tweetId tweet)) chunk

insertCountries :: MonadIO m => Connection -> TweetsStream m r -> Stream (Of (Tweet, Maybe (DbEntityId Country))) m r
insertCountries conn tweetsStream =
  tweetsStream
    & insertEntities
      conn
      ( \tweets -> do
          let (withoutCountry, withCountry) = partition (isJust . tweetCountry) tweets
          res <- zip withCountry <$> insert (mapMaybe tweetCountry withCountry)
          return (map (second Just) res ++ map (,Nothing) withoutCountry)
      )
    & S.concat
  where
    insert :: [Country] -> IO [DbEntityId Country]
    insert countries = coerce . map fromOnly <$> (returning conn "INSERT INTO countries (code, name) VALUES (?, ?) ON CONFLICT DO NOTHING RETURNING id" countries :: IO [Only Int])

insertUsers :: MonadIO m => Connection -> S.Stream (S.Of User) m r -> m Int64
insertUsers conn usersStream =
  sum <$> insertEntities_ conn insert usersStream
  where
    insert :: [User] -> IO Int64
    insert =
      executeMany
        conn
        "INSERT INTO accounts (id, screen_name, name, description, followers_count, friends_count, statuses_count) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"