{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Compression.GZip as GZip
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.Int (Int64)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding
import Data.Time
import Database.PostgreSQL.Simple
import Models.Country
import Models.Tweet
import Models.User
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (combine)

connectionString :: ByteString
connectionString =
  postgreSQLConnectionString $
    ConnectInfo
      { connectHost = "localhost",
        connectPort = 5432,
        connectUser = "xbalaj",
        connectPassword = "password",
        connectDatabase = "xbalaj"
      }

main :: IO ()
main = do
  getZonedTime >>= print

  conn <- connectPostgreSQL connectionString

  countriesMap <- insertAllCountriesFromData conn
  hashtagsMap <- insertAllHashtagsFromData conn

  _ <- insertAllUsersFromData conn

  _ <- insertAllTweets conn countriesMap

  _ <- insertAllMentionsFromData conn
  _ <- insertAllTweetHashtags conn hashtagsMap

  addConstraintsToTweets conn
  addConstraintsToTweetMentions conn
  addConstraintsToTweetHashtags conn

  getZonedTime >>= print

addConstraintsToTweets :: Connection -> IO ()
addConstraintsToTweets conn = do
  _ <-
    mapM
      (execute_ conn)
      [ -- clearing missing author_id references
        "UPDATE tweets SET author_id = null WHERE NOT EXISTS (SELECT a.id FROM accounts AS a WHERE a.id = tweets.author_id)",
        -- adding foreign key constraints
        "ALTER TABLE tweets ADD FOREIGN KEY (parent_id) REFERENCES tweets(id)",
        "ALTER TABLE tweets ADD FOREIGN KEY (country_id) REFERENCES countries(id)",
        "ALTER TABLE tweets ADD FOREIGN KEY (author_id) REFERENCES accounts(id)"
      ]
  return ()

addConstraintsToTweetMentions :: Connection -> IO ()
addConstraintsToTweetMentions conn = do
  _ <-
    mapM
      (execute_ conn)
      [ -- deleting rows with missing refs
        "DELETE FROM tweet_mentions AS tm WHERE NOT EXISTS (SELECT id FROM accounts AS a WHERE a.id = tm.account_id) OR NOT EXISTS (SELECT id FROM tweets as t WHERE t.id = tm.tweet_id)",
        -- remove duplicates
        "DELETE FROM tweet_mentions a USING tweet_mentions b WHERE a.id < b.id AND a.tweet_id = b.tweet_id AND a.account_id = b.account_id",
        -- adding foreign key constraints
        "ALTER TABLE tweet_mentions ADD FOREIGN KEY (account_id) REFERENCES accounts(id)",
        "ALTER TABLE tweet_mentions ADD FOREIGN KEY (tweet_id) REFERENCES tweets(id)",
        -- adding unique constraint
        "ALTER TABLE tweet_mentions ADD UNIQUE (account_id, tweet_id)"
      ]
  return ()

addConstraintsToTweetHashtags :: Connection -> IO ()
addConstraintsToTweetHashtags conn = do
  _ <-
    mapM
      (execute_ conn)
      [ -- deleting rows with missing refs
        "DELETE FROM tweet_hashtags AS th WHERE NOT EXISTS (SELECT id FROM hashtags AS h WHERE h.id = th.hashtag_id) OR NOT EXISTS (SELECT id FROM tweets as t WHERE t.id = th.tweet_id)",
        -- remove duplicates
        "DELETE FROM tweet_hashtags a USING tweet_hashtags b WHERE a.id < b.id AND a.tweet_id = b.tweet_id AND a.hashtag_id = b.hashtag_id",
        -- adding foreign key constraints
        "ALTER TABLE tweet_hashtags ADD FOREIGN KEY (hashtag_id) REFERENCES hashtags(id)",
        "ALTER TABLE tweet_hashtags ADD FOREIGN KEY (tweet_id) REFERENCES tweets(id)",
        -- adding unique constraint
        "ALTER TABLE tweet_hashtags ADD UNIQUE (hashtag_id, tweet_id)"
      ]
  return ()

insertAllTweets :: Connection -> Map.Map String Int -> IO Int64
insertAllTweets conn countryCodeToIdMap = insertTweets conn countryCodeToIdMap =<< parsedTweets

insertAllTweetHashtags :: Connection -> Map.Map String Int -> IO Int64
insertAllTweetHashtags conn hashtagsToIds = insertTweetHashtags conn hashtagsToIds =<< parsedTweets

insertAllMentionsFromData :: Connection -> IO Int64
insertAllMentionsFromData conn = insertUserMentions conn =<< parsedTweets

insertAllUsersFromData :: Connection -> IO Int64
insertAllUsersFromData conn = do
  tweets <- parsedTweets
  insertUsers conn $ map tweetAuthor tweets

insertAllHashtagsFromData :: Connection -> IO (Map.Map String Int)
insertAllHashtagsFromData conn = do
  tweets <- parsedTweets
  insertHashtags conn (join $ map tweetHashtags tweets)

insertAllCountriesFromData :: Connection -> IO (Map.Map String Int)
insertAllCountriesFromData conn = do
  tweets <- parsedTweets
  insertCountries conn (mapMaybe tweetCountry tweets)

parsedTweets :: IO [Tweet]
parsedTweets = mapMaybe (parseTweet . encodeUtf8) <$> tweetsByLines
  where
    dirPath = "data" :: FilePath
    suffix = ".jsonl.gz" :: String
    tweetsByLines :: IO [L.Text]
    tweetsByLines =
      let tweetFiles = tweetsZippedFilePaths >>= traverse B.readFile
       in fmap join $ map (L.lines . decodeUtf8 . GZip.decompress) <$> tweetFiles

    tweetsZippedFilePaths :: IO [String]
    tweetsZippedFilePaths = do
      exists <- doesDirectoryExist dirPath
      if exists
        then do
          fileNames <- filter (isSuffixOf suffix) <$> getDirectoryContents dirPath
          return $ map (combine dirPath) fileNames
        else fail $ "No such directory exists (directory=\"" ++ dirPath ++ "\")!"
