{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Compression.GZip as GZip
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Int (Int64)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding
import Data.Time
import Database.CountriesHandling
import Database.Database
import Database.PostgreSQL.Simple
import Database.TweetsHandling
import Database.UsersHandling
import Models.Tweet
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (combine)

main :: IO ()
main = do
  getZonedTime >>= print

  conn <- connect2Postgres

  countriesMap <- insertAllCountriesFromData conn
  hashtagsMap <- insertAllHashtagsFromData conn

  _ <- insertAllUsersFromData conn
  print . (++ " --- Finished inserting users.") =<< (show <$> getZonedTime)

  _ <- insertAllTweets conn countriesMap
  print . (++ " --- Finished inserting tweets.") =<< (show <$> getZonedTime)

  _ <- insertAllMentionsFromData conn
  print . (++ " --- Finished inserting tweet mentions.") =<< (show <$> getZonedTime)

  _ <- insertAllTweetHashtags conn hashtagsMap
  print . (++ " --- Finished inserting tweet hashtags.") =<< (show <$> getZonedTime)

  addConstraintsToTweets conn
  addConstraintsToTweetMentions conn
  addConstraintsToTweetHashtags conn

  getZonedTime >>= print

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
