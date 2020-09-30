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
parsedTweets = take 500 <$> catMaybes <$> map (parseTweet . encodeUtf8) <$> tweetsByLines
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
