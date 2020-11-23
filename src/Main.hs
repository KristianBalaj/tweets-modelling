{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Compression.GZip as GZip
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.List (isSuffixOf)
import Data.Maybe
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding
import Data.Time (getZonedTime)
import Elastic.Inserter as Elastic
import Models.Tweet
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (combine)

main :: IO ()
main = do
  getZonedTime >>= print

  parsedTweets >>= Elastic.insert . extractNestedTweets . take 10000

  getZonedTime >>= print

-- | Takes all the nested Tweets and adds them to the top level, to the top level tweets.
-- The result list then contains `top level tweets` and `all the nested tweets`.
extractNestedTweets :: [Tweet] -> [Tweet]
extractNestedTweets (x : xs) = extract (Just x) ++ extractNestedTweets xs
  where
    extract :: Maybe Tweet -> [Tweet]
    extract tweet = case tweet of
      Just a -> a : extract (parentTweet a)
      Nothing -> []
extractNestedTweets _ = []

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
