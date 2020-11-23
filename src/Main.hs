{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Compression.GZip as GZip
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.List
import Data.Maybe
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding
import Elastic.ElasticTweet
import Models.Tweet
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath (combine)

main :: IO ()
main = do
  parsedTweets >>= print . decodeUtf8 . encode . map ElasticTweet . take 5

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
