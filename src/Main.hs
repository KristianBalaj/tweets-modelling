{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Compression.GZip as GZip
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import Data.List
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding
import Database.PostgreSQL.Simple
import Models.Tweet
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
  tweetLines <- tweetsByLines
  print $ map (parseTweet . encodeUtf8) tweetLines

tweetsByLines :: IO [L.Text]
tweetsByLines =
  let tweetFiles = tweetsZippedFilePaths "data" >>= traverse B.readFile
   in fmap join $ map (L.lines . decodeUtf8 . GZip.decompress) <$> tweetFiles

tweetsZippedFilePaths :: FilePath -> IO [String]
tweetsZippedFilePaths dirPath = do
  exists <- doesDirectoryExist dirPath
  if exists
    then do
      fileNames <- filter (isSuffixOf ".jsonl.gz") <$> getDirectoryContents dirPath
      return $ map (combine dirPath) fileNames
    else fail $ "No such directory exists (directory=\"" ++ dirPath ++ "\")!"