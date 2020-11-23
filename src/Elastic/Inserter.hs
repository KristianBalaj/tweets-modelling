{-# LANGUAGE OverloadedStrings #-}

module Elastic.Inserter (insert) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.List.Split (chunksOf)
import Elastic.Models.BulkCommands
import Elastic.Models.ElasticTweet
import Models.Tweet
import Network.HTTP.Req

bulkSize :: Int
bulkSize = 5000

elasticPort :: Int
elasticPort = 9200

tweetsIndex :: String
tweetsIndex = "tweets"

-- | Creates 2 commands for adding the Tweet to the index via bulk.
tweetBulkCommands :: ElasticTweet -> [L.ByteString]
tweetBulkCommands elasticTweet@(ElasticTweet tweet) =
  [ encode (BulkIndexCommand $ BulkCommand {esIndex = tweetsIndex, documentId = tweetId tweet}),
    encode elasticTweet
  ]

insert :: [Tweet] -> IO ()
insert tweets = mapM_ bulkInsert $ chunksOf bulkSize tweets
  where
    bulkInsert :: [Tweet] -> IO ()
    bulkInsert tweetsChunk = runReq
      defaultHttpConfig
      $ do
        let body = L.intercalate "\n" (join $ map (tweetBulkCommands . ElasticTweet) tweetsChunk) <> "\n"

        _ <- req POST (http "localhost" /: "_bulk") (ReqBodyLbs body) bsResponse (header "Content-Type" "application/x-ndjson" <> port elasticPort)
        pure ()