{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Elastic.Inserter (insert, ElasticConfig (..)) where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Function
import Elastic.Models.BulkCommands
import Elastic.Models.ElasticTweet
import GHC.Generics
import Models.Tweet
import Network.HTTP.Req
import Streaming
import qualified Streaming.Prelude as S

bulkSize :: Int
bulkSize = 5000

data ElasticConfig = MkElasticConfig
  { elasticPort :: Int,
    index :: String -- in our case the tweets index
  }
  deriving (FromJSON, Generic)

-- | Creates 2 commands for adding the Tweet to the index via bulk.
tweetBulkCommands :: ElasticConfig -> ElasticTweet -> [L.ByteString]
tweetBulkCommands (MkElasticConfig _ tweetsIndex) elasticTweet@(ElasticTweet tweet) =
  [ encode (MkBulkIndexCommand $ MkBulkCommand {esIndex = tweetsIndex, documentId = tweetId tweet}),
    encode elasticTweet
  ]

insert :: MonadIO m => ElasticConfig -> TweetsStream m r -> m ()
insert conf@(MkElasticConfig elasticPort' _) tweetsStream =
  tweetsStream
    & chunksOf bulkSize
    & S.mapped S.toList
    & S.mapM_ (liftIO . bulkInsert)
    & void
  where
    bulkInsert :: [Tweet] -> IO ()
    bulkInsert tweetsChunk = runReq
      defaultHttpConfig
      $ do
        let body = L.intercalate "\n" (join $ map (tweetBulkCommands conf . ElasticTweet) tweetsChunk) <> "\n"

        _ <- req POST (http "localhost" /: "_bulk") (ReqBodyLbs body) bsResponse (header "Content-Type" "application/x-ndjson" <> port elasticPort')
        pure ()