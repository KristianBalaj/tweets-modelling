{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Postgres.Inserter (insert) where

import CmdLine
import qualified Codec.Compression.GZip as GZip
import Control.Exception (Exception)
import Control.Monad
import Control.Monad (join)
import Control.Monad.Catch
import Control.Monad.Catch (bracket)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, allocate, release, runResourceT)
import Data.Aeson
import Data.Aeson.Types (FromJSON (parseJSON))
import Data.Bifunctor (second)
import qualified Data.ByteString.Lazy as B
import Data.Coerce
import Data.Function
import Data.Functor
import Data.Int (Int64)
import Data.List (isSuffixOf, partition)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.IO as S
import Data.Time
import Data.Time (getZonedTime)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, Only (Only, fromOnly), ToRow, close, returning)
import Models.Country
import Models.Tweet
import Postgres.Database.Database
  ( DbEntityId (..),
    addConstraintsToTweetHashtags,
    addConstraintsToTweetMentions,
    addConstraintsToTweets,
    connect2Postgres,
    insertEntities,
  )
import Postgres.Database.TablesInserts (insertCountries, insertHashtags, insertTweetHashtags, insertTweets, insertUserMentions, insertUsers)
import Streaming
import Streaming (MonadIO (liftIO), Of ((:>)), chunksOf, concats)
import qualified Streaming as S
import Streaming.ByteString.Char8 as C
import qualified Streaming.Prelude as S
import Streaming.Zip (gunzip)
import System.Directory (getDirectoryContents)

-- | The allocation and release was inspired by @danidiaz in my StackOverflow question
-- | https://stackoverflow.com/questions/71072001/combining-resourcet-with-bracket-in-a-streaming-pipeline/71086092
insert :: (MonadIO m, MonadResource m) => ConnectInfo -> TweetsStream m r -> m ()
insert connInfo tweetsStream =
  do
    (key, conn) <- allocate (connect2Postgres connInfo) close
    res <- inserting tweetsStream conn
    release key
    return res
  where
    inserting :: (MonadIO m, MonadResource m) => TweetsStream m r -> Connection -> m ()
    inserting stream conn = do
      _ <-
        stream
          & insertCountries conn
          & S.store (insertTweets conn)
          & S.map fst
          & S.store (insertUsers conn . S.map tweetAuthor)
          & S.store (insertUserMentions conn)
          & insertHashtags conn
          & insertTweetHashtags conn

      -- The constraints are added later to make the import faster
      liftIO (addConstraintsToTweets conn)
      liftIO (addConstraintsToTweetMentions conn)
      liftIO (addConstraintsToTweetHashtags conn)
