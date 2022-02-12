{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Postgres.Inserter (insert) where

import Control.Monad.Trans.Resource (MonadResource, allocate, release)
import Data.Function ((&))
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, close)
import Models.Tweet (Tweet (tweetAuthor), TweetsStream)
import Postgres.Database.Database
  ( addConstraintsToTweetHashtags,
    addConstraintsToTweetMentions,
    addConstraintsToTweets,
    connect2Postgres,
  )
import Postgres.Database.TablesInserts (insertCountries, insertHashtags, insertTweetHashtags, insertTweets, insertUserMentions, insertUsers)
import Streaming (MonadIO (..))
import qualified Streaming.Prelude as S

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
