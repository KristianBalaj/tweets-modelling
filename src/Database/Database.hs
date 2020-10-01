{-# LANGUAGE OverloadedStrings #-}

module Database.Database
  ( insertChunkSize,
    connect2Postgres,
    addConstraintsToTweetHashtags,
    addConstraintsToTweetMentions,
    addConstraintsToTweets,
  )
where

import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple

insertChunkSize :: Int
insertChunkSize = 5000

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

connect2Postgres :: IO Connection
connect2Postgres = connectPostgreSQL connectionString

addConstraintsToTweets :: Connection -> IO ()
addConstraintsToTweets conn =
  mapM_
    (execute_ conn)
    [ -- clearing missing author_id references
      "UPDATE tweets SET author_id = null WHERE NOT EXISTS (SELECT a.id FROM accounts AS a WHERE a.id = tweets.author_id)",
      -- adding foreign key constraints
      "ALTER TABLE tweets ADD FOREIGN KEY (parent_id) REFERENCES tweets(id)",
      "ALTER TABLE tweets ADD FOREIGN KEY (country_id) REFERENCES countries(id)",
      "ALTER TABLE tweets ADD FOREIGN KEY (author_id) REFERENCES accounts(id)"
    ]

addConstraintsToTweetMentions :: Connection -> IO ()
addConstraintsToTweetMentions conn =
  mapM_
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

addConstraintsToTweetHashtags :: Connection -> IO ()
addConstraintsToTweetHashtags conn =
  mapM_
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