{-# LANGUAGE OverloadedStrings #-}

module Postgres.Database.UsersHandling (insertUsers) where

import Data.Int (Int64)
import Data.List.Split (chunksOf)
import Postgres.Database.Database (insertChunkSize)
import Database.PostgreSQL.Simple (Connection, executeMany)
import Models.User (User)

insertUsers :: Connection -> [User] -> IO Int64
insertUsers conn users = sum <$> mapM insert (chunksOf insertChunkSize users)
  where
    insert :: [User] -> IO Int64
    insert =
      executeMany
        conn
        "INSERT INTO accounts (id, screen_name, name, description, followers_count, friends_count, statuses_count) VALUES (?, ?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING"