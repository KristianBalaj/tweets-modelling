{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as B
import Database.PostgreSQL.Simple
import Models.Tweet

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
  putStrLn "Hello, Haskell!"
  tweet <- fmap parseTweet $ B.readFile "test.json"
  print tweet

  -- conn <- connectPostgreSQL connectionString
  -- res <- query_ conn "select * from users" :: IO [String]
  -- mapM print res
  return ()
