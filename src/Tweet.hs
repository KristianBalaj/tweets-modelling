{-# LANGUAGE DeriveGeneric #-}

module Tweet (Tweet (..), parseTweet) where

--(FromJSON, decode)

import Coordinates
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Place
import User

data Tweet = Tweet
  { created_at :: String,
    id :: Integer,
    id_str :: String,
    text :: String,
    source :: String,
    truncated :: Bool,
    in_reply_to_status_id :: Maybe Integer,
    in_reply_to_status_id_str :: Maybe String,
    in_reply_to_user_id :: Maybe Integer,
    in_reply_to_user_id_str :: Maybe String,
    in_reply_to_screen_name :: Maybe String,
    -- user :: User,
    coordinates :: Maybe Coordinates,
    place :: Maybe Place,
    quoted_status_id :: Maybe Integer,
    quoted_status_id_str :: String,
    is_quote_status :: Bool,
    quoted_status :: Tweet,
    retweeted_status :: Tweet,
    quote_count :: Maybe Integer,
    reply_count :: Integer,
    retweet_count :: Integer,
    favorite_count :: Maybe Integer,
    entities :: String, -- TODO
    extended_entities :: String -- TODO
  }
  deriving (Show, Generic)

instance FromJSON Tweet

parseTweet :: B.ByteString -> Maybe Tweet
parseTweet = decode