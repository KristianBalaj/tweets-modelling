module Postgres.Inserter (insert) where

import Control.Monad (join)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Maybe
import Data.Time
import Database.PostgreSQL.Simple (Connection)
import Models.Tweet
import Postgres.Database.CountriesHandling (insertCountries)
import Postgres.Database.Database
import Postgres.Database.TweetsHandling (insertHashtags, insertTweetHashtags, insertTweets, insertUserMentions)
import Postgres.Database.UsersHandling (insertUsers)

insert :: [Tweet] -> IO ()
insert tweets = do
  conn <- connect2Postgres

  countriesMap <- insertAllCountriesFromData conn tweets
  hashtagsMap <- insertAllHashtagsFromData conn tweets

  _ <- insertAllUsersFromData conn tweets
  print . (++ " --- Finished inserting users.") =<< (show <$> getZonedTime)

  _ <- insertAllTweets conn tweets countriesMap
  print . (++ " --- Finished inserting tweets.") =<< (show <$> getZonedTime)

  _ <- insertAllMentionsFromData conn tweets
  print . (++ " --- Finished inserting tweet mentions.") =<< (show <$> getZonedTime)

  _ <- insertAllTweetHashtags conn tweets hashtagsMap
  print . (++ " --- Finished inserting tweet hashtags.") =<< (show <$> getZonedTime)

  addConstraintsToTweets conn
  addConstraintsToTweetMentions conn
  addConstraintsToTweetHashtags conn

insertAllCountriesFromData :: Connection -> [Tweet] -> IO (Map.Map String Int)
insertAllCountriesFromData conn tweets = do
  insertCountries conn (mapMaybe tweetCountry tweets)

insertAllHashtagsFromData :: Connection -> [Tweet] -> IO (Map.Map String Int)
insertAllHashtagsFromData conn tweets = do
  insertHashtags conn (join $ map tweetHashtags tweets)

insertAllTweets :: Connection -> [Tweet] -> Map.Map String Int -> IO Int64
insertAllTweets conn tweets countryCodeToIdMap = insertTweets conn countryCodeToIdMap tweets

insertAllTweetHashtags :: Connection -> [Tweet] -> Map.Map String Int -> IO Int64
insertAllTweetHashtags conn tweets hashtagsToIds = insertTweetHashtags conn hashtagsToIds tweets

insertAllMentionsFromData :: Connection -> [Tweet] -> IO Int64
insertAllMentionsFromData = insertUserMentions

insertAllUsersFromData :: Connection -> [Tweet] -> IO Int64
insertAllUsersFromData conn tweets = insertUsers conn $ map tweetAuthor tweets