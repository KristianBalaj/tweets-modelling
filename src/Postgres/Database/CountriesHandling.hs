{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Postgres.Database.CountriesHandling (insertCountries) where

import Control.Monad (join)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Postgres.Database.Database (insertChunkSize)
import Database.PostgreSQL.Simple (Connection, returning)
import Models.Country

insertCountries :: Connection -> [Country] -> IO (Map.Map String Int)
insertCountries conn countries = do
  res <- join <$> mapM insert (chunksOf insertChunkSize (filter (\x -> length (countryCode x) == 2) countries))
  return $ Map.fromList res
  where
    insert :: [Country] -> IO [(String, Int)]
    insert = returning conn "INSERT INTO countries (code, name) VALUES (?, ?) ON CONFLICT DO NOTHING RETURNING code, id"