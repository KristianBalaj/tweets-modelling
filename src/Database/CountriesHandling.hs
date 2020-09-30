{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.CountriesHandling (insertCountries) where

import qualified Data.Map as Map
import Database.PostgreSQL.Simple (Connection, returning)
import Models.Country

insertCountries :: Connection -> [Country] -> IO (Map.Map String Int)
insertCountries conn countries = do
  res :: [(String, Int)] <- returning conn "INSERT INTO countries (code, name) VALUES (?, ?) ON CONFLICT DO NOTHING RETURNING code, id" countries
  return $ Map.fromList res