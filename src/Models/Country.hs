{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Models.Country where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson
import qualified Data.Map as Map
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import GHC.Generics

data Country = Country
  { countryCode :: String,
    countryName :: String
  }
  deriving (Show, Generic, Eq)

instance FromJSON Country where
  parseJSON (Object o) =
    Country <$> (o .: "country_code")
      <*> (o .: "country")
  parseJSON _ = mzero

instance ToRow Country where
  toRow t = [toField (countryCode t), toField (countryName t)]

insertCountries :: Connection -> [Country] -> IO (Map.Map String Int)
insertCountries conn countries = do
  res :: [(String, Int)] <- returning conn "INSERT INTO countries (code, name) VALUES (?, ?) ON CONFLICT DO NOTHING RETURNING code, id" countries
  return $ Map.fromList res