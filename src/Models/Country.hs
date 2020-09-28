{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Country where

import Data.Aeson
import GHC.Generics

data Country = Country
  { code :: String,
    name :: String
  }
  deriving (Show, Generic)

instance FromJSON Country where
  parseJSON (Object o) =
    Country <$> (o .: "country_code")
      <*> (o .: "country")