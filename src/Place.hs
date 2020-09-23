{-# LANGUAGE DeriveGeneric #-}

module Place where

import Data.Aeson
import GHC.Generics

data Place = Place
  { id :: String,
    url :: String,
    place_type :: String,
    name :: String,
    full_name :: String,
    country_code :: String,
    country :: String,
    bounding_box :: String,
    attributes :: String
  }
  deriving (Show, Generic)

instance FromJSON Place