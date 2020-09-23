{-# LANGUAGE DeriveGeneric #-}

module Coordinates where

import Data.Aeson
import GHC.Generics

data Coordinates = Coordinates
  { coordinates :: [Float],
    coord_type :: String
  }
  deriving (Show, Generic)

fieldsRename :: String -> String
fieldsRename "coord_type" = "type"
fieldsRename name = name

instance FromJSON Coordinates where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = fieldsRename
        }