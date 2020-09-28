{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Coordinates where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson
import GHC.Generics

data Coordinates = Coordinates
  { longitude :: Float,
    latitude :: Float
  }
  deriving (Show, Generic)

instance FromJSON Coordinates where
  parseJSON (Object o) =
    Coordinates
      <$> ( do
              res <- o .: "coordinates"
              case res of
                [long, _] -> return long
                _ -> mzero
          )
      <*> ( do
              res <- o .: "coordinates"
              case res of
                [_, lat] -> return lat
                _ -> mzero
          )
