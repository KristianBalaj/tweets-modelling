{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Coordinates where

import Control.Monad (MonadPlus (mzero))
import Data.Aeson
import Data.ByteString.Builder
import Data.ByteString.UTF8 as BSU
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
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

instance ToField Coordinates where
  toField coords = Plain (byteString $ BSU.fromString $ "st_setsrid(st_point(" ++ show (longitude coords) ++ ", " ++ show (latitude coords) ++ "), 4326)")

instance ToRow Coordinates