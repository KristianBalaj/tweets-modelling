{-# LANGUAGE OverloadedStrings #-}

module Elastic.Models.BulkCommands where

import Data.Aeson

data BulkCommand = BulkCommand
  { esIndex :: String,
    documentId :: String
  }

-- | Used for bulk index operation.
-- It is the command (when serialized to JSON) that should be followed by document to be uploaded.
newtype BulkIndexCommand = BulkIndexCommand BulkCommand

instance ToJSON BulkIndexCommand where
  toJSON (BulkIndexCommand command) =
    object
      [ "index"
          .= object
            [ "_index" .= esIndex command,
              "_id" .= documentId command
            ]
      ]