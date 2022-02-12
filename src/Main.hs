{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import CmdLine
import qualified Codec.Compression.GZip as GZip
import Control.Exception (NoMethodError (..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Trans.Resource (MonadResource, ResourceT, allocate, release, runResourceT)
import Data.Aeson
import Data.Aeson.Types (FromJSON (parseJSON))
import qualified Data.ByteString.Lazy as B
import Data.Function
import Data.Functor
import Data.List (isSuffixOf)
import Data.Maybe
import Data.Proxy (Proxy)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy.IO as S
import Data.Time (getZonedTime)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection)
import Elastic.Inserter as Elastic
import Models.Tweet
import qualified Postgres.Inserter as PostgresInserter
import Streaming
import qualified Streaming as S
import Streaming.ByteString.Char8 as C
import qualified Streaming.Prelude as S
import Streaming.Zip (gunzip)
import System.Directory (getDirectoryContents)
import System.Exit
import System.FilePath (combine)

deriving instance FromJSON ConnectInfo

newtype ArgsException = WrongFileFormatException ImportOptions deriving (Exception)

instance Show ArgsException where
  show (WrongFileFormatException Postgres) = "Couldn't parse Postgres connection file."
  show (WrongFileFormatException Elastic) = "Couldn't parse Elastic connection file."

main :: IO ()
main = runImport `catches` [Handler parserExit]
  where
    -- Handles when the args parser throws exit code
    parserExit :: ExitCode -> IO ()
    parserExit _ = pure ()

runImport :: IO ()
runImport = do
  (Params importOption connectionFile) <- cmdParamsParser
  fileContents <- B.readFile connectionFile
  case importOption of
    Postgres ->
      case decode @ConnectInfo fileContents of
        Nothing -> throwM $ WrongFileFormatException importOption
        Just ci -> runResourceT (tweetsStream & PostgresInserter.insert ci)
    Elastic -> case decode @ElasticConfig fileContents of
      Nothing -> throwM $ WrongFileFormatException importOption
      Just ec -> runResourceT (tweetsStream & Elastic.insert ec)

tweetsStream :: (MonadIO m, MonadResource m) => TweetsStream m ()
tweetsStream =
  do
    fileNames <-
      liftIO $
        Prelude.take 1
          . Prelude.map (combine "data")
          . Prelude.filter (isSuffixOf ".jsonl.gz")
          <$> getDirectoryContents "data"

    S.each fileNames
      & S.mapM
        (\fileName -> C.readFile fileName & gunzip)
      & S.mconcat
      & C.lines
      & mapsM (S.toList . C.unpack)
      & S.map (parseTweet . B.fromStrict . encodeUtf8 . T.pack)
      & S.catMaybes
      & S.map tweetWithParentTweets
      & S.concat
      & void
