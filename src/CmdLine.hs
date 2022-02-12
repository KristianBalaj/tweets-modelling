{-# LANGUAGE TypeApplications #-}

module CmdLine (Params (..), ImportOptions (..), cmdParamsParser) where

import Control.Arrow (Arrow (first))
import Options.Applicative

data ImportOptions = Postgres | Elastic deriving (Read, Show, Bounded, Enum)

data Params = Params
  { importOption :: ImportOptions,
    connectionFile :: FilePath
  }
  deriving (Show)

cmdParams :: Parser Params
cmdParams =
  Params
    <$> ( let importOptionsStr = show ([minBound .. maxBound] :: [ImportOptions])
           in option
                auto
                ( long "import"
                    <> help ("Where to import the data. Possible options: " <> importOptionsStr)
                    <> metavar importOptionsStr
                )
        )
      <*> strOption
        ( long "connection-file"
            <> help "A path to json file containing connection data for the selected import"
            <> metavar "FILE"
        )

cmdParamsParser :: IO Params
cmdParamsParser = execParser opts
  where
    opts = info (cmdParams <**> helper) (fullDesc <> progDesc "IP ranges lookup")