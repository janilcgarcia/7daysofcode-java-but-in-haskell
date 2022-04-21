{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Config
  ( Config(..)
  , loadConfig
  , getApiKey
  )
where

import GHC.Generics (Generic)
import Data.Text (Text)

import Data.Yaml (ParseException, decodeFileEither
                 , FromJSON(..), Value(..), (.:)
                 )

import Data.Aeson.Types (prependFailure, typeMismatch)

newtype IMDBConfig = IMDBConfig { apiKey :: Text }
                   deriving (Show, Eq, Generic)

newtype Config = Config { imdb :: IMDBConfig }
               deriving (Show, Eq, Generic)

instance FromJSON IMDBConfig where
  parseJSON (Object v) = IMDBConfig <$> v .: "api-key"
  parseJSON invalid = prependFailure
    "imdb must be an object with an api-key field"
    (typeMismatch "Object" invalid)

instance FromJSON Config

-- |Load config from the config.yaml file. If something happens during parsing
-- |produces a ParseException, otherwise produces a Config
loadConfig :: IO (Either ParseException Config)
loadConfig = decodeFileEither "config.yaml"

-- |Shortcut for apiKey . imdb, the only configuration present
getApiKey :: Config -> Text
getApiKey = apiKey . imdb
