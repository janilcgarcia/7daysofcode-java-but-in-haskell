{-# LANGUAGE DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Network.Wreq
import Data.Yaml (ParseException, decodeFileEither)
import Data.Aeson (FromJSON, parseJSON, Value(..), (.:))
import Data.Aeson.Types (prependFailure, typeMismatch
                        , Parser(..), Result
                        , fromJSON)

import Data.Text (Text(..))
import qualified Data.Text as T

import GHC.Generics (Generic)

import System.IO (hPutStrLn, stderr, hPrint)

import Control.Lens ((^.), Prism', each)
import Data.Aeson.Lens (key, _Array, _JSON, AsJSON(..), values)

import qualified Data.Vector as V

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

loadConfig :: IO (Either ParseException Config)
loadConfig = decodeFileEither "config.yaml"

getApiKey :: Config -> Text
getApiKey = apiKey . imdb

top250MoviesUrl :: String -> Maybe String -> String
top250MoviesUrl apiKey lang =
  let langSegment = maybe "" (++ "/") lang
  in "https://imdb-api.com/" ++ langSegment  ++ "API/Top250Movies/" ++ apiKey

data Movie = Movie { id :: String
                   , rank :: String
                   , title :: String
                   , fullTitle :: String
                   , year :: String
                   , image :: String
                   , crew :: String
                   , imdbRating :: String
                   , imdbRatingCount :: String
                   }
           deriving (Show, Eq)

instance FromJSON Movie where
  parseJSON (Object v) =
    Movie <$> v .: "id" <*> v .: "rank" <*> v .: "title" <*>
    v .: "fullTitle" <*> v .: "year" <*> v .: "image" <*> v .: "crew" <*>
    v .: "imDbRating" <*> v .: "imDbRatingCount"

  parseJSON invalid = prependFailure
    "a Movie must be an object"
    (typeMismatch "Object" invalid)

main :: IO ()
main = do
  config <- loadConfig

  case config of
    Left error -> do
      hPutStrLn stderr "Error loading config: "
      hPrint stderr error

    Right config ->
      run config

  where
    run config = do
      let apiKey = T.unpack $ getApiKey config
      r <- asJSON =<< get (top250MoviesUrl apiKey Nothing) :: IO (Response Value)

      let items = r ^. responseBody . key "items" . _Array
          movies :: Result (V.Vector Movie) = mapM fromJSON items

      print movies


