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
import qualified JSONParser as JP

import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Map as M

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
      r <- get $ top250MoviesUrl apiKey Nothing

      let bodyText = TE.decodeUtf8 . LBS.toStrict $ r ^. responseBody
          parsedBody = JP.parseJson bodyText

      case parsedBody of
        Left error ->
          putStrLn "Parsing error..."

        Right result -> do
          printResult result "Title" "title"
          printResult result "ID" "id"
          printResult result "Rank" "rank"
          printResult result "FullTitle" "fullTitle"
          printResult result "Year" "year"
          printResult result "Image" "image"
          printResult result "Crew" "crew"
          printResult result "IMDB Rating" "imDbRating"
          printResult result "IMDB Rating COunt" "imDbRatingCount"
          
    printResult (JP.JObject m) name key =
      let result = m M.! "items" in
      case extractByKey result key of
        Left message ->
          TIO.putStrLn $ T.append "Can't find key: " message
        Right entries -> do
          TIO.putStrLn $ T.snoc name ':'
          mapM_ TIO.putStrLn entries
          TIO.putStrLn ""

    printResult _ _ _ = putStrLn "Invalid result returned"

    key :: Text -> JP.JSON -> Maybe Text
    key k (JP.JObject m) = case M.lookup k m of
      Just (JP.JString t) -> Just t
      _ -> Nothing

    key _ _ = Nothing

    extractByKey :: JP.JSON -> Text -> Either Text [Text]
    extractByKey (JP.JArray objects) k =
      let findKeyOrError json = case key k json of
            Just value -> Right value
            Nothing -> Left $ T.concat ["Can't find ", k, " at object"]
      in mapM findKeyOrError objects

    extractByKey _ _ = Left "Invalid object type"


