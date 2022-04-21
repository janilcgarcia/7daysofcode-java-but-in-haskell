{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Network.Wreq

import GHC.Generics (Generic)

import System.IO (hPutStrLn, stderr, hPrint)

import Control.Lens ((^.))

import qualified Data.Vector as V

import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Map as M

import Config
import qualified JSONParser as JP

-- |Movie model
data Movie = Movie { title :: Text
                   , urlImage :: Text
                   , year :: Int
                   , rating :: Double
                   }
           deriving (Show, Eq)

-- |Get the API URL for the top 250 movies on IMDB takes an API key and can
-- |optionally take a lang parameter
top250MoviesUrl :: String -> Maybe String -> String
top250MoviesUrl apiKey lang =
  let langSegment = maybe "" (++ "/") lang
  in "https://imdb-api.com/" ++ langSegment  ++ "API/Top250Movies/" ++ apiKey

-- |Lookup a key on a JSON. The JSON must be a JObject and must contain a string
-- |value or it will fail.
keyLookup :: Text -> JP.JSON -> Maybe Text
keyLookup k (JP.JObject m) = case M.lookup k m of
  Just (JP.JString t) -> Just t
  _ -> Nothing
keyLookup _ _ = Nothing

-- |Parse movie from the API Json response.
parseMovie :: JP.JSON -> Maybe Movie
parseMovie json =
  let attr = flip keyLookup json
  in Movie <$>
     attr "title" <*>
     attr "image" <*>
     fmap (read . T.unpack) (attr "year") <*>
     fmap (read . T.unpack) (attr "imDbRating")

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
          case flip traverseItems parseMovie $ result of
            Just items -> mapM_ print items
            Nothing -> pure ()

    -- |Traverse items on a JSON object and collect a list of f applied to all
    -- |traversed items
    traverseItems :: JP.JSON -> (JP.JSON -> Maybe a) -> Maybe [a]
    traverseItems (JP.JObject m) f = do
      itemsJson <- M.lookup "items" m
      items <- case itemsJson of
        JP.JArray items -> Just items
        _ -> Nothing

      mapM f items
    traverseItems _ _ = Nothing


