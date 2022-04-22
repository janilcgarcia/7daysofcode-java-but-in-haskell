{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import qualified Web.Scotty as W
import Network.Wreq

import GHC.Generics (Generic)

import System.IO (hPutStrLn, stderr, hPrint)

import Control.Lens ((^.))

import qualified Data.Vector as V

import Text.Printf (printf)
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
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

htmlBase :: Text -> Text
htmlBase contents = T.intercalate "\n"
  [ "<!DOCTYPE html>"
  , "<html>"
  , "  <head>"
  , "    <meta charset=\"utf-8\" />"
  , "    <title>Top 250 IMDB movies</title>"
  , "    <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css\" />"
  , "  </head>"
  , "  <body>"
  , "    <div class=\"container\">"
  , "      <h1>Top 250 Movies on IMDB</h1>"
  , contents
  , "    </div>"
  , "  <script src=\"https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js\"></script>"
  , "  </bdoy>"
  , "</html>"
  ]

htmlMovieSection :: Movie -> Text
htmlMovieSection (Movie title urlImage year rating) =
  let ratingText = T.pack (printf "%.2f" rating :: String)
  in T.intercalate "\n"
  [ "<div class=\"card mb-3 mt-2\">"
  , "  <div class=\"row g-0\">"
  , "    <div class=\"col-md-2\">"
  , "      <img src=\"", urlImage, "\" class=\"img-fluid rounded-start\" />"
  , "    </div>"
  , "    <div class=\"col-md-10\">"
  , "      <div class=\"card-body\">"
  , T.concat ["        <h4 class=\"card-title\">", title, "</h4>"]
  , "        <div class=\"card-text\">"
  , "          <dl class=\"row\">"
  , "            <dt class=\"col-sm-3\">Released at:</dt>"
  , T.concat ["            <dd>", T.pack $ show year, "</dd>"]
  , "            <dt class=\"col-sm-3\">Released at:</dt>"
  , T.concat ["            <dd>", ratingText, "</dd>"]
  , "          </dl>"
  , "        </div>"
  , "      </div>"
  , "    </div>"
  , "  </div>"
  , "</div>"
  ]

htmlMovieSections :: [Movie] -> Text
htmlMovieSections = T.intercalate "\n" . map htmlMovieSection

html :: [Movie] -> Text
html = htmlBase . htmlMovieSections

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
            Just items -> runServer items
            Nothing -> pure ()

    runServer movies =
      let index = TL.fromStrict . html $ movies
      in W.scotty 3000 $
         W.get "/" $ do
           W.html index

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


