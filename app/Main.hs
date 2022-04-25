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

import Text.Parsec (ParseError)

import Data.List (sortOn)

-- |A content model
data ContentType = Movie deriving (Show, Eq)
data Content = Content { title :: Text
                       , urlImage :: Text
                       , year :: Int
                       , rating :: Double
                       , cType :: ContentType
                       }
             deriving (Show, Eq)

data APIError = MalformedJSON
              | InvalidBody
              deriving (Show, Eq)

-- |Get the API URL for the top 250 movies on IMDB takes an API key and can
-- |optionally take a lang parameter
top250MoviesUrl :: Text -> Maybe Text -> Text
top250MoviesUrl apiKey lang =
  let langSegment = maybe "" (`T.append` "/") lang
  in T.concat ["https://imdb-api.com/", langSegment, "API/Top250Movies/", apiKey]

-- |Lookup a key on a JSON. The JSON must be a JObject and must contain a string
-- |value or it will fail.
keyLookup :: Text -> JP.JSON -> Maybe Text
keyLookup k (JP.JObject m) = case M.lookup k m of
  Just (JP.JString t) -> Just t
  _ -> Nothing
keyLookup _ _ = Nothing

-- |Parse movie from the API Json response.
parseMovie :: JP.JSON -> Maybe Content
parseMovie json =
  let attr = flip keyLookup json
  in Content <$>
     attr "title" <*>
     attr "image" <*>
     fmap (read . T.unpack) (attr "year") <*>
     fmap (read . T.unpack) (attr "imDbRating") <*>
     pure Movie

htmlBase :: Text -> Text -> Text
htmlBase title body = T.intercalate "\n"
  [ "<!DOCTYPE html>"
  , "<html>"
  , "  <head>"
  , "    <meta charset=\"utf-8\" />"
  , T.concat ["    <title>", title, "</title>"]
  , "    <link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css\" />"
  , "  </head>"
  , "  <body>"
  , "    <div class=\"container\">"
  , T.concat ["      <h1>", title, "</h1>"]
  , body
  , "    </div>"
  , "  <script src=\"https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/js/bootstrap.bundle.min.js\"></script>"
  , "  </bdoy>"
  , "</html>"
  ]

htmlMovieSection :: Content -> Text
htmlMovieSection (Content title urlImage year rating _) =
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

htmlMovieSections :: [Content] -> Text
htmlMovieSections = T.intercalate "\n" . map htmlMovieSection

htmlForMovies :: [Content] -> Text
htmlForMovies = htmlBase "Top 250 Movies" . htmlMovieSections

toMaybe :: Either a b -> Maybe b
toMaybe (Right b) = Just b
toMaybe _ = Nothing

leftMap :: (e -> e') -> Either e a -> Either e' a
leftMap _ (Right r) = Right r
leftMap f (Left e) = Left $ f e

rightOr :: e -> Maybe a -> Either e a
rightOr _ (Just r) = Right r
rightOr e Nothing = Left e

getTopMovies :: Text -> IO (Either APIError [Content])
getTopMovies key = do
  r <- get . T.unpack $ top250MoviesUrl key Nothing

  let json = JP.parseJson . TE.decodeUtf8 . LBS.toStrict $ r ^. responseBody
  pure $ leftMap (const InvalidBody) json >>=
    rightOr MalformedJSON . flip traverseItems parseMovie

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
      let apiKey = getApiKey config
      parsedBody <- getTopMovies apiKey

      case parsedBody of
        Left error ->
          putStrLn $ "Request error: " ++ show error

        Right result ->
          runServer result

    runServer movies =
      let index f = TL.fromStrict . htmlForMovies $ sortOn f movies

      in W.scotty 3000 $ do
         W.get "/" $ do
           W.redirect "/rating"

         W.get "/:order" $ do
           order :: Text <- W.param "order"

           case order of
             "title" -> W.html $ index title
             "year" -> W.html $ index year
             _ -> W.html $ index (((-1.0) *) . rating)

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

