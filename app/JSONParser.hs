{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module JSONParser
  ( parseJson
  , JSON(..)
  )
where

import Data.Map (Map)
import Data.Text (Text, pack)
import qualified Data.Map as M

import Text.Parsec
import Text.Parsec.Char

data JSON = JObject (Map Text JSON)
          | JArray [JSON]
          | JString Text
          deriving (Show, Eq)

jstring :: Stream s m Char => ParsecT s u m Text 
jstring = 
  let stringContentChar = (char '\\' *> char '"') <|> satisfy (/= '"')
      stringContent = many stringContentChar
  in pack <$> (char '"' *> stringContent <* char '"')

jarray :: Stream s m Char => ParsecT s u m [JSON]
jarray = char '[' *> sepBy jsonValue (char ',') <* char ']'

jobject :: Stream s m Char => ParsecT s u m (Map Text JSON)
jobject =
  let kv = (,) <$> (jstring <* char ':') <*> jsonValue
  in M.fromList <$> (char '{' *> sepBy kv (char ',') <* char '}')

jsonValue :: Stream s m Char => ParsecT s u m JSON
jsonValue = fmap JString jstring <|> fmap JArray jarray <|> fmap JObject jobject

-- |Parse JSON from text
parseJson :: Text -> Either ParseError JSON
parseJson = parse jsonValue "Json document"
