{-# LANGUAGE TypeSynonymInstances #-}

module Alfred.Query (jsonQuery,escapeString,escapeText, Query, transformQuery, Query', transformQuery') where

import Network.HTTP
import Network.URI hiding (escapeString)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import System.IO.Error

-- | This function performs a query by performing an HTTP GET request
-- at the url obtained by concatenating the first argument with the
-- second one (after escaping it). The first argument is intended to
-- be the base url and the second one the query string. The result is
-- then parsed as a JSON object. For example,
-- for a Google search:
-- 
-- @
-- runQuery :: String -> IO (Text,[Text])
-- runQuery query = jsonQuery suggestURL query
-- 
-- suggestURL = "http://suggestqueries.google.com/complete/search?output=toolbar&client=firefox&hl=en&q="
-- @
-- 

type Query a = String -> IO (Either String a)
type Query' a = [String] -> IO (Either String a)

jsonQuery :: FromJSON a => String -> Query a
jsonQuery base query =
   case (parseURI $ base ++ escapeString query) of
     Nothing -> return $ Left "illformed url"
     Just url -> catchIOError execute (return . Left . show)
         where execute = do
                 res <- simpleHTTP (mkJSONRequest url)
                 case res of
                      Left err -> return $ Left (show err)
                      Right res ->  case eitherDecodeStrict (rspBody res) of
                                      Left msg -> return $ Left ("JSON decoding error: " ++ msg ++ "\n" ++ 
                                                   show (rspBody res))
                                      Right res -> return (Right res)
                 

transformQuery :: (a -> b) -> Query a -> Query b
transformQuery f = fmap (fmap (fmap f))

transformQuery' :: (a -> b) -> Query' a -> Query' b
transformQuery' f = fmap (fmap (fmap f))

mkJSONRequest :: URI -> Request ByteString
mkJSONRequest url = setHeaders (mkRequest GET url) jsonHeaders

jsonHeaders :: [Header]
jsonHeaders = [mkHeader HdrContentType "application/json"]

-- | Escapes the string for use in a URL.
escapeString :: String -> String
escapeString = escapeURIString isAlphaNum

-- | Escapes the text for use in a URL.
escapeText :: Text -> Text
escapeText = T.pack . escapeString . T.unpack
