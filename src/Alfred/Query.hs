{-# LANGUAGE TypeSynonymInstances #-}

module Alfred.Query 
    ( jsonQuery
    , escapeString
    , escapeText
    , Query
    , transformQuery
    , Query'
    , transformQuery'
    ) where

import Network.HTTP
import Network.URI hiding (escapeString)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import System.IO.Error

-- | Type representing queries for use in 'Alfred.runScript'.
type Query a = String -> IO (Either String a)

-- | Alternative type representing queries for use in
-- 'Alfred.runScript''.
type Query' a = [String] -> IO (Either String a)


-- | This function performs a query by performing an HTTP GET request
-- at the url obtained by concatenating the first argument with the
-- second one (after escaping it). The returned query takes a string
-- as an argument and appends it to the base url to obtain the url
-- that is used for the query. The result is then parsed as a JSON
-- object. For example, for a Google search:
-- 
-- @
-- runQuery :: String -> IO (Text,[Text])
-- runQuery query = jsonQuery suggestURL query
-- 
-- suggestURL = "http://suggestqueries.google.com/complete/search?output=toolbar&client=firefox&hl=en&q="
-- @
-- 

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


-- | Constructions a request for doing a JSON query.
mkJSONRequest :: URI -> Request ByteString
mkJSONRequest url = setHeaders (mkRequest GET url) jsonHeaders
    where jsonHeaders :: [Header]
          jsonHeaders = [mkHeader HdrContentType "application/json"]

                 
-- | Functorial map for 'Query'.
transformQuery :: (a -> b) -> Query a -> Query b
transformQuery f = fmap (fmap (fmap f))

-- | Functorial map for 'Query''.
transformQuery' :: (a -> b) -> Query' a -> Query' b
transformQuery' f = fmap (fmap (fmap f))

-- | Escapes the string for use in a URL.
escapeString :: String -> String
escapeString = escapeURIString isAlphaNum

-- | Escapes the text for use in a URL.
escapeText :: Text -> Text
escapeText = T.pack . escapeString . T.unpack
