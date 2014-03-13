module Alfred.Query (jsonQuery,escapeString,escapeText) where

import Network.HTTP
import Network.URI hiding (escapeString)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)


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

jsonQuery :: FromJSON a => String -> String -> IO a
jsonQuery base query =
   case (parseURI $ base ++ escapeString query) of
     Nothing -> error "illformed url"
     Just url -> do res <- simpleHTTP (mkJSONRequest url)
                    case res of
                      Left err -> error (show err)
                      Right res ->  case eitherDecodeStrict (rspBody res) of
                                      Left msg -> error ("JSON decoding error: " ++ msg ++ "\n" ++ 
                                                   show (rspBody res))
                                      Right res -> return res

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
