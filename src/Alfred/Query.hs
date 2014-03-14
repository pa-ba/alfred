{-# LANGUAGE TypeSynonymInstances #-}

module Alfred.Query 
    ( jsonQuery
    , xmlQuery
    , xmlQueryLazy
    , escapeString
    , escapeText
    , Query
    , transformQuery
    , Query'
    , transformQuery'
    ) where

import Network.HTTP
import Network.BufferType
import Network.URI hiding (escapeString)
import Data.Aeson
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)
import System.IO.Error

import Text.XML.Expat.Tree

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
jsonQuery = genericQuery mkJSONRequest result 
    where result res = case eitherDecodeStrict (rspBody res) of
                         Left msg -> return $ Left ("JSON decoding error: " ++ msg ++ "\n" ++ 
                                                   show (rspBody res))
                         Right res -> return (Right res)


-- | Constructions a request for doing an XML query.

mkJSONRequest :: BufferType ty => URI -> Request ty
mkJSONRequest url = setHeaders (mkRequest GET url) jsonHeaders
    where jsonHeaders :: [Header]
          jsonHeaders = [mkHeader HdrContentType "application/json"]

-- | This function performs a query by performing an HTTP GET request
-- at the url obtained by concatenating the first argument with the
-- second one (after escaping it). The returned query takes a string
-- as an argument and appends it to the base url to obtain the url
-- that is used for the query. The result is then parsed as an XML
-- document. For example, for a DBLP search:
--
-- @
-- runQuery :: Query (Node Text Text)
-- runQuery query = xmlQuery suggestURL query
-- 
-- suggestURL = "http://dblp.uni-trier.de/search/author?xauthor="
-- @
-- 


xmlQuery :: (GenericXMLString a, GenericXMLString b) => String -> Query (Node a b)
xmlQuery = genericQuery mkXMLRequest result
    where result res = case parse' defaultParseOptions (rspBody res) of
                         Left msg -> return $ Left ("XML decoding error: " ++ show msg 
                                                    ++ "\n" ++ show (rspBody res))
                         Right tree -> return (Right tree)

-- | Lazy variant of 'xmlQueryLazy'. This function may be useful if
-- results tend to be lengthy and only a small prefix of the result is
-- used.
xmlQueryLazy :: (GenericXMLString a, GenericXMLString b) => String -> Query (Node a b)
xmlQueryLazy = genericQuery mkXMLRequest result
    where result res = let (tree, _) = parse defaultParseOptions (rspBody res) 
                       in  return (Right tree)

-- | Generic function to construct queries.
genericQuery :: HStream ty => (URI -> Request ty)
             -> (Response ty -> IO (Either String b))
             -> String -> Query b
genericQuery mkRequest result base query =
   case (parseURI $ base ++ escapeString query) of
     Nothing -> return $ Left "illformed url"
     Just url -> catchIOError execute (return . Left . show)
         where execute = do
                 res <- simpleHTTP (mkRequest url)
                 case res of
                      Left err -> return $ Left (show err)
                      Right res -> result res



-- | Constructions a request for doing a JSON query.

mkXMLRequest :: BufferType ty => URI -> Request ty
mkXMLRequest url = setHeaders (mkRequest GET url) xmlHeaders
    where xmlHeaders :: [Header]
          xmlHeaders = [mkHeader HdrContentType "application/xml"]

                 
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
