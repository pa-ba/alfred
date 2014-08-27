{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Alfred.Query 
    ( jsonQuery
    , jsonQuery'
    , xmlQuery
    , xmlQueryLazy
    , escapeString
    , escapeText
    , Query
    , transformQuery
    , Query'
    ) where

import Network.HTTP
import Network.BufferType
import Network.URI hiding (escapeString)
import Data.Aeson

import qualified Data.Text as T

import Data.Text (Text)
import System.IO.Error
import Data.ByteString

import Text.XML.Expat.Tree

-- | Type representing queries for use in 'Alfred.runScript'.
type Query a = Query' Text a

-- | Alternative type representing queries for use in
-- 'Alfred.runScript''.
type Query' q a = q -> IO (Either Text a)


-- | This function performs a query by performing an HTTP GET request
-- at the url obtained by concatenating the first argument with the
-- second one (after escaping it). The returned query takes a string
-- as an argument and appends it to the base url to obtain the url
-- that is used for the query. The result is then parsed as a JSON
-- object. For example, for a Google search:
-- 
-- @
--  runQuery :: Query (Text,[Text])
--  runQuery = jsonQuery suggestURL
--  
--  suggestURL = "http://google.com/complete/search?client=firefox&q="
-- @
-- 

jsonQuery :: FromJSON a => Text -> Query a
jsonQuery = jsonQuery' id


-- | This function is a variant of 'jsonQuery' that takes a function
-- as an additional argument that is used to transform the raw
-- 'ByteString' that is returned by the query. This can be helpful if
-- the source does not provide valid UTF-8 formatted JSON. For
-- example, for a Google search:
-- 
-- @
--  runQuery :: Query (Text,[Text])
--  runQuery = jsonQuery' (encodeUtf8 . decodeLatin1) suggestURL
--  
--  suggestURL = "http://google.com/complete/search?client=firefox&q="
-- @
-- 


jsonQuery' :: FromJSON a => (ByteString -> ByteString) -> Text -> Query a
jsonQuery' convert = genericQuery mkJSONRequest result 
    where result res = case eitherDecodeStrict (convert $ rspBody res) of
                         Left msg -> return $ Left $ T.concat 
                                     ["JSON decoding error: ", T.pack msg, "\n", T.pack $ show $ rspBody res]
                         Right res -> return (Right res)


-- | Constructions a request for doing an XML query.

mkJSONRequest :: BufferType ty => URI -> Request ty
mkJSONRequest url = setHeaders (mkRequest GET url) jsonHeaders
    where jsonHeaders :: [Header]
          jsonHeaders = [mkHeader HdrContentType "application/json; charset=utf-8"]

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


xmlQuery :: (GenericXMLString a, GenericXMLString b) => Text -> Query (Node a b)
xmlQuery = genericQuery mkXMLRequest result
    where result res = case parse' defaultParseOptions (rspBody res) of
                         Left msg -> return $ Left $ T.concat 
                                     ["XML decoding error: ", T.pack $ show msg ,"\n", T.pack $ show (rspBody res)]
                         Right tree -> return (Right tree)

-- | Lazy variant of 'xmlQueryLazy'. This function may be useful if
-- results tend to be lengthy and only a small prefix of the result is
-- used.
xmlQueryLazy :: (GenericXMLString a, GenericXMLString b) => Text -> Query (Node a b)
xmlQueryLazy = genericQuery mkXMLRequest result
    where result res = let (tree, _) = parse defaultParseOptions (rspBody res) 
                       in  return (Right tree)

-- | Generic function to construct queries.
genericQuery :: HStream ty => (URI -> Request ty)
             -> (Response ty -> IO (Either Text b))
             -> Text -> Query b
genericQuery mkRequest result base query = let urlText = T.concat [base,  escapeText query] in
   case (parseURI $ T.unpack urlText) of
     Nothing -> return $ Left $ T.concat ["illformed url: ", urlText]
     Just url -> catchIOError execute (return . Left . T.pack . show)
         where execute = do
                 res <- simpleHTTP (mkRequest url)
                 case res of
                      Left err -> return $ Left $ T.pack (show err)
                      Right res -> result res



-- | Constructions a request for doing a JSON query.

mkXMLRequest :: BufferType ty => URI -> Request ty
mkXMLRequest url = setHeaders (mkRequest GET url) xmlHeaders
    where xmlHeaders :: [Header]
          xmlHeaders = [mkHeader HdrContentType "application/xml"]


-- | Functorial map for 'Query''.
transformQuery :: (a -> b) -> Query' q a -> Query' q b
transformQuery f = fmap (fmap (fmap f))

-- | Escapes the string for use in a URL.
escapeString :: String -> String
escapeString = escapeURIString isUnescapedInURI

-- | Escapes the text for use in a URL.
escapeText :: Text -> Text
escapeText = T.pack . escapeString . T.unpack
