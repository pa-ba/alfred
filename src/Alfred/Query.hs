{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Alfred.Query
-- Copyright   :  (c) 2014 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@di.ku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides utility functions to query web APIs. These
-- queries can then be used to feed Alfred with suggestions.
--
--------------------------------------------------------------------------------

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

import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Types hiding (Query)

import Network.URI hiding (escapeString)

import qualified Data.Text as T

import Data.ByteString.Lazy
import Data.Text (Text)
import System.IO.Error

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
    where result res = case eitherDecode (convert $ responseBody res) of
                         Left msg -> return $ Left $ T.concat
                                     ["JSON decoding error: ", T.pack msg, "\n", T.pack $ show $ responseBody res]
                         Right res -> return (Right res)


-- | Constructions a request for doing an XML query.

mkJSONRequest :: Request -> Request
mkJSONRequest req = req {requestHeaders=jsonHeaders}
    where jsonHeaders :: [Header]
          jsonHeaders = (hContentType, "application/json; charset=utf-8")  : requestHeaders req

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
    where result res = let (tree, err) = parse defaultParseOptions (responseBody res) in
                        case err of
                         Just msg -> return $ Left $ T.concat
                                     ["XML decoding error: ", T.pack $ show msg ,"\n", T.pack $ show (responseBody res)]
                         Nothing -> return (Right tree)

-- | Lazy variant of 'xmlQueryLazy'. This function may be useful if
-- results tend to be lengthy and only a small prefix of the result is
-- used.
xmlQueryLazy :: (GenericXMLString a, GenericXMLString b) => Text -> Query (Node a b)
xmlQueryLazy = genericQuery mkXMLRequest result
    where result res = let (tree, _) = parse defaultParseOptions (responseBody res)
                       in  return (Right tree)

-- | Generic function to construct queries.
genericQuery :: (Request -> Request)
             -> (Response ByteString -> IO (Either Text b))
             -> Text -> Query b
genericQuery modRequest result base query = let urlText = T.concat [base,  escapeText query] in
   case (parseUrl $ T.unpack urlText) of
     Nothing -> return $ Left $ T.concat ["illformed url: ", urlText]
     Just req -> catchIOError execute (return . Left . T.pack . show)
         where execute = withManager (\man -> (httpLbs (modRequest req) man)) >>= result


-- | Constructions a request for doing a JSON query.

mkXMLRequest :: Request -> Request
mkXMLRequest req = req {requestHeaders=xmlHeaders}
    where xmlHeaders :: [Header]
          xmlHeaders = (hContentType, "application/xml") : requestHeaders req


-- | Functorial map for 'Query''.
transformQuery :: (a -> b) -> Query' q a -> Query' q b
transformQuery f = fmap (fmap (fmap f))

-- | Escapes the string for use in a URL.
escapeString :: String -> String
escapeString = escapeURIString isUnescapedInURI

-- | Escapes the text for use in a URL.
escapeText :: Text -> Text
escapeText = T.pack . escapeString . T.unpack
