{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Alfred
-- Copyright   :  (c) 2014 Patrick Bahr
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@di.ku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides utility functions to interact with Alfred
-- version 2. It is intended to be used for writing "script filters"
-- used in Alfred workflows.
--
-- For example the following excerpt defines a script for Google
-- search with auto completion:
--
-- @
-- import Alfred
-- import Alfred.Query
-- import qualified Data.Text as T
-- import Data.Text (Text)
-- import Data.Text.Encoding
--
-- runQuery :: Query (Text,[Text])
-- runQuery = jsonQuery suggestURL
--
-- suggestURL = "http://google.com/complete/search?client=firefox&q="
--
-- mkItems :: Renderer [Text]
-- mkItems = searchRenderer Search {
--             searchURL = \s -> T.concat ["https://www.google.com/search?q=", s],
--             notFound = \s -> T.concat ["No suggestion. Google for ", s, "."],
--             found = \s -> T.concat ["Search results for ", s]}
--
-- main = runScript (transformQuery snd runQuery) mkItems
-- @
--
--
--------------------------------------------------------------------------------


module Alfred
    ( Item (..)
    , item
    , Icon (..)
    , Renderer
    , Renderer'
    , runScript
    , runScript'
    , searchRenderer
    , searchRenderer'
    , Search (..)
    , Search' (..)) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.List
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment
import Text.XML.Generator

import Alfred.Query

-- | This type represents items that should be rendered by Alfred as
-- the result of a script filter.

data Item = Item {
      uid          :: Maybe Text
    , arg          :: Text
    , isFile       :: Bool
    , valid        :: Maybe Bool
    , autocomplete :: Maybe Text
    , title        :: Text
    , subtitle     :: Text
    , icon         :: Maybe Icon
}

-- | Default item.
item :: Item
item = Item {uid=Nothing,arg=undefined,isFile=False,valid=Nothing,
                  autocomplete=Nothing,title=undefined, subtitle=undefined,
                  icon=Just (IconFile "icon.png")}

-- | A list of items.
type Items = [Item]

-- | Represents icons of an item.

data Icon = FileIcon Text | FileType Text | IconFile Text


-- | Render an icon as XML element.
xmlIcon :: Icon -> Xml Elem
xmlIcon icon = case icon of
               FileIcon str -> mk str (xattr "type" "fileicon")
               FileType str -> mk str (xattr "type" "filetype")
               IconFile str -> mk str mempty
    where mk :: Text -> Xml Attr -> Xml Elem
          mk str f = xelem "icon" (f <#> xtext str)

-- | Render an item as XML element.
xmlItem :: Item -> Xml Elem
xmlItem (Item  uid arg file val auto title sub icon) =
    xelem "item" $
          (uid' <> xattr "arg" arg <> val' <> auto' <> file') <#>
          (xelemWithText "title" title <> xelemWithText "subtitle" sub <> icon')

        where uid' = case uid of Nothing -> mempty; Just uid -> xattr "uid" uid
              val' = case val of
                       Nothing -> mempty
                       Just val -> xattr "valid" (if val then "yes" else "no")
              file' = if file then xattr "type" "file" else mempty
              auto' = case auto of Nothing -> mempty; Just auto -> xattr "autocomplete" auto
              icon' = case icon of Nothing -> mempty; Just icon -> xmlIcon icon

-- | Render items as an XML element.
xmlItems :: Items -> Xml Elem
xmlItems = xelem "items" . mconcat . map xmlItem


-- | Render items as an XML 'ByteString'.
renderItems :: Items -> ByteString
renderItems  = xrender . xmlItems

-- | Print items as XML to stdout.
printItems :: Items -> IO ()
printItems = B.putStr . renderItems

-- | This type represents rendering functions as used by 'runScript'.
type Renderer a = Renderer' Text a

-- | This type represents rendering functions as used by 'runScript''.
type Renderer' q a = (q -> Either Text a -> Items)

-- | This function runs a script consisting of a query function and a
-- rendering function. The query function takes string parameters and
-- produces an output that is then passed to the rendering function to
-- produce items that are then passed to Alfred.
runScript' :: ([Text] -> q)
           -> Query' q a    -- ^ query function
           -> Renderer' q a  -- ^ rendering function
           -> IO ()
runScript' inp runQuery mkItems = do
  args <- (inp . map (T.pack . umlaut)) <$> getArgs
  res <- runQuery args
  printItems $ mkItems args res


-- | Normalise strange umlauts.
umlaut :: String -> String
umlaut [] = []
umlaut ('o':'\776':r) = ('\246' : umlaut r)
umlaut ('O':'\776':r) = ('\214' : umlaut r)
umlaut ('u':'\776':r) = ('\252' : umlaut r)
umlaut ('U':'\776':r) = ('\220' : umlaut r)
umlaut ('a':'\776':r) = ('\228' : umlaut r)
umlaut ('A':'\776':r) = ('\196' : umlaut r)
umlaut ('a':'\778':r) = ('\229' : umlaut r)
umlaut ('A':'\778':r) = ('\197' : umlaut r)
umlaut (x:r) = x : umlaut r


-- | This function runs a script consisting of a query function and a
-- rendering function. The query function takes string parameters and
-- produces an output that is then passed to the rendering function to
-- produce items that are then passed to Alfred.
runScript :: Query a    -- ^ query function
          -> Renderer a -- ^ rendering function
          -> IO ()
runScript = runScript' (T.concat . intersperse " ")

-- | This data type represents standard search scripts used by
-- 'searchRenderer'.
data Search a = Search {searchURL, notFound :: Text -> Text, found :: a -> Text}


-- | This data type represents advanced standard search scripts used
-- by 'searchRenderer''.
data Search' a = Search' {simpleSearch :: Search a,
                          resultURL :: a -> Text, resultTitle :: a -> Text}


-- | This function produces a rendering function for standard search
-- scripts. For example a Google search rendering function is defined
-- as follows:
--
-- @
--  mkItems :: Renderer [Text]
--  mkItems = searchRenderer Search {
--              searchURL = \s -> T.concat ["https://www.google.com/search?q=", s],
--              notFound = \s -> T.concat ["No suggestion. Google for ", s, "."],
--              found = \s -> T.concat ["Search results for ", s]}
-- @
--

searchRenderer :: Search Text -> Renderer [Text]
searchRenderer s = searchRenderer' Search' { simpleSearch = s, resultURL = searchURL s . escapeText
                                           , resultTitle = id}


-- | This function produces a rendering function for standard search
-- scripts. As opposed to the simpler variant 'searchRenderer', this
-- function works on arbitrary query result types. For example a DBLP
-- search rendering function is defined as follows:
--
-- @
--  mkItems :: Renderer [(Text, Text)]
--  mkItems = searchRenderer' Search'{
--              simpleSearch = Search {
--                searchURL = \s -> T.concat ["http://dblp.uni-trier.de/search/author?author=", s],
--                notFound = \s -> T.concat ["No suggestion. Search DBLP for ", s, "."],
--                found = \s -> T.concat ["Open bibliography of ", s]},
--              resultURL = \(_,r) -> T.concat ["http://dblp.uni-trier.de/pers/hd/",r,".html"],
--              resultTitle = fst}
-- @
--
-- In the above example the query result type is @(Text,Text)@ where
-- the first component is the name of the result and the second
-- component is used to construct a URL that leads directly to the
-- search result.


searchRenderer' :: Search' a -> Renderer [a]
searchRenderer' Search' {simpleSearch = Search {searchURL, found, notFound}, resultURL, resultTitle} s res =
    case res of
      (Right suggs) -> case suggs of
          [] -> [Item {uid=Nothing,arg=searchURL2 (escapeText s),isFile=False,
                       valid=Nothing,autocomplete=Nothing,title=s,
                       subtitle=notFound s,icon=Just (IconFile "icon.png")}]
          res ->  map mkItem res

      (Left err) -> [Item {uid=Nothing,arg=searchURL2 (escapeText s),isFile=False,
                           valid=Nothing,autocomplete=Nothing,title= s,
                           subtitle=T.concat ["Error: ", err],icon=Just (IconFile "icon.png")}]
  where mkItem t = Item {uid=Nothing,arg=arg,isFile=False,valid=Nothing,
                         autocomplete=Just t', title=t',  subtitle=found t,icon=Just (IconFile "icon.png")}
            where arg   = T.concat ["\"",resultURL t,"\" \"", searchURL (escapeText s), "\""]
                  t' = resultTitle t
        searchURL2 s = T.concat ["\"",url,"\" \"", url, "\""]
            where url = searchURL s
