{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

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
-- 
-- runQuery :: String -> IO (Text,[Text])
-- runQuery query = jsonQuery suggestURL query
-- 
-- suggestURL = "http://suggestqueries.google.com/complete/search?output=toolbar&client=firefox&hl=en&q="
-- 
-- mkItems :: (Text, [Text]) -> [Item]
-- mkItems = mkSearchItems MkSearch {
--             searchURL = \s -> T.concat ["https://www.google.com/search?q=", s],
--             notFound = \s -> T.concat ["No suggestion. Google for ", s, "."],
--             found = \s -> T.concat ["Search results for ", s]}
-- 
-- main = runScript runQuery mkItems
-- @
-- 
--
--------------------------------------------------------------------------------


module Alfred
    ( Item (..)
    , Icon (..)
    , runScript
    , runScript'
    , mkSearchItems
    , MkSearch (..)) where

import Text.XML.Generator
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Monoid
import System.Environment
import Data.List

import Alfred.Query

-- | This type represents items that should be rendered by Alfred as
-- the result of a script filter.

data Item = Item {
      uid :: Maybe Text
    , arg :: Text
    , isFile :: Bool
    , valid :: Maybe Bool
    , autocomplete :: Maybe Text
    , title :: Text
    , subtitle :: Text
    , icon :: Maybe Icon
}


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


-- | This function runs a script consisting of a query function and a
-- rendering function. The query function takes string parameters and
-- produces an output that is then passed to the rendering function to
-- produce items that are then passed to Alfred.
runScript' :: ([String] -> IO a)  -- ^ query function
           -> (a -> Items)        -- ^ rendering function
           -> IO ()
runScript' runQuery mkItems = do
  args <- getArgs
  res <- runQuery args
  printItems $ mkItems res


-- | This function runs a script consisting of a query function and a
-- rendering function. The query function takes string parameters and
-- produces an output that is then passed to the rendering function to
-- produce items that are then passed to Alfred.
runScript :: (String -> IO a)  -- ^ query function
          -> (a -> Items)      -- ^ rendering function
          -> IO ()
runScript runQuery = runScript' (runQuery . concat . intersperse " ")

-- | This data type represents standard search scripts used by
-- 'mkSearchItems'.
data MkSearch = MkSearch {searchURL, found, notFound :: Text -> Text}


-- | This function produces a rendering function for standard search
-- scripts. For example a Google search rendering function is defined
-- as follows:
-- 
-- @
-- mkItems :: (Text, [Text]) -> Items
-- mkItems = mkSearchItems MkSearch {
--             searchURL = \s -> T.concat ["https://www.google.com/search?q=", s],
--             notFound = \s -> T.concat ["No suggestion. Google for ", s, "."],
--             found = \s -> T.concat ["Search results for ", s]}
-- @
--

mkSearchItems :: MkSearch -> (Text, [Text]) -> Items
mkSearchItems MkSearch {searchURL, found, notFound} suggs = 
    case suggs of
      (s,[]) -> [Item {uid=Nothing,arg=searchURL (escapeText s),isFile=False,
                       valid=Nothing,autocomplete=Nothing,title=s,
                       subtitle=notFound s,icon=Just (IconFile "icon.png")}]
      (s,res@(r:_)) ->  first ++ map mkItem res
         where first = if s == r then []
                       else [Item {uid=Nothing,arg=searchURL (escapeText s),isFile=False,
                              valid=Nothing,autocomplete=Just r, title=s, subtitle=found s,
                              icon=Just (IconFile "icon.png")}]

  where mkItem :: Text -> Item
        mkItem s = Item {uid=Nothing,arg=searchURL (escapeText s),isFile=False,valid=Nothing,
                         autocomplete=Just s, title=s,  subtitle=found s,icon=Just (IconFile "icon.png")}

