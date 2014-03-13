{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

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

data Icon = FileIcon Text | FileType Text | IconFile Text

xmlIcon :: Icon -> Xml Elem
xmlIcon icon = case icon of
               FileIcon str -> mk str (xattr "type" "fileicon")
               FileType str -> mk str (xattr "type" "filetype")
               IconFile str -> mk str mempty
    where mk :: Text -> Xml Attr -> Xml Elem
          mk str f = xelem "icon" (f <#> xtext str)

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

xmlItems :: [Item] -> Xml Elem
xmlItems = xelem "items" . mconcat . map xmlItem 

renderItems :: [Item] -> ByteString
renderItems  = xrender . xmlItems

printItems :: [Item] -> IO ()
printItems = B.putStr . renderItems

runScript' :: ([String] -> IO a) -> (a -> [Item]) -> IO ()
runScript' runQuery mkItems = do
  args <- getArgs
  res <- runQuery args
  printItems $ mkItems res


runScript :: (String -> IO a) -> (a -> [Item]) -> IO ()
runScript runQuery = runScript' (runQuery . concat . intersperse " ")

data MkSearch = MkSearch {searchURL, found, notFound :: Text -> Text}

mkSearchItems :: MkSearch -> (Text, [Text]) -> [Item]
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

