{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alfred
import Alfred.Query
import qualified Data.Text as T
import Data.Text (Text)
import Text.XML.Expat.Tree
import Data.Maybe

runQuery :: Query (Node Text Text)
-- Use the lazy variant since DBLP returns a vast XML document
-- containing all possible matches. 
runQuery query = xmlQueryLazy suggestURL query


suggestURL = "http://dblp.uni-trier.de/search/author?xauthor="

mkItems :: Renderer [(Text, Text)]
mkItems = searchRenderer' Search'{ 
            simpleSearch = Search {
              searchURL = \s -> T.concat ["http://dblp.uni-trier.de/search/author?author=", s],
              notFound = \s -> T.concat ["No suggestion. Search DBLP for ", s, "."],
              found = \(s,_) -> T.concat ["Open bibliography of ", s]},
            resultURL = \(_,r) -> T.concat ["http://dblp.uni-trier.de/pers/hd/",r,".html"],
            resultTitle = fst}
                                   

getAuthors :: Node Text Text -> [(Text,Text)]
getAuthors Element {eName = "authors", eChildren = cs} =
    take 20 [(author,res) | Element{eName = "author", eChildren = chs,eAttributes=atts} <- cs,
             let author = T.concat $ map (\(Text t) -> t) chs,
                 res <- maybeToList (lookup "urlpt" atts)]
getAuthors _ = []

main = runScript (transformQuery getAuthors runQuery) mkItems
