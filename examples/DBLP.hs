{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alfred
import Alfred.Query
import qualified Data.Text as T
import Data.Text (Text)
import Text.XML.Expat.Tree

runQuery :: Query (Node Text Text)
-- Use the lazy variant since DBLP returns a vast XML document
-- containing all possible matches. 
runQuery query = xmlQueryLazy suggestURL query


suggestURL = "http://dblp.uni-trier.de/search/author?xauthor="

mkItems :: Renderer [Text]
mkItems = searchRenderer Search {
            searchURL = \s -> T.concat ["http://dblp.uni-trier.de/search/author?author=", s],
            notFound = \s -> T.concat ["No suggestion. Search DBLP for ", s, "."],
            suggestError = \s -> T.concat ["Could not load suggestions! Search DBLP for ", s, "."],
            found = \s -> T.concat ["Open bibliography of ", s]}

getAuthors :: Node Text Text -> [Text]
getAuthors Element {eName = "authors", eChildren = cs} =
    take 20 [author | Element{eName = "author", eChildren = [Text author]} <- cs]
getAuthors _ = []

main = runScript (transformQuery getAuthors runQuery) mkItems
