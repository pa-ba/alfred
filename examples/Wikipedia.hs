{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alfred
import Alfred.Query
import qualified Data.Text as T
import Data.Text (Text)

runQuery :: Query (Text,[Text])
runQuery query = jsonQuery suggestURL query

suggestURL = "http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search="


mkItems :: Text -> Either String [Text] -> [Item]
mkItems = searchRenderer Search {
            searchURL = \s -> T.concat ["http://en.wikipedia.org/wiki/Special:Search?search=", s],
            notFound = \s -> T.concat ["No suggestion. Search Wikipedia for ", s, "."],
            suggestError = \s -> T.concat ["Could not load suggestions! Search Wikipedia for ", s, "."],
            found = \s -> T.concat ["Read wikipedia article on ", s]}

main = runScript (transformQuery snd runQuery) mkItems
