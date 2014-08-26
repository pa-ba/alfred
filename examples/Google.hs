{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alfred
import Alfred.Query
import qualified Data.Text as T
import Data.Text (Text)

runQuery :: Query (Text,[Text])
runQuery query = jsonQuery suggestURL query

suggestURL = "http://suggestqueries.google.com/complete/search?output=toolbar&client=firefox&hl=en&q="

mkItems :: Renderer [Text]
mkItems = searchRenderer Search {
            searchURL = \s -> T.concat ["https://www.google.com/search?q=", s],
            notFound = \s -> T.concat ["No suggestion. Google for ", s, "."],
            suggestError = \s -> T.concat ["Could not load suggestions! Google for ", s, "."],
            found = \s -> T.concat ["Search results for ", s]}

main = runScript (transformQuery snd runQuery) mkItems



