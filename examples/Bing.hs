{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alfred
import Alfred.Query
import qualified Data.Text as T
import Data.Text (Text)


runQuery :: Query (Text,[Text])
runQuery query = jsonQuery suggestURL query

suggestURL = "http://api.bing.com/osjson.aspx?query="

mkItems :: Renderer [Text]
mkItems = searchRenderer Search {
            searchURL = \s -> T.concat ["http://www.bing.com/search?q=", s],
            notFound = \s -> T.concat ["No suggestion. Bing for ", s, "."],
            found = \s -> T.concat ["Search results for ", s]}

main = runScript (transformQuery snd runQuery) mkItems



