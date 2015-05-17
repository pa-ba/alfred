{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alfred
import Alfred.Query
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Lazy.Encoding as L

runQuery :: Query (Text,[Text])
runQuery = jsonQuery' (L.encodeUtf8 . L.decodeLatin1) suggestURL

suggestURL = "http://google.com/complete/search?client=firefox&q="

mkItems :: Renderer [Text]
mkItems = searchRenderer Search {
            searchURL = \s -> T.concat ["https://www.google.com/search?q=", s],
            notFound = \s -> T.concat ["No suggestion. Google for ", s, "."],
            found = \s -> T.concat ["Search results for ", s]}

main = runScript (transformQuery snd runQuery) mkItems



