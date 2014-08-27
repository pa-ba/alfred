{-# LANGUAGE OverloadedStrings #-}

module Main where

import Alfred
import Alfred.Query
import qualified Data.Text as T
import Data.Text (Text)
import Data.List

runQuery :: Query' (Text,Text) (Text,[Text])
runQuery (country, query) = jsonQuery (suggestURL country) query

suggestURL :: Text -> Text
suggestURL country = T.concat ["http://", country, ".wikipedia.org/w/api.php?action=opensearch&format=json&search="]




mkItems (country, query) = searchRenderer Search {
            searchURL = \s -> T.concat ["http://",country,".wikipedia.org/wiki/Special:Search?search=", s],
            notFound = \s -> T.concat ["No suggestion. Search Wikipedia for ", s, "."],
            found = \s -> T.concat ["Read wikipedia article on ", s]} query

runMyScript :: Query' (Text,Text) a    -- ^ query function
           -> Renderer' (Text,Text) a  -- ^ rendering function
           -> IO ()
runMyScript  = runScript' trans
    where trans (x:xs) = (x, T.concat $ intersperse " " xs)
          trans []     = ("en", "") 

main  = runMyScript (transformQuery snd runQuery) mkItems
