{-# LANGUAGE OverloadedStrings,TupleSections #-}

module Main where

import Alfred (Item (..), Icon (..), item, runScript, Renderer)
import Alfred.Query
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson hiding (Result)
import Control.Applicative
import Control.Monad
import Data.Char

data Result = Result {
      location :: Text
    , self :: Text
    , docs :: Text}
            deriving Show
newtype Results = Results [Result] deriving Show

instance FromJSON Result where
    parseJSON (Object v) = Result  <$>
                          v .: "location" <*>
                          v .: "self" <*>
                          v .: "docs"
    parseJSON _ = mzero

instance FromJSON Results where
    parseJSON (Object v) = Results <$> v .: "results"
    parseJSON _ = mzero

runQuery :: Query Results
runQuery query = jsonQuery suggestURL query

suggestURL = "http://www.haskell.org/hoogle/?mode=json&hoogle="
searchURL s = T.concat ["http://www.haskell.org/hoogle/?q=", escapeText s]
suggestError s = T.concat ["Could not load suggestions! Search Hoogle for ", s, "."]
notFound s = T.concat ["No suggestion. Search Hoogle for ", s, "."]
searchText s = T.concat ["Search Hoogle for ", s, "."]
found s = T.concat ["Read documentation on ", s]


searchURL2 s = T.concat ["\"",url,"\" \"", url, "\""]
    where url = searchURL s


mkItems :: Renderer Results
mkItems s (Left _) = [item {arg=searchURL2 s,title=s, subtitle=suggestError s,
                        icon=Just (IconFile "notfound.png")}]
mkItems s (Right (Results res)) = case res of 
           [] -> [item {arg=searchURL2 s,title=s,subtitle=notFound s,
                        icon=Just (IconFile "notfound.png")}]
           res -> map (mkItem s) res


mkItem :: Text -> Result -> Item
mkItem s r = item {arg=arg,autocomplete=auto,
                   title=self r, subtitle=docs r,icon=Just (IconFile "icon.png")}
    where auto = case T.breakOnAll s (self r) of
                   [] -> Nothing
                   (_,suf):_ -> Just $ T.takeWhile (not . isSpace) suf
          arg = T.concat ["\"",location r,"\" \"", searchURL s, "\""]

                 


main = runScript runQuery mkItems
