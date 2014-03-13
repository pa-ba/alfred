module Alfred.Query (jsonQuery,escapeString,escapeText) where

import Network.HTTP
import Network.URI hiding (escapeString)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)


jsonQuery :: FromJSON a => String -> String -> IO a
jsonQuery base query =
   case (parseURI $ base ++ escapeString query) of
     Nothing -> error "illformed url"
     Just url -> do res <- simpleHTTP (mkRequest GET url)
                    case res of
                      Left err -> error (show err)
                      Right res ->  case eitherDecodeStrict (rspBody res :: ByteString) of
                                      Left msg -> error ("JSON decoding error: " ++ msg ++ "\n" ++ 
                                                   show (rspBody res))
                                      Right res -> return res

escapeString :: String -> String
escapeString = escapeURIString isAlphaNum

escapeText :: Text -> Text
escapeText = T.pack . escapeString . T.unpack
