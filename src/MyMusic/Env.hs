module MyMusic.Env where

import Data.Maybe (fromMaybe)
import Network.URI (URI(..), URIAuth(..))
import System.Environment (getEnv, lookupEnv)
import Text.Read (readMaybe)


getPort :: IO Int
getPort = do
  port <- lookupEnv "PORT"
  return $ fromMaybe 4200 (port >>= readMaybe)


getElasticsearchURL :: IO String
getElasticsearchURL = getEnv "ELASTICSEARCH_URL"


getCouchURI :: IO URI
getCouchURI = do
  protocol <- getEnv "COUCHDB_PROTOCOL"
  host <- getEnv "COUCHDB_HOST"
  port <- getEnv "COUCHDB_PORT"
  username <- getEnv "COUCHDB_USERNAME"
  password <- getEnv "COUCHDB_PASSWORD"

  return $ URI {
    uriScheme = protocol ++ ":",
    uriAuthority = Just $ URIAuth {
      uriUserInfo = username ++ ":" ++ password ++ "@",
      uriRegName = host,
      uriPort = ":" ++ port },
    uriFragment = "",
    uriPath = "",
    uriQuery = ""
    }
