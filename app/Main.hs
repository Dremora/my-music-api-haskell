{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson (Value (Null, Number, String), (.=), object)
import Data.Aeson.Types (toJSON, genericToJSON, defaultOptions)
import Data.Data (Data, Typeable)
import Data.Monoid (mconcat)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.IO (putStrLn)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status201, status500, status400, status401)
import Network.Wai.Middleware.RequestLogger
import Prelude hiding (putStrLn)
import Web.Scotty (ScottyM, ActionM, scotty, scottyOpts, param, raise, json,
  status, finish, jsonData, get, post, put, notFound, defaultHandler,
  middleware, rescue, next, liftAndCatchIO)


import MyMusic.Album (getAlbum, createAlbum, updateAlbum, searchAlbums)
import MyMusic.Auth (authenticate)
import MyMusic.Records (Album)
import MyMusic.Env (getPort)

data AlbumPayload = AlbumPayload {
  album :: Album
} deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, FromJSON)


instance ToJSON AlbumPayload where
  toJSON = genericToJSON defaultOptions


app :: ScottyM ()
app = do
  middleware logStdoutDev

  defaultHandler $ \e -> do
    liftAndCatchIO $ putStrLn e
    status status500
    json $ object ["error" .= String "Something went wrong"]

  put "/session" $ do
    code <- param "code" `rescue` invalidPayload
    authToken <- liftAndCatchIO $ authenticate code
    case authToken of
      Right token -> do
        status status201
        json  $ object ["is_authenticated" .= toJSON True]
      Left _ -> do
        status status401
        json  $ object ["is_authenticated" .= toJSON False]

  get "/albums" $ do
    query <- param "query" `rescue` (\_ -> next)
    albums <- liftAndCatchIO $ searchAlbums query
    json $ object ["albums" .= toJSON albums]

  get "/albums/:id" $ do
    albumId <- param "id"
    albumM <- liftAndCatchIO $ getAlbum albumId
    case albumM of
      Just album -> json $ object ["album" .= album]
      Nothing -> next

  post "/albums" $ do
    (albumPayload :: AlbumPayload) <- jsonData `rescue` invalidPayload
    newAlbum <- liftAndCatchIO $ createAlbum (album albumPayload)
    status status201
    json $ toJSON (AlbumPayload newAlbum)

  put "/albums/:id" $ do
    albumId <- param "id"
    albumPayload <- jsonData `rescue` invalidPayload
    albumM <- liftAndCatchIO $ updateAlbum albumId (album albumPayload)
    case albumM of
      Just updatedAlbum ->
        json $ toJSON (AlbumPayload updatedAlbum)
      Nothing -> do
        status status400
        json $ object ["error" .= String "Album doesn't exist"]

  notFound $ do
    json $ object ["error" .= String "Not Found"]


invalidPayload :: a -> ActionM b
invalidPayload = \_ -> do
  status status400
  json $ object ["error" .= String "Invalid payload"]
  finish


main = do
  port <- getPort
  scotty port app
