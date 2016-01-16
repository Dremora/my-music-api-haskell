{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyMusic.Album
  ( getAlbum
  , createAlbum
  , updateAlbum
  , searchAlbums
  ) where

import Control.Monad.IO.Class (liftIO)
import Database.CouchDB (getDoc, getAndUpdateDoc, newDoc, doc, runCouchDBURI, db,
  CouchMonad, Rev, Doc)
import Control.Monad (void)
import Data.Aeson.Types (fromJSON, toJSON, Result(Error, Success))
import Data.Aeson.Compatibility
import Data.Maybe (catMaybes)
import Data.Text (Text, pack, unpack)
import Database.Bloodhound.Client (searchByIndex, mkSearch, withBH,
  parseEsResponse, indexDocument)
import Database.Bloodhound.Types (Query(QueryMultiMatchQuery), MultiMatchQuery,
  multiMatchQueryOperator, multiMatchQueryType, multiMatchQueryLenient, MultiMatchQueryType(MultiMatchCrossFields),
  size, Size(Size), SearchResult, EsError, BooleanOperator(And), ZeroTermsQuery(ZeroTermsNone),
  MultiMatchQueryType(MultiMatchCrossFields), FieldName(FieldName), IndexName(IndexName),
  mkMultiMatchQuery, QueryString(QueryString), Server(Server), Lenient(Lenient),
  hitSource, hits, searchHits, unpackId, hitDocId, MappingName(MappingName),
  defaultIndexDocumentSettings, DocId(DocId))
import Network.HTTP.Client (defaultManagerSettings)

import MyMusic.Records (Album(albumId), AlbumID, FirstPlayed)
import MyMusic.Env (getCouchURI, getElasticsearchURL)


albumsDb = db "albums"

musicIndex = IndexName "music"
albumMapping = MappingName "album"


runCouchDB :: CouchMonad a -> IO a
runCouchDB monad = do
  couchURI <- getCouchURI
  runCouchDBURI couchURI monad


getAlbum :: AlbumID -> IO (Maybe Album)
getAlbum docId = runCouchDB $ do
  maybeDoc <- getDoc albumsDb (doc $ unpack docId)
  return $ do
    (_, _, album) <- maybeDoc
    return $ album { albumId = Just docId }


createAlbum :: Album -> IO Album
createAlbum album = runCouchDB $ do
  (docId, _) <- newDoc albumsDb album
  liftIO $ runBH' $ indexDocument musicIndex albumMapping
    defaultIndexDocumentSettings album (DocId $ pack $ show docId)
  return $ album { albumId = Just $ pack $ show docId }


updateAlbum :: AlbumID -> Album -> IO (Maybe Album)
updateAlbum albumId_ album = runCouchDB $ do
  rev <- getAndUpdateDoc albumsDb (doc $ unpack albumId_) (\_ -> return album)
  liftIO $ runBH' $ indexDocument musicIndex albumMapping
    defaultIndexDocumentSettings album (DocId $ albumId_)
  return (rev >> Just (album { albumId = Just albumId_ }))


runBH' monad = do
  server <- getElasticsearchURL
  withBH defaultManagerSettings ((Server . pack) server) monad


searchAlbums :: Text -> IO [Album]
searchAlbums queryString = do
  let fields = map FieldName ["artist", "title", "year"]
  let query = QueryMultiMatchQuery $ (mkMultiMatchQuery fields (QueryString queryString)) {
      multiMatchQueryOperator = And
    , multiMatchQueryType = Just MultiMatchCrossFields
    , multiMatchQueryLenient = Just (Lenient True)
    }
  let search = (mkSearch (Just query) Nothing) { size = Size 50 }
  reply <- runBH' $ searchByIndex musicIndex search
  (parsedReply :: Either EsError (SearchResult Album)) <- parseEsResponse reply
  case parsedReply of
    Left _ -> return []
    Right results ->
      let
        extractAlbum hit = hitSource hit >>= \source -> Just $ source { albumId = Just $ unpackId $ hitDocId hit }
      in
        return . catMaybes $ map extractAlbum ((hits . searchHits) results)
