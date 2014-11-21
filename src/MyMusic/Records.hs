{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module MyMusic.Records (Album(..), AlbumID, FirstPlayed) where

import Data.Data (Data, Typeable)
import Data.Foldable (asum)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, withText, withScientific)
import Data.Aeson.Types (genericToJSON, genericParseJSON, defaultOptions
  , omitNothingFields, fieldLabelModifier, constructorTagModifier, camelTo2)
import Data.Text (Text, pack)


-- TODO
-- (?) empty strings: comments, mbid, tag_issues -- remove all
-- null: comments, first_played -- remove all
-- title, tag_issues, edition: convert all to strings
-- MBID type


data Album = Album {
  albumId :: Maybe AlbumID
, title :: StringOrNumber
, artist :: Text
, year :: Maybe Int
, comments :: Maybe Text
, firstPlayed :: Maybe FirstPlayed
, sources :: [Source]
} deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)


albumJSONOptions = defaultOptions {
  omitNothingFields = True,
  fieldLabelModifier = \s -> case s of
    "albumId" -> "id"
    _ -> camelTo2 '_' s
}

type AlbumID = Text


instance ToJSON Album where
  toJSON = genericToJSON albumJSONOptions

instance FromJSON Album where
  parseJSON = genericParseJSON albumJSONOptions


data FirstPlayed =
  Timestamp Int | Date Int Int Int | Unknown deriving (Eq, Ord, Show, Read, Data)

instance ToJSON FirstPlayed where
  toJSON (Timestamp t) = toJSON t
  toJSON (Date year month day) = toJSON [year, month, day]
  toJSON Unknown = toJSON ([] :: [Int])

instance FromJSON FirstPlayed where
  parseJSON value = asum [
    fmap Timestamp $ parseJSON value,
    fmap (\(y, m, d) -> Date y m d) $ parseJSON value,
    fmap (\() -> Unknown) $ parseJSON value ]


type MBID = Text

data Source = Source {
  accurateRip :: Maybe Text,
  sourceComments :: Maybe Text,
  cueIssues :: Maybe Text,
  discs :: Maybe Int,
  download :: Maybe Download,
  edition :: Maybe StringOrNumber,
  format :: Maybe Format,
  location :: Location,
  mbid :: Maybe MBID,
  tagIssues :: Maybe StringOrNumber
} deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

sourceJSONOptions = defaultOptions {
  omitNothingFields = True,
  fieldLabelModifier = \s -> case s of
    "sourceComments" -> "comments"
    _ -> camelTo2 '_' s
}

instance ToJSON Source where
  toJSON = genericToJSON sourceJSONOptions

instance FromJSON Source where
  parseJSON = genericParseJSON sourceJSONOptions


data Download = Download Text | Downloads [Text] deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance ToJSON Download where
  toJSON (Download string) = toJSON string
  toJSON (Downloads array) = toJSON array

instance FromJSON Download where
  parseJSON value = asum [
    fmap Download $ parseJSON value,
    fmap Downloads $ parseJSON value ]

data Format = MP3 | TAK | APE | MPC | FLAC | Mixed deriving (Eq, Ord, Show, Read, Typeable, Data, Generic, FromJSON)

instance ToJSON Format where
  toJSON = genericToJSON defaultOptions


data Location = Foobar2000 | GoogleMusic | Spotify | AppleMusic deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

locationJSONOptions = defaultOptions {
  constructorTagModifier = camelTo2 '-'
}

instance ToJSON Location where
  toJSON = genericToJSON locationJSONOptions

instance FromJSON Location where
  parseJSON = genericParseJSON locationJSONOptions


data StringOrNumber = StringOrNumber Text deriving (Eq, Ord, Show, Read, Typeable, Data, Generic)

instance ToJSON StringOrNumber where
  toJSON (StringOrNumber string) = toJSON string

instance FromJSON StringOrNumber where
  parseJSON value = asum [
      withText "String" (return . StringOrNumber) value,
    withScientific "Number" (return . StringOrNumber . pack . show . round) value ]
