{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Aeson.Compatibility () where

import Data.Aeson (Value(Array, Null, Bool, Number, String, Object),
  ToJSON, FromJSON, toJSON, parseJSON, fromJSON)
import qualified Data.Aeson as Aeson (Result(Success, Error))
import Data.Function ((&))
import Text.JSON (JSON, readJSON, showJSON)
import qualified Text.JSON as TextJSON (Result(Ok, Error))
import Text.JSON.Types (JSValue(JSNull, JSBool, JSString, JSRational, JSArray, JSObject),
  fromJSString, toJSString, fromJSObject, toJSObject)
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap


jsValueToJSON :: JSValue -> Value
jsValueToJSON JSNull =
  Null
jsValueToJSON (JSBool bool) =
  Bool bool
jsValueToJSON (JSString string) =
  String $ T.pack $ fromJSString string
jsValueToJSON (JSRational _ rational) =
  Number $ fromRational rational
jsValueToJSON (JSArray array) =
  Array $ Vector.fromList $ map jsValueToJSON array
jsValueToJSON (JSObject object) =
  object
  & fromJSObject
  & map (\(k, v) -> (T.pack k, jsValueToJSON v))
  & HashMap.fromList
  & Object


jsonToJsValue :: Value -> JSValue
jsonToJsValue Null =
  JSNull
jsonToJsValue (Bool bool) =
  JSBool bool
jsonToJsValue (String string) =
  JSString $ toJSString $ T.unpack string
jsonToJsValue (Number scientific) =
  JSRational False $ toRational scientific
jsonToJsValue (Array array) =
  array
  & Vector.toList
  & map jsonToJsValue
  & JSArray
jsonToJsValue (Object object) =
  object
  & HashMap.toList
  & map (\(k, v) -> (T.unpack k, jsonToJsValue v))
  & toJSObject
  & JSObject


convertResult :: Aeson.Result a -> TextJSON.Result a
convertResult (Aeson.Success value) = TextJSON.Ok value
convertResult (Aeson.Error error) = TextJSON.Error error


instance (ToJSON a, FromJSON a) => JSON a where
  readJSON = convertResult . fromJSON . jsValueToJSON
  showJSON = jsonToJsValue . toJSON
