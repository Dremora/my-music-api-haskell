{-# LANGUAGE OverloadedStrings #-}

module MyMusic.Auth (authenticate) where

import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client (HttpException(StatusCodeException))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.OAuth.OAuth2.Internal (OAuth2(OAuth2))
import qualified Network.OAuth.OAuth2.Internal as OAuth
import qualified Network.OAuth.OAuth2.HttpClient as HttpClient
import Data.ByteString (ByteString, pack)
import Control.Exception (handleJust)

facebookKey :: OAuth2
facebookKey = OAuth2 { OAuth.oauthClientId = "560044377462970"
                     , OAuth.oauthClientSecret = "bb409c2c395126ea8c9290c3f18fb5cf"
                     , OAuth.oauthCallback = Just "http://localhost:4200/"
                     , OAuth.oauthOAuthorizeEndpoint = "https://www.facebook.com/dialog/oauth"
                     , OAuth.oauthAccessTokenEndpoint = "https://graph.facebook.com/v2.3/oauth/access_token"
                     }




authenticate :: ByteString -> IO (OAuth.OAuth2Result ByteString)
authenticate code = do
  manager <- HTTP.newManager tlsManagerSettings
  token <- handleJust catchHttpException handler (HttpClient.fetchAccessToken manager facebookKey code)
  return (token >>= return . OAuth.accessToken)


catchHttpException :: HttpException -> Maybe ()
catchHttpException (StatusCodeException status headers _) = Just ()
catchHttpException _ = Nothing


-- handler :: () -> IO ()
handler _ = Left (pack "foo")
