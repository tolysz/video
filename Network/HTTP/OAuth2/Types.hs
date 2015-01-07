{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.HTTP.OAuth2.Types
 ( OAuth2 (..)
 , AuthToken (..)
 , AuthError (..)
 , OAuth2Result (..)
 )
 where

import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Control.Applicative
import Prelude
import Data.Time.Clock (UTCTime (..), getCurrentTime, addUTCTime)
import Data.Text (Text)
import qualified Data.Text as T

import Data.Typeable
import GHC.Generics
import Data.Aeson.TH

import Data.Char (toLower, isUpper)
import Data.Bool


data OAuth2 = OAuth2
  { oauthClientId            :: Text
  , oauthClientSecret        :: Text
  , oauthAuthUri             :: String
  , oauthTokenUri            :: String
  , oauthRevokeUri           :: String
  , oauthRedirectUri         :: Text
  } deriving (Show, Eq, Typeable, Generic)

instance FromJSON OAuth2 where
  parseJSON = genericParseJSON
     (defaultOptions
      { fieldLabelModifier = fromCamel 5
      , fieldsWithDefaults = [ ("partial_match", [| False |]) ]
      })

instance ToJSON OAuth2

-- deriveJSON
--    defaultOptions
--     { fieldLabelModifier = fromCamel 4
--     }
--    ''GGGeometry


data AuthToken = AuthToken
  { atAccessToken  :: Text
  , atTokenType    :: Text
  , atExpiresIn    :: Maybe Int
  , atExpiresAt    :: Maybe UTCTime
  , atIdToken      :: Maybe Text
  , atRefreshToken :: Maybe Text
  } deriving Show

data AuthError = AuthError
 { aeError  :: Text
 , aeStatus :: Maybe Int
 } deriving (Show, Generic)

instance ToJSON AuthError

instance FromJSON AuthError where
   parseJSON (String s) = pure $ AuthError s Nothing
   parseJSON (Object v) =
    AuthError <$>  v .:? "error" .!= (T.pack . show $ v)
              <*> pure Nothing
   parseJSON v = pure $ AuthError (T.pack . show $ v) (Just 500)
     -- fail "Not a AuthError object"

instance FromJSON AuthToken where
   parseJSON (Object v) =
    AuthToken <$>  v .: "access_token"
              <*>  v .: "token_type"
              <*>  v .:? "expires_in"
              <*>  v .:? "expires_at"
              <*>  v .:? "id_token"
              <*>  v .:? "refresh_token"
   parseJSON _ = fail "Not a AuthToken object"
instance Default AuthToken where
 def = AuthToken "" "Bearer" Nothing Nothing Nothing Nothing

type OAuth2Result a = Either AuthError a




fromCamel :: Int -> String -> String
fromCamel n = worker True . drop n
  where
   worker _     []     = []
   worker lastUp (c:cs) =
        (bool
           (bool [c] (['_' , (toLower c)]) (isUpper c))
           [toLower c]
           lastUp
           ) ++ (worker (isUpper c) cs)
