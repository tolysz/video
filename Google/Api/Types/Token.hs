{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-

This is basically very manual test. Check following link for details.

Google web oauth: https://developers.google.com/accounts/docs/OAuth2WebServer

Google OAuth 2.0 playround: https://developers.google.com/oauthplayground/

-}

module Google.Api.Types.Token where

import           Data.Aeson.TH                 (defaultOptions, deriveJSON)
import           Data.Text                     (Text)
import           Prelude                       hiding (id)
import qualified Prelude                       as P (id)


data Token = Token { issued_to   :: Text
                   , audience    :: Text
                   , user_id     :: Maybe Text
                   , scope       :: Text
                   , expires_in  :: Integer
                   -- , email          :: Maybe Text
                   -- , verified_email :: Maybe Bool
                   , access_type :: Text
                   } deriving (Show)


$(deriveJSON defaultOptions ''Token)
