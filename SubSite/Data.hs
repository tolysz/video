{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE NoCPP           #-}

module SubSite.Data where

import           Yesod
import           Prelude
-- import           Types
import           Network.HTTP.Conduit (Manager)

import Data.String.QM
import Data.Text.Lazy.Builder (fromLazyText)
import qualified Data.Text.Lazy as TL

import Text.Julius

data OAuth2App = OAuth2App

mkYesodSubData "OAuth2App" [parseRoutes|
/ RootOAuth2R
/google/debug    GoogleDebugR
/google/callback GoogleCallbackR
/google/manage   GoogleManageR
/google/login    GoogleOAuthLoginR
/google/logout   GoogleOAuthLogoutR
!/*Texts  Mo404R
|]
