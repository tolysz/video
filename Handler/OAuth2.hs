
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE LambdaCase            #-}

module Handler.OAuth2 where

import Network.HTTP.OAuth2.Types
import Types
import Text.Hamlet (hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Maybe (fromJust)
import Data.String.QM
import Control.Monad ((>=>))
import Import
import Data.Time.Clock ( diffUTCTime )

import qualified Data.Aeson.Types as DA
import qualified Data.Text as T
import qualified Data.List as DL

import Network.HTTP.OAuth2

type ApiReq a = Handler (TC a)

instance FromJSON a => IsString (Handler (TC a)) where
  fromString = getQueryOA'
-- This simplyfies requesting data to suplying url with the appriopirate types
-- example how monad is an onion and changing boundaries changes meaning

handleGoogleCallbackR :: Handler Html
handleGoogleCallbackR = do
  codeMaybe <- lookupGetParam "code"
  errorMaybe <- lookupGetParam "error"
  case (codeMaybe, errorMaybe) of
    (Just c, _)       -> processTokenOU (\mgr gc -> fetchAccessToken mgr gc c)
                          >> redirect HomeR
    (_, Just e)       -> traceHTML e
    (Nothing,Nothing) -> redirect HomeR

handleGoogleManageR :: Handler ()
handleGoogleManageR = redirect ("https://security.google.com/settings/security/permissions?pli=1" :: String)

handleGoogleOAuthLogoutR :: Handler ()
handleGoogleOAuthLogoutR = -- do
   -- revoke token somehow else
   void . runDB . deleteBy . OnePerRealm =<< getUserIdent

handleGoogleOAuthLoginR :: Handler ()
handleGoogleOAuthLoginR = do
         gc <- googleKey
         redirect $
           generateAuthUrl
             gc
             (Scope ["https://www.googleapis.com/auth/youtubepartner", "https://www.googleapis.com/auth/youtube.upload", "https://www.googleapis.com/auth/youtube"])
             False

getHttpManager' :: Handler Manager
getHttpManager' = appHttpManager <$> getYesod

-- And we'll spell out the handler type signature.
handleRootOAuth2R :: Handler ()
handleRootOAuth2R = return ()

googleKey :: Handler OAuth2
googleKey = do
     render <- getUrlRender
     Just (OAuth2Google {..}) <- appGoogleWebAppOAuth . appSettings <$> getYesod
     return OAuth2 { oauthClientId     = gaClientId
                   , oauthClientSecret = gaClientSecret
                   , oauthRedirectUri  = render GoogleCallbackR
                   , oauthAuthUri      = "https://accounts.google.com/o/oauth2/auth"
                   , oauthTokenUri     = "https://accounts.google.com/o/oauth2/token"
                   , oauthRevokeUri    = "https://accounts.google.com/o/oauth2/revoke?token="
                   }

getToken :: Handler AuthToken
getToken = do
  now <- liftIO getCurrentTime
  (runDB . getBy . OnePerRealm =<< getUserIdent) >>= \case
      Just (Entity _ (OAuthAccess{..}))      -- we have the entry inside DB
        | Just ex  <- oAuthAccessExpires     -- it has expires field
        , diffUTCTime ex now > 10            -- it still has at least 10s on the clock
        , Just at <- oAuthAccessAccessToken  -- and we have the access token
          -> return def{atAccessToken=at}    -- thus we use it

      Just (Entity _ (OAuthAccess{..}))      -- we have the entry inside DB
        | Just _ <- oAuthAccessRefreshToken  -- we have the refresh tocken there
          -> refreshTokenOU def{atRefreshToken=oAuthAccessRefreshToken} >> getToken         -- this need to show errors whan fails

      _  -- we failed on all of the above redirect oauth-login
          -> redirect GoogleOAuthLoginR   -- have no credentials and need to get some

getQueryOA :: (FromJSON a)=> String -> Handler (OAuth2Result a)
getQueryOA url = do
   mgr   <- getHttpManager'
   token <- getToken
   liftIO $ getOAuth2 mgr token url

getQueryOA' :: (FromJSON a)=> String -> ApiReq a
getQueryOA' url = getQueryOA url >>= \case
        Left e  -> sendResponseStatus (toEnum (fromMaybe 500 $ aeStatus e) ) (aeError e)
        Right v -> return $ TC v

-- put a new tocken away, we loose the old one...
updateDBToken :: AuthToken -> Handler ()
updateDBToken AuthToken{..} = do
       uid <- getUserIdent
       runDB $ do
         deleteBy $ OnePerRealm uid
         void $ insert $ OAuthAccess
                        uid
                        "google"
                        Nothing
                        (Just atAccessToken)
                        atExpiresAt
                        atRefreshToken
                        (Just atTokenType)
                        ["https://www.googleapis.com/auth/youtube", "https://www.googleapis.com/auth/youtube.upload", "https://www.googleapis.com/auth/youtubepartner"]

refreshTokenOU :: AuthToken -> Handler ()
refreshTokenOU t = processTokenOU (\mgr gc -> refreshToken mgr gc t)

processTokenOU :: (Manager -> OAuth2 -> IO (OAuth2Result AuthToken))
               -> Handler ()
processTokenOU action = do
  gc <- googleKey
  mgr <- getHttpManager'
  liftIO (action mgr gc) >>=
     \case
        Left e -> sendResponseStatus (toEnum (fromMaybe 500 $ aeStatus e) ) (aeError e)
        Right t -> updateDBToken t

traceHTML (show -> e) = defaultLayout [whamlet|We hit authError <br> #{e}|]

-- handleGoogleDebugR :: SubApp OAuth2App String
handleGoogleDebugR :: Texts -> ApiReq Value
handleGoogleDebugR v = do
  r <- T.intercalate "&" .  map (\(a,b)-> a <> "=" <> b ) . reqGetParams <$> getRequest
  fromString $ T.unpack $ T.intercalate "/"  ("https://www.googleapis.com" : v) <> "?" <> r

