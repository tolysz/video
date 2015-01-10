{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE LambdaCase            #-}

module SubSite.OAuth2 where

import SubSite.Import
import Network.HTTP.OAuth2.Types
import Types
import Text.Hamlet (hamlet)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Data.Maybe
import Data.String.QM

import Data.Time.Clock ( diffUTCTime )

import qualified Data.Aeson.Types as DA
import qualified Data.Text as T
import qualified Data.List as DL
import Google.Api.Types.GoogleUser

import Network.HTTP.OAuth2

instance YesodSubDispatch OAuth2App (HandlerT App IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesOAuth2App )

-- This simplyfies requesting data to suplying url with the appriopirate types
type ApiReq a = SubApp OAuth2App (TC a)
-- example how monad is an onion and changing boundaries changes meaning
instance FromJSON a => IsString (SubApp OAuth2App (TC a)) where
  fromString = getQueryOA'

handleGoogleCallbackR :: SubApp OAuth2App Html
handleGoogleCallbackR = do
  codeMaybe <- lookupGetParam "code"
  errorMaybe <- lookupGetParam "error"
  case (codeMaybe, errorMaybe) of
    (Just c, _)       -> processTokenOU (\mgr gc -> fetchAccessToken mgr gc c)
                          >> lift (redirect (OAuthSiteR GoogleDebugR))
    (_, Just e)       -> traceHTML e
    (Nothing,Nothing) -> lift (redirect HomeR)

handleGoogleManageR :: SubApp OAuth2App ()
handleGoogleManageR = lift $ redirect ("https://security.google.com/settings/security/permissions?pli=1" :: String)


handleGoogleOAuthLogoutR :: SubApp OAuth2App ()
handleGoogleOAuthLogoutR =
   lift . runDB . deleteBy . OnePerRealm =<< getUserIdent

handleGoogleOAuthLoginR :: SubApp OAuth2App ()
handleGoogleOAuthLoginR = do
         gc <- googleKey
         redirect $
           generateAuthUrl
             gc
             (Scope ["https://www.googleapis.com/auth/youtubepartner", "https://www.googleapis.com/auth/youtube.upload", "https://www.googleapis.com/auth/youtube"])
             False


getHttpManager' :: SubApp OAuth2App Manager
getHttpManager' = lift $ appHttpManager <$> getYesod

-- And we'll spell out the handler type signature.
handleRootOAuth2R :: SubApp OAuth2App ()
handleRootOAuth2R = return ()

handleMo404R _ = lift $ defaultLayout [whamlet|Welcome to the subsite! 404|]

googleKey :: SubApp OAuth2App OAuth2
googleKey = lift $ do
     render <- getUrlRender
     Just (OAuth2Google {..}) <- appGoogleWebAppOAuth . appSettings <$> getYesod
     return OAuth2 { oauthClientId     = gaClientId
                   , oauthClientSecret = gaClientSecret
                   , oauthRedirectUri  = render (OAuthSiteR GoogleCallbackR)
                   , oauthAuthUri      = "https://accounts.google.com/o/oauth2/auth"
                   , oauthTokenUri     = "https://accounts.google.com/o/oauth2/token"
                   , oauthRevokeUri    = "https://accounts.google.com/o/oauth2/revoke" -- maybe a function?
                   }

getToken :: SubApp OAuth2App AuthToken
getToken = do
  now <- liftIO getCurrentTime
  (lift . runDB . getBy . OnePerRealm =<< getUserIdent) >>=
    f now
   where
      f now x
        | Just (Entity _ (OAuthAccess{..}))  <- x  -- we have the entry inside DB
        , Just ex  <- oAuthAccessExpires           -- it has expires field
        , diffUTCTime ex now > 10                  -- it still has at least 10s on the clock
        , Just at <- oAuthAccessAccessToken        -- and we have the access token
          = return def{atAccessToken=at}           -- thus we use it

        | Just (Entity _ (OAuthAccess{..}))  <- x -- we have the entry inside DB
        , Just _ <- oAuthAccessRefreshToken       -- we have the refresh tocken there
          = refreshTokenOU def{atRefreshToken=oAuthAccessRefreshToken} >> getToken         -- this need to fail and show errors

        | otherwise -- we failed on all of the above redirect home
          = lift (redirect $ OAuthSiteR GoogleOAuthLoginR)   -- have no credentials and need to get some

getQueryOA :: (FromJSON a)=> String -> SubApp OAuth2App (OAuth2Result a)
getQueryOA url = do
   mgr   <- getHttpManager'
   token <- getToken
   liftIO $ getOAuth2 mgr token url

getQueryOA' :: (FromJSON a)=> String -> ApiReq a
getQueryOA' url = getQueryOA url >>= \case
        Left e  -> lift $ sendResponseStatus (toEnum (fromMaybe 500 $ aeStatus e) ) (aeError e)
        Right v -> return $ TC v


-- get user identity from the DB, meybe this is already cached?
getUserIdent :: SubApp OAuth2App (Key User)
getUserIdent = lift $ do
  aid <- fmap (userIdent . fromJust) . runDB . get =<< requireAuthId
  runDB $ do
     Just (Entity k _) <- getBy $ UniqueUser aid
     return k

-- put a new tocken away, we loose the old one...
updateDBToken :: AuthToken -> SubApp OAuth2App ()
updateDBToken AuthToken{..} = do
       uid <- getUserIdent
       lift $ runDB $ do
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

refreshTokenOU :: AuthToken -> SubApp OAuth2App ()
refreshTokenOU t = processTokenOU (\mgr gc -> refreshToken mgr gc t)

processTokenOU :: (Manager -> OAuth2 -> IO (OAuth2Result AuthToken))
               -> SubApp OAuth2App ()
processTokenOU action = do
  gc <- googleKey
  mgr <- getHttpManager'
  liftIO (action mgr gc) >>=
     \case
        Left e -> lift $ sendResponseStatus (toEnum (fromMaybe 500 $ aeStatus e) ) (aeError e)
        Right t -> updateDBToken t


traceHTML (show -> e) = lift $ defaultLayout [whamlet|We hit authError <br> #{e}|]


-- handleGoogleDebugR :: SubApp OAuth2App String
handleGoogleDebugR = getListVideosR
-- handleGoogleDebugR = getGoogleUserR


-- Move sample requests out once it is finished.
getGoogleUserR     :: ApiReq GoogleUser
getGoogleUserR     =  "https://www.googleapis.com/oauth2/v2/userinfo"

getListVideosR     :: ApiReq Value
getListVideosR     = "https://www.googleapis.com/youtube/v3/videos?part=snippet,contentDetails,status,statistics,recordingDetails,fileDetails&id=ACft-tpu47g"

-- id: "UCSkMt4A9QMBnuFVrmPHQ1iw"
getListMyChannels  :: ApiReq Value
getListMyChannels  = "https://www.googleapis.com/youtube/v3/channels?part=snippet&mine=true"

getListMyPlaylists :: ApiReq Value
getListMyPlaylists = "https://www.googleapis.com/youtube/v3/playlists?part=snippet&channelId=UCSkMt4A9QMBnuFVrmPHQ1iw&maxResults=50"

getAllVideos       :: ApiReq Value
getAllVideos       = "https://www.googleapis.com/youtube/v3/search?part=snippet&forMine=true&type=video&maxResults=50"

-- ACft-tpu47g



--    \case
--       -> case of
--        Just v --

-- return ()

-- instance Default Value where
--   def = DA.emptyObject
{-
  do
  setUltDestCurrent
  lookupSession "refreshToken" >>= \case
     Just rtk -> do
         mgr   <- httpManager <$> getYesod
         now <- liftIO $ getCurrentTime
         (liftIO $ refreshToken mgr googleKey def{atRefreshToken=Just rtk}) >>= \case
           (Right token) -> do
               setSession "token" (atAccessToken token)
               setSession "tokenType" (atTokenType token)
               setSession "ExpiresAt" (T.pack . show $ addUTCTime (realToFrac (fromMaybe 3600 $ atExpiresIn token)) now)
               clearUltDest
           Left _ -> getOAuthLoginR
     Nothing -> getOAuthLoginR-}

{--
id


Pubblic - visible to all


snippet.categoryId

status.embeddable
status.license
status.publicStatsViewable


status.publishAt

-- all / most the same place
recordingDetails.locationDescription
recordingDetails.location.latitude
recordingDetails.location.longitude


snippet.title
snippet.description
status.privacyStatus
snippet.tags[]
recordingDetails.recordingDate


private
public
unlisted

--

PUT https://www.googleapis.com/youtube/v3/videos?part=status&fields=status&key={YOUR_API_KEY}

Content-Type:  application/json
Authorization:  Bearer ya29.8gD_7gLL14dOD0cujDiz-VP-OGPM-9Mp331oSGRQBnpcoocJEGAhOj3aBqmc8rnoeBj7ThZgVlOjew
X-JavaScript-User-Agent:  Google APIs Explorer

{
 "id": "ACft-tpu47g",
 "status": {
  "privacyStatus": "private"
 }
}


200 OK

- Hide headers -

Cache-Control:  no-cache, no-store, max-age=0, must-revalidate
Content-Encoding:  gzip
Content-Type:  application/json; charset=UTF-8
Date:  Mon, 05 Jan 2015 02:25:16 GMT
ETag:  "F9iA7pnxqNgrkOutjQAa9F2k8HY/rMMyhSGqlP7XmNPDmwtACS4DhUw"
Expires:  Fri, 01 Jan 1990 00:00:00 GMT
Pragma:  no-cache
Server:  GSE
Transfer-Encoding:  chunked
Vary:  Origin, X-Origin

{
 "status": {
  "uploadStatus": "processed",
  "privacyStatus": "private",
  "license": "youtube",
  "embeddable": true,
  "publicStatsViewable": true
 }
}
-}

