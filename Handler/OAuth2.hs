
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Handler.OAuth2 where

import Import

import Data.Time.Clock ( diffUTCTime )
import Data.Maybe
import qualified Data.Text as T
import Control.Lens
{--
 -- see how new imports will looks like
import Data {Time.Clock (diffUTCTime), Possible, Text qualified as T}
-}

import Network.HTTP.OAuth2
import Network.HTTP.OAuth2.Types
import Network.Google.Api.Kinds
import qualified Database.Esqueleto as E

fetchNext :: ListResponse a sym -> Possible String
fetchNext lr = lr ^. lrNextPageToken . to (fmap T.unpack) -- . to (possible Nothing Nothing (Just . T.unpack))

fetchItems :: ListResponse a sym -> [a]
fetchItems lr = lr ^. lrItems

instance FromJSON a => IsString (Text -> Handler (TC a)) where
  fromString a = \b -> getQueryOA' b a
-- ^ This simplyfies requesting data to suplying url with the appriopirate types
--   example how monad is an onion and changing boundaries changes meaning

handleGoogleCallbackR :: Handler Html
handleGoogleCallbackR = do
  codeMaybe <- lookupGetParam "code"
  errorMaybe <- lookupGetParam "error"
  gidMaybe <- lookupGetParam "state"

  unless (isJust gidMaybe) $ redirect (HomeR [])

  case (codeMaybe, errorMaybe) of
    (Just c, _)       -> processTokenOU (fromJust gidMaybe) (fetchAccessToken c)
                          >> redirect (HomeR [])
    (_, Just e)       -> traceHTML e
    (Nothing,Nothing) -> redirect (HomeR [])

handleGoogleManageR :: Handler ()
handleGoogleManageR = redirect ("https://security.google.com/settings/security/permissions?pli=1" :: String)

handleGoogleOAuthLogoutR :: Handler ()
handleGoogleOAuthLogoutR = -- do
   -- revoke token somehow else
   -- todo: brocken
   -- void . runDB . deleteBy . OnePerRealm =<< getUserIdent
   return ()

handleGoogleOAuthLoginR :: Text -> Handler ()
handleGoogleOAuthLoginR gid = do
         setSession "GUUID" gid
         gc <- googleKey gid
         redirect $
           generateAuthUrl
             gc
             False
             (Scope [ "profile"                                            -- try to recocer username
                    , "https://www.googleapis.com/auth/youtubepartner"     -- whatever they do
                    , "https://www.googleapis.com/auth/youtube.upload"     -- be able to upload new videos
                    , "https://www.googleapis.com/auth/youtube"            -- see all data
                    ]
              )

getHttpManager' :: Handler Manager
getHttpManager' = appHttpManager <$> getYesod

-- And we'll spell out the handler type signature.
handleRootOAuth2R :: Handler ()
handleRootOAuth2R = return ()

googleKey :: Text -> Handler OAuth2
googleKey uuid = do
     render <- getUrlRender
     Just (OAuth2Google {..}) <- appGoogleWebAppOAuth . appSettings <$> getYesod
     return OAuth2 { oauthClientId     = gaClientId
                   , oauthClientSecret = gaClientSecret
                   , oauthRedirectUri  = render GoogleCallbackR
                   , oauthAuthUri      = "https://accounts.google.com/o/oauth2/auth"
                   , oauthTokenUri     = "https://accounts.google.com/o/oauth2/token"
                   , oauthRevokeUri    = "https://accounts.google.com/o/oauth2/revoke?token="
                   , oauthState        = Just uuid
                   }

getToken :: Text ->  Handler AuthToken
getToken gid = do
  now <- liftIO getCurrentTime
  uid <- getUserIdent

  runDB ( do
     Entity gid' _ <- getBy404 (UniqueSiteGroup gid)
     getBy (UniqueOAuthAccess uid gid')) >>= \case
      Just (Entity _ (OAuthAccess{..}))      -- we have the entry inside DB
        | Just ex  <- oAuthAccessExpires     -- it has expires field
        , diffUTCTime ex now > 10            -- it still has at least 10s on the clock
        , Just atok <- oAuthAccessAccessToken  -- and we have the access token
          -> return def{atAccessToken=atok}    -- thus we use it

      Just (Entity _ (OAuthAccess{..}))      -- we have the entry inside DB
        | Just _ <- oAuthAccessRefreshToken  -- we have the refresh tocken there
          -> refreshTokenOU gid def{atRefreshToken=oAuthAccessRefreshToken} >> getToken gid        -- this need to show errors whan fails

      _  -- we failed on all of the above redirect oauth-login
          -> redirect ( GoogleOAuthLoginR gid)  -- have no credentials and need to get some

getQueryOA :: (FromJSON a) => Text -> String -> Handler (OAuth2Result a)
getQueryOA gid url = do
   mgr   <- getHttpManager'
   token <- getToken gid
   liftIO $ getOAuth2 mgr token url

getQueryOA' :: (FromJSON a)=> Text -> String -> ApiReq a
getQueryOA' gid url = getQueryOA gid url >>= \case
        Left e  -> sendResponseStatus (toEnum (fromMaybe 500 $ aeStatus e) ) (aeError e)
        Right v -> return $ TC v

-- put a new tocken away, we loose the old one...
updateDBToken :: Text -> AuthToken -> Handler ()
updateDBToken gid' AuthToken{..} = do
       uid  <- getUserIdent
       runDB $ do
--          gid  <- get404 (UniqueSiteGroup gid')
         Entity gid _ <- getBy404 (UniqueSiteGroup gid')
         deleteBy $ UniqueOAuthAccess uid gid
         void $ insert $ OAuthAccess
                        uid
                        gid
                        "google"               -- let sets oauth provider arbitrary to google
                        Nothing
                        (Just atAccessToken)   -- store the current access tocken
                        atExpiresAt            -- store current expiry for access tocken
                        atRefreshToken         -- guard this one, tocken to refresh access token
                        (Just atTokenType)     -- type methinks only Barer
                          -- scopes to include
                        [ "profile"                                         -- basic user info
                        , "https://www.googleapis.com/auth/youtube"         -- basic yt info
                        , "https://www.googleapis.com/auth/youtube.upload"  -- being able to change yt settings
                        , "https://www.googleapis.com/auth/youtubepartner"  -- some promotions and stuff
                        ]

refreshTokenOU :: Text -> AuthToken -> Handler ()
refreshTokenOU gid = processTokenOU gid . refreshToken

processTokenOU :: Text -> (Manager -> OAuth2 -> IO (OAuth2Result AuthToken))
               -> Handler ()
processTokenOU gid action = do
  gc <- googleKey gid
  mgr <- getHttpManager'
  liftIO (action mgr gc) >>=
     \case
        Left e -> sendResponseStatus (toEnum (fromMaybe 500 $ aeStatus e) ) (aeError e)
        Right t -> updateDBToken gid t

traceHTML :: Show a => a -> Handler Html
traceHTML (show -> e) = defaultLayout [whamlet|We hit authError <br> #{e}|]

handleGoogleDebugR :: Text -> Texts -> ApiReq Value
handleGoogleDebugR gid v = do
  r <- T.intercalate "&" .  map (\(a,b)-> a <> "=" <> b ) . reqGetParams <$> getRequest
  (fromString $ T.unpack $ T.intercalate "/"  ("https://www.googleapis.com" : v) <> "?" <> r) gid
