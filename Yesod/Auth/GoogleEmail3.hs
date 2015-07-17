{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE CPP               #-}
-- | Use an email address as an identifier via Google's login system.
--
-- Note that this is a replacement for "Yesod.Auth.GoogleEmail", which depends
-- on Google's now deprecated OpenID system. For more information, see
-- <https://developers.google.com/+/api/auth-migration>.
--
-- By using this plugin, you are trusting Google to validate an email address,
-- and requiring users to have a Google account. On the plus side, you get to
-- use email addresses as the identifier, many users have existing Google
-- accounts, the login system has been long tested (as opposed to BrowserID),
-- and it requires no credential managing or setup (as opposed to Email).
--
-- In order to use this plugin:
--
-- * Create an application on the Google Developer Console <https://console.developers.google.com/>
--
-- * Create OAuth credentials. The redirect URI will be <http://yourdomain/auth/page/googleemail2/complete>. (If you have your authentication subsite at a different root than \/auth\/, please adjust accordingly.)
--
-- * Enable the Google+ API.
--
-- Since 1.3.1
module Yesod.Auth.GoogleEmail3
    ( authGoogleEmail
    , forwardUrl
    , YesodGoogleAuth (..)
    ) where
import           ClassyPrelude
import           Blaze.ByteString.Builder (fromByteString, toByteString)
import           Control.Applicative      ((<$>), (<*>))
-- import           Control.Arrow            (second)
import           Control.Monad            (liftM, unless)
import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode        as A
import           Data.Aeson.Parser        (json')
import           Data.Aeson.Types         (FromJSON (parseJSON), parseEither,
                                           withObject)
import Data.Maybe (fromJust, isNothing)
--- import Control.Arrow ((***), second)

import           Data.Aeson.Lens
-- import Data.Aeson.Lens
-- import           Control.Lens.Operators   ((^?))
import Control.Lens ((^?) , (^.))
import Control.Lens.Iso (non)

import           Data.Conduit             (($$+-))
import           Data.Conduit.Attoparsec  (sinkParser)
import qualified Data.HashMap.Strict      as M
import           Data.Monoid              (mappend)
import           Data.Maybe               (fromJust, isJust)
import           Data.Text                (Text)
import qualified Data.Text                as T
-- import           Data.Text.Encoding       (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Builder   as TL
import           Network.HTTP.Client      (parseUrl, requestHeaders,
                                           responseBody, urlEncodedBody, newManager)
import           Network.HTTP.Conduit     (http, withManagerSettings, withManager, mkManagerSettings)
import           Network.HTTP.Types       (renderQueryText)
import Network.Connection (TLSSettings (..))
import           Network.Mail.Mime        (randomString)
import           System.Random            (newStdGen)
import           Yesod.Auth               (Auth, AuthPlugin (AuthPlugin),
                                           AuthRoute, Creds (Creds),
                                           Route (PluginR), YesodAuth,
                                           authHttpManager, setCredsRedirect)
import qualified Yesod.Auth.Message       as Msg
import           Yesod.Core               (HandlerSite, MonadHandler,
                                           getRouteToParent, getUrlRender,
                                           getYesod, invalidArgs, lift,
                                           lookupGetParam,
                                           lookupSession, notFound, redirect,
                                           setSession, whamlet, (.:),
                                           TypedContent, HandlerT, liftIO, Yesod(..), toWidget, lucius)


catMaybes1 :: [(a, Maybe b)] -> [(a, b)]
catMaybes1 = map ( second fromJust ) . filter ( isJust . snd )

class  Yesod site => YesodGoogleAuth site where
  googleClientID     :: site -> Maybe Text
  googleClientSecret :: site -> Maybe Text

getGoogleClientID :: (MonadHandler m, HandlerSite m ~ site, YesodGoogleAuth site) => m Text
getGoogleClientID = maybe notFound return =<< googleClientID <$> getYesod

getGoogleClientSecret :: (MonadHandler m, HandlerSite m ~ site, YesodGoogleAuth site) => m Text
getGoogleClientSecret = maybe notFound return   =<< googleClientSecret <$> getYesod


pid :: Text
pid = "googleemail3"

forwardUrl :: AuthRoute
forwardUrl = PluginR pid ["forward"]

csrfKey :: Text
csrfKey = "_GOOGLE_CSRF_TOKEN"

getCsrfToken :: MonadHandler m => m (Maybe Text)
getCsrfToken = lookupSession csrfKey

getCreateCsrfToken :: MonadHandler m => m Text
getCreateCsrfToken = do
    mtoken <- getCsrfToken
    case mtoken of
        Just token -> return token
        Nothing -> do
            stdgen <- liftIO newStdGen
            let token = T.pack $ fst $ randomString 10 stdgen
            setSession csrfKey token
            return token

settingsSsl = mkManagerSettings (TLSSettingsSimple True False False) Nothing
authGoogleEmail :: (YesodAuth m, YesodGoogleAuth m)
                => AuthPlugin m
authGoogleEmail =
    AuthPlugin pid dispatch login
  where
    complete = PluginR pid ["complete"]

    getDest :: MonadHandler m
            => Text -> (Route Auth -> Route (HandlerSite m))
            -> m Text
    getDest clientID tm = do
        csrf <- getCreateCsrfToken
        render <- getUrlRender
        let qs = map (second Just)
                [ ("scope", "email")
                , ("state", csrf)
                , ("redirect_uri", render $ tm complete)
                , ("response_type", "code")
                , ("client_id", clientID)
                , ("access_type", "offline")
                ]
        return $ decodeUtf8
               $ toByteString
               $ fromByteString "https://accounts.google.com/o/oauth2/auth"
                    `mappend` renderQueryText True qs

    login tm = do
        [whamlet|$newline never
<p>
    <a href=@{tm forwardUrl} .googleLogin > _{Msg.LoginGoogle}|]
        toWidget [lucius|
           a.googleLogin {
             padding: 10px 48px;
             background: #dd4b39;
             color: #FFF;
             text-decoration: none;
             background-size: 52px 53px;
             display: inline-block;
             width: 250px;
             text-align: end;
             background-repeat: no-repeat;
             background-image:url(data:image/png#{semicolon}base64,iVBORw0KGgoAAAANSUhEUgAAAIAAAACACAYAAADDPmHLAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyRpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuMy1jMDExIDY2LjE0NTY2MSwgMjAxMi8wMi8wNi0xNDo1NjoyNyAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENTNiAoTWFjaW50b3NoKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDozMUNBRjFDMEU2NzIxMUUyOTJBM0JEMjRDRjFDQUI2NSIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDozMUNBRjFDMUU2NzIxMUUyOTJBM0JEMjRDRjFDQUI2NSI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjQ5OTI5RUZGRTY2NDExRTI5MkEzQkQyNENGMUNBQjY1IiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjQ5OTI5RjAwRTY2NDExRTI5MkEzQkQyNENGMUNBQjY1Ii8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+esXGJgAAEB9JREFUeNrsXQt0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);
           }
        |]

    dispatch :: (YesodAuth site, YesodGoogleAuth site)
             => Text
             -> [Text]
             -> HandlerT Auth (HandlerT site IO) TypedContent
    dispatch "GET" ["forward"] = do
        tm <- getRouteToParent
        void getCreateCsrfToken
        clientID <- lift getGoogleClientID
        lift (getDest clientID tm) >>= redirect

    dispatch "GET" ["complete"] = do
#if DEVELOPMENT
#else
        mstate <- lookupGetParam "state"
--         mtoken <- getCsrfToken
--         liftIO $ do
--             print mstate
--             print mtoken
--
        case mstate of
            Nothing -> invalidArgs ["CSRF state from Google is missing"]
            Just state -> do
                mtoken <- getCreateCsrfToken
                unless (state == mtoken) $ invalidArgs ["Invalid CSRF token from Google"]
#endif
        mcode <- lookupGetParam "code"
        code <-
            case mcode of
                Nothing -> invalidArgs ["Missing code paramter"]
                Just c -> return c

        render <- getUrlRender

        req' <- liftIO $ parseUrl "https://accounts.google.com/o/oauth2/token" -- FIXME don't hardcode, use: https://accounts.google.com/.well-known/openid-configuration
        clientID     <- lift getGoogleClientID
        clientSecret <- lift getGoogleClientSecret
        let req =
                urlEncodedBody
                    [ ("code", encodeUtf8 code)
                    , ("client_id", encodeUtf8 clientID)
                    , ("client_secret", encodeUtf8 clientSecret)
                    , ("redirect_uri", encodeUtf8 $ render complete)
                    , ("grant_type", "authorization_code")
                    ]
                    req'
                        { requestHeaders = []
                        }

        value <- withManagerSettings settingsSsl $ \manager -> do
           -- manager <- newManager -- liftM authHttpManager $ lift getYesod
           res <- http req manager
           responseBody res $$+- sinkParser json'
        Tokens accessToken tokenType <-
            case parseEither parseJSON value of
                Left e -> error e
                Right t -> return t

        unless (tokenType == "Bearer") $ error $ "Unknown token type: " ++ show tokenType

        req2' <- liftIO $ parseUrl "https://www.googleapis.com/plus/v1/people/me"
        let req2 = req2'
                { requestHeaders =
                    [ ("Authorization", encodeUtf8 $ "Bearer " `mappend` accessToken)
                    ]
                }
        value2 <- withManagerSettings settingsSsl $ \manager -> do
             res2 <- http req2 manager
             responseBody res2 $$+- sinkParser json'
        Person emails fullName <-
            case parseEither parseJSON value2 of
                Left e -> error e
                Right x -> return x
        email <-
            case map emailValue $ filter (\e -> emailType e == "account") emails of
                [e] -> return e
                [] -> error "No account email"
                x -> error $ "Too many account emails: " ++ show x
        lift $ setCredsRedirect $ Creds pid email $ (catMaybes1 [("full_name",fullName), ("avatar", value2 ^? key "image" . key "url"  . _String )]) <> allPersonInfo value2

    dispatch _ _ = notFound

semicolon :: Text
semicolon = ";"

data Tokens = Tokens Text Text
instance FromJSON Tokens where
    parseJSON = withObject "Tokens" $ \o -> Tokens
        <$> o .: "access_token"
        <*> o .: "token_type"

type FullName = Maybe Text
type Avatar   = Maybe Text

data Person = Person [Email] FullName
-- Avatar

instance FromJSON Person where
    parseJSON = withObject "Person" $ \o -> Person
        <$> o .: "emails"
        <*> o A..:? "displayName"
--        <*> ((o .:? "url") >>= (A..:? "image"))
--        <*> (fmap (\a -> a ^? key "url") <$> o A..:? "image" )

data Email = Email
    { emailValue :: Text
    , emailType  :: Text
    }
    deriving Show

instance FromJSON Email where
    parseJSON = withObject "Email" $ \o -> Email
        <$> o .: "value"
        <*> o .: "type"

allPersonInfo :: A.Value -> [(Text, Text)]
allPersonInfo (A.Object o) = map enc $ M.toList o
    where enc (key, A.String s) = (key, s)
          enc (key, v) = (key, TL.toStrict $ TL.toLazyText $ A.encodeToTextBuilder v)
allPersonInfo _ = []
