{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Foundation where

import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Text.Hamlet              (hamletFile, ihamletFile, hamletFileReload)
-- import Text.Julius              (Javascript,js)
import Text.Jasmine             (minifym)
import Yesod.Core.Types         (Logger)
import Yesod.Default.Util       (addStaticContentExternal)
import Yesod.AngularUI

import Data.Maybe (fromJust, isJust)
import qualified Data.List as DL
import Types

import           Yesod.Auth.BrowserId        (authBrowserId)
import qualified Yesod.Auth.BrowserId as BId (forwardUrl)
import Yesod.Auth.GoogleEmail3                  (authGoogleEmail, YesodGoogleAuth(..))
import qualified Yesod.Auth.GoogleEmail3  as GId( forwardUrl )
import Yesod.Facebook
import Yesod.Auth.Facebook2           (authFacebook, facebookLogin)
import qualified Data.Text as T (split, pack)
import Control.Applicative
import Data.Bool
import Control.Monad
import Database.Persist.Postgresql          (pgConnStr)

import qualified Database.PostgreSQL.Simple as PGS (connect, withTransaction)
import qualified Database.PostgreSQL.Simple.Internal as PGS

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import qualified Database.Esqueleto as E

import qualified Data.UUID.V4 as UUID
import qualified Data.UUID    as UUID

import Network.Wai.Middleware.Cors (simpleCors)

import qualified Data.Text.Lazy.Encoding as TLE (decodeUtf8)
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Network.Wai as W
import qualified Data.ByteString.Lazy as L
import Types.ConnPoolRaw


-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appConnPoolRaw :: ConnectionPoolRaw -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , userChannels   :: CMap MsgBus
    , appVersion     :: Text
--     , appOAuth2      :: OAuth2App
    }


instance HasHttpManager App where
    getHttpManager = appHttpManager

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
mkMessage "App" "messages" "en"
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage


-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootMaster $ appRoot . appSettings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = bool sslOnlySessions id compiledAsDevel $ Just <$> defaultClientSessionBackend
                                                        120    -- timeout in minutes
                                                        "config/client_session_key.aes"
    yesodMiddleware =  bool (sslOnlyMiddleware (180 * 24 * 60)) id  compiledAsDevel . defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
--         urender <- getUrlRenderParams
        mrender <- getMessageRender

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR app_min_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _)       _ = return Authorized
--     isAuthorized (RedirHashR _)  _ = return Authorized
--     isAuthorized (AuthR LoginR)  _ = return . maybe Authorized (const AuthenticationRequired) =<< maybeAuthId
--     isAuthorized (AuthR LogoutR) _ = return . maybe AuthenticationRequired (const Authorized) =<< maybeAuthId
--     isAuthorized FaviconR        _ = return Authorized
--     isAuthorized RobotsR         _ = return Authorized
    -- isAuthorized GoogleVerifyR _ = return Authorized
    -- Default to Authorized for now.
--     isAuthorized (RedirHashR _)  _ = return Authorized
--     return . maybe AuthenticationRequired (const Authorized) =<< maybeAuthId
--     isAuthorized (HomeR)         _ = return Authorized
--     return . maybe AuthenticationRequired (const Authorized) =<< maybeAuthId
    isAuthorized _               _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    urlRenderOverride a (StaticR s) = case staticRoot of
              Just r -> Just $ uncurry (joinPath a r) $ renderRoute s
              Nothing -> Nothing
             where
               staticRoot = appStaticRoot $ appSettings a
    urlRenderOverride _ _ = Nothing

    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    errorHandler NotFound = fmap toTypedContent $ defaultLayout $ do
        setTitle "Request page not located"
        toWidget [hamlet|
<h1>Not Found
<p>We apologize for the inconvenience, but the requested page could not be located.
|]
    errorHandler other = defaultErrorHandler other


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master


--- Implement Connetion Pool to tackle this
-- for now it have to


-- getConn = liftIO . PGS.connectPostgreSQL =<< pgConnStr . appDatabaseConf . appSettings <$> getYesod

-- runRawDB :: forall (m :: * -> *) b.
--       (MonadHandler m, HandlerSite m ~ App) =>
--       (PGS.Connection -> IO b) -> m b

runRawDB cc = do
--    conn <- getConn
   cp <- appConnPoolRaw <$> getYesod
   liftIO (withRawDBConn cp cc)

runRawDBT cc = do
--    conn <- getConn
   cp <- appConnPoolRaw <$> getYesod
   liftIO $ withRawDBConn cp (\conn -> PGS.withTransaction conn (cc conn))

instance YesodFacebook App where
 fbHttpManager = appHttpManager
 fbCredentials = appFbCredentials . appSettings

instance YesodGoogleAuth App where
  googleClientID     a = gaClientId     <$> (appGoogleWebAppOAuth . appSettings) a
  googleClientSecret a = gaClientSecret <$> (appGoogleWebAppOAuth . appSettings) a

newUUID = decodeUtf8 . UUID.toASCIIBytes <$> liftIO UUID.nextRandom

instance YesodAuth App where
    type AuthId App = UsersId

    -- Where to send a user after successful login
    loginDest a = HomeR []
    logoutDest _ = HomeR []
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = False

    getAuthId creds = do
--         $(logError) $ T.pack . show $ credsExtra creds
        runDB $ do
--         x <- getBy $ UniqueUser $ credsIdent creds
        x <- E.select $
             E.from $ \(p `E.LeftOuterJoin` e) -> do
             E.on $ (p E.^. UsersId) E.==. e E.^. EmailUserId
             E.where_ $ (e E.^. EmailEmail ) E.==. E.val (credsIdent creds)
             return p
        let nn = DL.lookup "full_name" $ credsExtra creds
            na = DL.lookup "avatar"    $ credsExtra creds
        case x of
            [Entity uid Users{..}] -> do
                when (isNothing usersName && isJust nn) $
                   update uid [UsersName =. nn]
                when (isNothing usersAvatar && isJust na) $
                   update uid [UsersAvatar =. na]
                when (usersDeleted) $
                   update uid [UsersDeleted =. False]

                return $ Just uid
            [] -> do
                ruuid <- newUUID
                Just <$> do
                  uu <- insert Users
                    { usersUuid = ruuid
                    , usersName      = nn
                    , usersFriendly  = Nothing
                    , usersAvatar    = na
                    , usersDeleted   = False
                    }
                  insert Email
                    { emailEmail = credsIdent creds
                    , emailUserId  = uu
                    }
                  return uu

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ authBrowserId def
                    , authGoogleEmail
                    , authFacebook ["email"]
                    ]

    authHttpManager = getHttpManager

    loginHandler = do
        tp <- getRouteToParent
        lift $ authLayout $ do
            master <- getYesod
            let [b,g,f] = authPlugins master
            let render = flip apLogin tp
                wb = render b
                wg = render g
                wf = render f
            toWidget $(widgetFile "login")

semicolon :: Text
semicolon = ";" :: Text

instance YesodAuthPersist App

-- Note: Some functionality previously pre
-- -- --  sent in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding


-- todo: find a way on how to add i18n to angular
-- almost
instance YesodAngular App where
   angularUIEntry = do
     loggedIn <- isJust <$> handlerToWidget maybeAuthId
     perms <- handlerToWidget userPerms
     $(widgetFile "uiEntry")

langIdLocale :: LangId -> Route App
langIdLocale LangEnGB = StaticR angular_i18n_angular_locale_en_gb_js
langIdLocale LangEnUs = StaticR angular_i18n_angular_locale_en_us_js
langIdLocale LangPl   = StaticR angular_i18n_angular_locale_pl_js
langIdLocale LangRu   = StaticR angular_i18n_angular_locale_ru_js
langIdLocale LangFr   = StaticR angular_i18n_angular_locale_fr_js
langIdLocale LangDe   = StaticR angular_i18n_angular_locale_de_js
langIdLocale LangIt   = StaticR angular_i18n_angular_locale_it_js


angularUILayout :: Text -> WidgetT App IO () ->  HandlerT App IO Html
angularUILayout ngApp widget = do
        master <- getYesod
        mrender <- getMessageRender
        langI18Ang <- langIdLocale <$> getUserLang
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR app_min_css
            $(widgetFile "empty-layout")
        withUrlRenderer $(hamletFile "templates/angular-layout-wrapper.hamlet")

-- todo: convert route into javascript function
-- instance IsString ((Route App -> [(Text, Text)] -> Text) -> Javascript) where
--  fromString a = [js|^{rawJS a}|]

ghcjsLayout :: HandlerT App IO Html
-- ghcjsLayout = withUrlRenderer $(hamletFile "templates/v2.hamlet")
ghcjsLayout = withUrlRenderer $(hamletFile  "templates/v2.hamlet")

-- get user identity from the DB, maybe this is already cached?
getUserIdent :: Handler (Key Users)
getUserIdent = requireAuthId

getGroupKey :: GUUID -> AppM (Key SiteGroup)
getGroupKey u = runDB $ entityKey <$> getBy404 (UniqueSiteGroup u)

getUserAdmin :: Handler Bool
getUserAdmin =
  maybeAuthId >>= \case
      Nothing -> return False
      Just aid ->
          runDB $
             E.select (
             E.from $ \(p `E.InnerJoin` e) -> do
             E.on $ (p E.^. UsersId) E.==. e E.^. SiteAdminUserId
             E.where_ $ (p E.^. UsersId ) E.==. E.val aid
             return e)
             >>= return . \case
               [Entity _ v] -> siteAdminIsAdmin v
               _ -> False

getUserLang :: Handler LangId
getUserLang = cached (readLang <$> languages)


type ApiReq a    = Handler (TC a)
type ApiPost b a = b -> Handler (PC a)

type AppM x = HandlerT App IO x

userPerms :: AppM Permssions
userPerms = do
         permVers <- appVersion <$> getYesod
         isAdmin <-  getUserAdmin
         let isDebugger = isAdmin
         isLogged <- isJust <$> maybeAuthId
         let userGroup = Map.empty
         return Permssions{..}

requestBodyLBS :: Handler L.ByteString
requestBodyLBS = liftIO . W.strictRequestBody =<< waiRequest

requestBodyText :: Handler Text
requestBodyText = TL.toStrict . TLE.decodeUtf8 <$> requestBodyLBS
