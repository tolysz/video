{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Foundation where

import Database.Persist.MongoDB hiding (master)
import Import.NoFoundation
import Text.Hamlet              (hamletFile, ihamletFile)
-- import Text.Julius              (Javascript,js)
import Text.Jasmine             (minifym)
import Yesod.Core.Types         (Logger)
import Yesod.Default.Util       (addStaticContentExternal)
import Yesod.AngularUI
-- import SubSite.Data
import Data.Maybe (fromJust)
import Types

import Yesod.Auth.BrowserId           (authBrowserId)
import qualified Yesod.Auth.BrowserId as BId (forwardUrl)
import Yesod.Auth.GoogleEmail3        (authGoogleEmail, YesodGoogleAuth(..))
import qualified Yesod.Auth.GoogleEmail3        as GId( forwardUrl )
import Yesod.Facebook
import Yesod.Auth.Facebook2           (authFacebook, facebookLogin)
import qualified Data.Text as T (split)
import Control.Applicative
import Data.Bool

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , userChannels   :: CMap MsgBus

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
    yesodMiddleware = bool (sslOnlyMiddleware 240) id  compiledAsDevel . defaultYesodMiddleware

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
            addStylesheet $ StaticR app_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet") -- ) mrender urender

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _)       _ = return Authorized
    isAuthorized (RedirHashR _)  _ = return Authorized
--     isAuthorized (AuthR LoginR)  _ = return . maybe Authorized (const AuthenticationRequired) =<< maybeAuthId
    isAuthorized (AuthR LogoutR) _ = return . maybe AuthenticationRequired (const Authorized) =<< maybeAuthId
    isAuthorized FaviconR        _ = return Authorized
    isAuthorized RobotsR         _ = return Authorized
    -- isAuthorized GoogleVerifyR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized (RedirHashR _)  _ = return . maybe AuthenticationRequired (const Authorized) =<< maybeAuthId
    isAuthorized (HomeR)         _ = return . maybe AuthenticationRequired (const Authorized) =<< maybeAuthId
    isAuthorized _               _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
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

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = MongoContext
    runDB action = do
        master <- getYesod
        runMongoDBPool
            (mgAccessMode $ appDatabaseConf $ appSettings master)
            action
            (appConnPool master)

instance YesodFacebook App where
 fbHttpManager = appHttpManager
 fbCredentials = appFbCredentials . appSettings

instance YesodGoogleAuth App where
  googleClientID     a = gaClientId     <$> (appGoogleWebAppOAuth . appSettings) a
  googleClientSecret a = gaClientSecret <$> (appGoogleWebAppOAuth . appSettings) a


instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest a = HomeR -- RedirHashR []
    --     do
--       YesodRequest{..} <- getRequest <$> getYesod
--       maybe HomeR (RedirHashR . T.split (== '/')) $ "hash" `lookup` reqCookies
    -- :#:
--     loginDest a =
    -- Where to send a user after logout
    logoutDest _ = AuthR LoginR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing ->
                Just <$> insert User
                    { userIdent = credsIdent creds
          --          , userPassword  = Nothing
                    , userName      = Nothing
                    , userFriendly  = Nothing
                    , userSiteAdmin = False
                    , userAvatar    = Nothing
                    }

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
--             [whamlet|
-- <div style="width:500px;margin:0 auto">
--   <div >
--       <h3>Login using:
--       <a href="@{AuthR facebookLogin}" .facebookLogin>
--           Facebook
--       <h3>&mdash; OR &mdash;
--       <a href="@{AuthR GId.forwardUrl}" .googleLogin>
--           Google+
--       <h3>&mdash; OR &mdash;
--       ^{wb}
--       <br>
--             |]
--
--             return ()

--     loginHandler = do
--       return
--         tp <- getRouteToParent
--         master <- lift $ getYesod
--         [b,g,f] <- mapM (flip apLogin tp) (authPlugins master)
--         lift $ authLayout $ do
--           [whamlet|<div style="width:500px;margin:0 auto">|]

--     loginHandler = do
--       tp <- getRouteToParent
--
--       plug <- do
--            master <- getYesod
--            map (\a -> (apLogin a) tp) (authPlugins master)
--       lift $ defaultLayout $ [whamlet|<div style="width:500px;margin:0 auto">^{login plug}|]

semicolon = ";" :: Text

-- login :: [Widget] -> Widget
-- login plug = toWidget $(widgetFile "login")

instance YesodAuthPersist App

-- instance RenderMessage App Message where
--     renderMessage _ _ = defaultFormMessage

-- Note: Some functionality previously pre
-- -- --  sent in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding

-- instance (YesodAngular app ~ app, RenderMessage app m) => RenderMessage app m where
--   renderMessage  =  renderMessage

-- instance RenderMessage (YesodAngular ) FormMessage where
--    renderMessage _ _ = defaultFormMessage

-- todo: find a way on how to add i18n to angular
instance YesodAngular App where
--   renderMessageAUI = Just renderMessage
--    angularRM = renderMessage
   angularUIEntry = $(widgetFile "uiEntry")

angularUILayout :: Text -> WidgetT App IO () ->  HandlerT App IO Html
angularUILayout ngApp widget = do
        void requireAuthId
        master <- getYesod
        mrender <- getMessageRender
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR app_min_css
            $(widgetFile "empty-layout")
        withUrlRenderer $(hamletFile "templates/angular-layout-wrapper.hamlet")

-- instance IsString ((Route App -> [(Text, Text)] -> Text) -> Javascript) where
--  fromString a = [js|^{rawJS a}|]

-- get user identity from the DB, maybe this is already cached?
getUserIdent :: Handler (Key User)
getUserIdent = do
  aid <- fmap (userIdent . fromJust) . runDB . get =<< requireAuthId
  runDB $ do
     Just (Entity k _) <- getBy $ UniqueUser aid
     return k

getUserAdmin :: Handler Bool
getUserAdmin = do
  aid <- fmap (userIdent . fromJust) . runDB . get =<< requireAuthId
  runDB $ do
     Just (Entity _ v) <- getBy $ UniqueUser aid
     return $ userSiteAdmin v
