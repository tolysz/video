{-# LANGUAGE FlexibleInstances #-}
module Foundation where

import Database.Persist.MongoDB hiding (master)
import Import.NoFoundation
import Text.Hamlet              (hamletFile)
-- import Text.Julius              (Javascript,js)
import Text.Jasmine             (minifym)
import Yesod.Core.Types         (Logger)
import Yesod.Default.Util       (addStaticContentExternal)
import Yesod.AngularUI
-- import SubSite.Data
import Data.Maybe (fromJust)
import Types

import Yesod.Auth.BrowserId           (authBrowserId)
import Yesod.Auth.GoogleEmail3        (authGoogleEmail, YesodGoogleAuth(..))
import Yesod.Facebook
import Yesod.Auth.Facebook2           (authFacebook)

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
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR app_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- isAuthorized GoogleVerifyR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

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
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    getAuthId creds = runDB $ do
        $(logError) $ credsPlugin creds
        $(logError) $ credsIdent creds
        $(logError) $ fromString $ show $ credsExtra creds
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert User
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

--     loginHandler = lift $ defaultLayout $ [whamlet|<div style="width:500px;margin:0 auto">^{login}|]


-- login :: Widget
-- login = toWidget $ {-addCassius $(cassiusFile "login") >> -}$(hamletFile "templates/login.hamlet")

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
mkMessage "App" "messages" "en"
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

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
   angularUIEntry = [whamlet|
 <div layout=column layout-fill ng-cloak>
   <section >
     <div layout="row"  hide-gt-md ng-controller="LeftCtrl"  layout-align="space-between start">
       <md-button ng-click="toggleLeft()" class="md-primary" >
           <span .fa .indent >
           Menu
       <md-button  href=@{AuthR LogoutR} md-ink-ripple="#bbb">
           <span .glyphicon .log-out>
           Logout({{maid | splitChars2:10 }})
     <div layout="row" layout-fill>
      <md-sidenav .md-sidenav-left .md-whiteframe-z2  md-is-locked-open="$media('gt-md')" md-component-id="left" tabindex="-1" style="width:250px" >
          <md-content style="overflow: auto;" .md-default-theme ng-controller="LeftCtrl">
              <md-toolbar style="min-height: 64px; max-height:64px;"  .md-default-theme>
                <h1 .md-toolbar-tools flex layout=row>
                  <a href="" ng-click="goHome()" tabindex=0>
                        Video Selector
              <div ng-repeat="section in sections">
                <a .menu-item .md-menu-item .menu-title  ng-click="unselect(section)" ui-sref={{section.state}} ui-sref-active=active md-ink-ripple="#bbb" tabindex=0 >
                  {{section.name}}
                <a .menu-item .md-menu-item .menu-sub-item
                   ng-show="section.visible"
                   ng-repeat="page in section.pages"
                   ui-sref-active=active
                   ui-sref={{page.state}}
                   md-ink-ripple="#bbb"
                   >
                 <span ng-class=page.icon>
                 {{page.name}}
              <a .menu-item .md-menu-item .menu-title  href=@{AuthR LogoutR} md-ink-ripple="#bbb">
                 <span .glyphicon .log-out>
                 Logout
      <div data-ui-view layout-fill tabindex="-1" role=main>
   |]

angularUILayout :: Text -> WidgetT App IO () ->  HandlerT App IO Html
angularUILayout ngApp widget = do
        void requireAuthId
        master <- getYesod
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR app_css
            $(widgetFile "login-layout")
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
