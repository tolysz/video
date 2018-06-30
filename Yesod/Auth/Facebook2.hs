{-# LANGUAGE RankNTypes        #-}
-- | @yesod-auth@ authentication plugin using Facebook's
-- server-side authentication flow.
module Yesod.Auth.Facebook2
    ( -- * Authentication plugin
      authFacebook
    , facebookLogin
    , facebookLogout

      -- * Useful functions
    , getUserAccessToken
    , setUserAccessToken

      -- * Advanced
    , deleteUserAccessToken
    ) where

import Prelude
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Arrow ((***), second)
import Data.Maybe (fromJust, isNothing, isJust)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Monoid (mappend, (<>))
import Data.Text (Text)
import Network.Wai (queryString)
import Yesod.Auth
import Yesod.Core
import qualified Data.Text as T
import qualified Facebook as FB
import qualified Yesod.Auth.Message as Msg
import qualified Yesod.Facebook as YF
import Text.Internal.Css  (Block(..))

-- | Route for login using this authentication plugin.
facebookLogin :: AuthRoute
facebookLogin = PluginR "fb" ["login"]


-- | Route for logout using this authentication plugin.  This
-- will log your user out of your site /and/ log him out of
-- Facebook since, at the time of writing, Facebook's policies
-- (<https://developers.facebook.com/policy/>) specified that the
-- user needs to be logged out from Facebook itself as well.  If
-- you want to always logout from just your site (and not from
-- Facebook), use 'LogoutR'.
facebookLogout :: AuthRoute
facebookLogout = PluginR "fb" ["logout"]

catMaybes1 :: [(a, Maybe b)] -> [(a, b)]
catMaybes1 = map ( second fromJust ) . filter ( isJust . snd )

-- | Yesod authentication plugin using Facebook.
-- authFacebook :: (YesodAuth site, YF.YesodFacebook site)

authFacebook :: (YesodAuth m, YF.YesodFacebook m)
           => [FB.Permission] -- ^ Permissions to be requested.
           -> AuthPlugin m
authFacebook perms = AuthPlugin "fb" dispatch login
  where
    -- Get the URL in facebook.com where users are redirected to.
--     getRedirectUrl :: YF.YesodFacebook site => (Route Auth -> Text) -> HandlerT site IO Text
    getRedirectUrl render =
        YF.runYesodFbT $ FB.getUserAccessTokenStep1 (render proceedR) perms
    proceedR = PluginR "fb" ["proceed"]

--     dispatch :: (YesodAuth site, YF.YesodFacebook site) =>
--                 Text -> [Text] -> HandlerT Auth (HandlerT site IO) TypedContent
    -- Redirect the user to Facebook.
    dispatch :: (YesodAuth site)
                 => Text
                 -> [Text]
                 -> AuthHandler site TypedContent
    dispatch "GET" ["login"] = do
        ur <- getUrlRender
        lift $ do
          y <- getYesod
          when (redirectToReferer y) setUltDestReferer
          redirect =<< getRedirectUrl ur
    -- Take Facebook's code and finish authentication.
    dispatch "GET" ["proceed"] = do
        render <- getUrlRender
        query  <- queryString <$> waiRequest
        let proceedUrl = render proceedR
            query' = [(a,b) | (a, Just b) <- query]

        (token, user) <- lift $ YF.runYesodFbT $ do
               tt@(FB.UserAccessToken userId _ _ ) <- FB.getUserAccessTokenStep2 proceedUrl query'
               u <- FB.getUser userId [("fields","email,name,locale")] (Just tt)
               return (tt,u)

        lift $ setUserAccessToken token
        maybe (dispatch "GET" ["kthxbye"]) (lift . setCredsRedirect) (createCreds token user)
    -- Logout the user from our site and from Facebook.
    dispatch "GET" ["logout"] = do
        y      <- lift getYesod
        mtoken <- lift getUserAccessToken
        when (redirectToReferer y) (lift setUltDestReferer)

        -- Facebook doesn't redirect back to our chosen address
        -- when the user access token is invalid, so we need to
        -- check its validity before anything else.
        valid <- maybe (return False) (lift . YF.runYesodFbT . FB.isValid) mtoken

        case (valid, mtoken) of
          (True, Just token) -> do
            render <- getUrlRender
            dest <- lift $ YF.runYesodFbT $ FB.getUserLogoutUrl token (render $ PluginR "fb" ["kthxbye"])
            redirect dest
          _ -> dispatch "GET" ["kthxbye"]
    -- Finish the logout procedure.  Unfortunately we have to
    -- replicate yesod-auth's postLogoutR code here since it's
    -- not accessible for us.  We also can't just redirect to
    -- LogoutR since it would otherwise call setUltDestReferrer
    -- again.
    dispatch "GET" ["kthxbye"] =
        lift $ do
          m <- getYesod
          deleteSession "_ID"
          deleteUserAccessToken
          onLogout
          redirectUltDest $ logoutDest m
    -- Anything else gives 404
    dispatch _ _ = notFound

    -- Small widget for multiple login websites.
--     login :: (YesodAuth site, YF.YesodFacebook site) =>
--              (Route Auth -> Route site)
--           -> WidgetT site IO ()
    login tm = do
        ur <- getUrlRender
        redirectUrl <- handlerToWidget $ getRedirectUrl (ur . tm)
        [whamlet|$newline never
<p>
    <a href="#{redirectUrl}" .facebookLogin > _{Msg.Facebook}
|]
        toWidget [lucius|
           a.facebookLogin {
           padding: 10px 48px;
           background: #3C5A99;
           color: #FFF;
           text-decoration: none;
           background-size: 48px 43px;
           background-position-x: 0px;
           background-repeat: no-repeat;
           display: inline-block;
           width: 250px;
           text-align: end;
           background-image:url(data:image/svg+xml#{semicolon}base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTgiPz4NCjwhLS0gR2VuZXJhdG9yOiBBZG9iZSBJbGx1c3RyYXRvciAxNi4wLjAsIFNWRyBFeHBvcnQgUGx1Zy1JbiAuIFNWRyBWZXJzaW9uOiA2LjAwIEJ1aWxkIDApICAtLT4NCjwhRE9DVFlQRSBzdmcgUFVCTElDICItLy9XM0MvL0RURCBTVkcgMS4xLy9FTiIgImh0dHA6Ly93d3cudzMub3JnL0dyYXBoaWNzL1NWRy8xLjEvRFREL3N2ZzExLmR0ZCI+DQo8c3ZnIHZlcnNpb249IjEuMSIgaWQ9IkxheWVyXzEiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgeG1sbnM6eGxpbms9Imh0dHA6Ly93d3cudzMub3JnLzE5OTkveGxpbmsiIHg9IjBweCIgeT0iMHB4Ig0KCSB3aWR0aD0iMjY2Ljg5M3B4IiBoZWlnaHQ9IjI2Ni44OTVweCIgdmlld0JveD0iMCAwIDI2Ni44OTMgMjY2Ljg5NSIgZW5hYmxlLWJhY2tncm91bmQ9Im5ldyAwIDAgMjY2Ljg5MyAyNjYuODk1Ig0KCSB4bWw6c3BhY2U9InByZXNlcnZlIj4NCjxwYXRoIGlkPSJCbHVlXzFfIiBmaWxsPSIjM0M1QTk5IiBkPSJNMjQ4LjA4MiwyNjIuMzA3YzcuODU0LDAsMTQuMjIzLTYuMzY5LDE0LjIyMy0xNC4yMjVWMTguODEyDQoJYzAtNy44NTctNi4zNjgtMTQuMjI0LTE0LjIyMy0xNC4yMjRIMTguODEyYy03Ljg1NywwLTE0LjIyNCw2LjM2Ny0xNC4yMjQsMTQuMjI0djIyOS4yN2MwLDcuODU1LDYuMzY2LDE0LjIyNSwxNC4yMjQsMTQuMjI1DQoJSDI0OC4wODJ6Ii8+DQo8cGF0aCBpZD0iZiIgZmlsbD0iI0ZGRkZGRiIgZD0iTTE4Mi40MDksMjYyLjMwN3YtOTkuODAzaDMzLjQ5OWw1LjAxNi0zOC44OTVoLTM4LjUxNVY5OC43NzdjMC0xMS4yNjEsMy4xMjctMTguOTM1LDE5LjI3NS0xOC45MzUNCglsMjAuNTk2LTAuMDA5VjQ1LjA0NWMtMy41NjItMC40NzQtMTUuNzg4LTEuNTMzLTMwLjAxMi0xLjUzM2MtMjkuNjk1LDAtNTAuMDI1LDE4LjEyNi01MC4wMjUsNTEuNDEzdjI4LjY4NGgtMzMuNTg1djM4Ljg5NWgzMy41ODUNCgl2OTkuODAzSDE4Mi40MDl6Ii8+DQo8L3N2Zz4NCg==);
           }
        |]

semicolon :: Text
semicolon = ";"

-- | Create an @yesod-auth@'s 'Creds' for a given
-- @'FB.UserAccessToken'@.
createCreds :: FB.UserAccessToken -> FB.User -> Maybe (Creds m)
createCreds (FB.UserAccessToken (FB.Id uid) _ _) FB.User{..} = case userEmail of
                  Just e -> Just $ Creds "fb" e $ catMaybes1 [("graph", Just id_ ), ("gender", T.pack . show <$> userGender), ("locale", userLocale), ("full_name", userName), ("avatar",  Just $ id_ <>"/picture") ]
                  Nothing -> Nothing
  where id_ = "https://graph.facebook.com/" <> uid

-- | Set the Facebook's user access token on the user's session.
-- Usually you don't need to call this function, but it may
-- become handy together with 'FB.extendUserAccessToken'.
-- setUserAccessToken :: MonadHandler m => FB.UserAccessToken
setUserAccessToken :: MonadHandler site => FB.UserAccessToken
                   -> site ()

setUserAccessToken (FB.UserAccessToken (FB.Id userId) data_ exptime) = do
  setSession "_FBID" userId
  setSession "_FBAT" data_
  setSession "_FBET" (T.pack $ show exptime)


-- | Get the Facebook's user access token from the session.
-- Returns @Nothing@ if it's not found (probably because the user
-- is not logged in via @yesod-auth-fb@).  Note that the returned
-- access token may have expired, we recommend using
-- 'FB.hasExpired' and 'FB.isValid'.
-- getUserAccessToken :: MonadHandler m => m (Maybe FB.UserAccessToken)
getUserAccessToken = runMaybeT $ do
  userId  <- MaybeT $ lookupSession "_FBID"
  data_   <- MaybeT $ lookupSession "_FBAT"
  exptime <- MaybeT $ lookupSession "_FBET"
  return $ FB.UserAccessToken (FB.Id userId) data_ (read $ T.unpack exptime)


-- | Delete Facebook's user access token from the session.  /Do/
-- /not use/ this function unless you know what you're doing.
deleteUserAccessToken :: MonadHandler m => m ()
deleteUserAccessToken = do
  deleteSession "_FBID"
  deleteSession "_FBAT"
  deleteSession "_FBET"