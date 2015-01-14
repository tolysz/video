{-#LANGUAGE  LambdaCase #-}
module Network.HTTP.OAuth2 where

import Prelude
import Network.HTTP.ClientExtra
import Network.HTTP.ClientExtra.Types
import Network.HTTP.Client
import Network.HTTP.OAuth2.Types
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Method (Method)

import Data.Aeson
import Data.Aeson.Types as DA
import Data.Bool
import Data.Default
import Data.Monoid
import Data.String
import Data.String.QM
import Data.Text (Text)
import Data.Time.Clock (UTCTime (..), getCurrentTime, addUTCTime)
import qualified Data.Text as T
import Data.Text.Encoding(decodeUtf8)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

import Control.Applicative
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow (..))

import Debug.Trace

newtype Scope = Scope [Text]
   deriving (Eq, Show, Read)

instance Monoid Scope where
  mempty = Scope []
  mappend (Scope a) (Scope b) = Scope (a ++ b)

instance Default Scope where
  def = Scope []

instance IsString Scope where
  fromString = Scope . (:[]) . fromString

instance  ToQueryE Scope where
 toQueryE (Scope []) = def
 toQueryE (Scope a) = QueryE [("scope", Just $ T.intercalate " " a)]

bearer bb token = RequestHeadersE $ if T.null token then [] else [("Authorization",  bb <> " " <> token)]

authorizeUrl OAuth2{..} = QueryE [ ("client_id"    , Just oauthClientId)
                                 , ("redirect_uri" , Just oauthRedirectUri)
                                 ]

forceOfflineIncremental = forceOffline  <> QueryE [("include_granted_scopes", Just "true"   )]
forceOffline = QueryE [ ("access_type"         , Just "offline")
                      , ("approval_prompt"     , Just "force"  )
                      , ("response_type"       , Just "code"   )
                      ]

generateAuthUrl :: OAuth2 -> Bool -> Scope ->  String
-- ^ Requested OA2 Scopes Incremental?
generateAuthUrl o@(OAuth2{..}) b s = oauthAuthUri <> BS.unpack ( fromQueryE  (authorizeUrl o <> bool forceOffline forceOfflineIncremental b <> toQueryE s))

traceS a = trace (show a) a

fetchAccessToken :: Text -> Manager -> OAuth2 -> IO (OAuth2Result AuthToken)
-- ^ take access code, manager and oauth setings
fetchAccessToken code mgr OAuth2{..} = do
      res  <- methodJSON mgr "POST" Nothing oauthTokenUri def def (UrlEncode def $ QueryE [("code", Just code),("client_id", Just oauthClientId),("client_secret", Just oauthClientSecret),("grant_type", Just "authorization_code"),("redirect_uri", Just oauthRedirectUri)])
      case res of
        Left (s,i) -> return . Left . traceS $ AuthError (decodeUtf8 $ BSL.toStrict s) (Just i)
        Right (Nothing,_,_) -> return $ Left $  AuthError "error parsing JSON" (Just 200)
        Right (Just z,_,_) -> do
          now <- getCurrentTime
          let zz = z{atExpiresAt = (`addUTCTime` now ) . fromIntegral <$> atExpiresIn z}
          return $ Right $ traceS zz

refreshToken :: AuthToken -> Manager -> OAuth2 -> IO (OAuth2Result AuthToken)
refreshToken o mgr OAuth2{..} =
      methodJSONOAuth mgr "POST" Nothing oauthTokenUri def def ( UrlEncode def $ QueryE [("refresh_token", atRefreshToken o),("client_id", Just oauthClientId),("client_secret", Just oauthClientSecret),("grant_type", Just "refresh_token")]) >>=
       \case
         Left a -> return $ Left a
         Right z -> do
          now <- getCurrentTime
          let zz = z{atRefreshToken = atRefreshToken o, atExpiresAt = (`addUTCTime` now ) . fromIntegral <$> atExpiresIn z}
          return $ Right $ traceS zz

--- revokeToken mgr 
-- oauthRevokeTokenEndpoint

getOAuth2 :: (MonadIO m, MonadThrow m, Functor m, FromJSON a) => Manager -> AuthToken -> String ->  m (OAuth2Result a)
getOAuth2 mgr AuthToken{..} url = methodJSONOAuth mgr "GET" Nothing url def (bearer atTokenType atAccessToken) (EmptyBody def)

putOAuth2 :: (Functor m, MonadIO m, ContentEncoder m b, MonadThrow m, FromJSON a) => Manager -> AuthToken -> String -> b -> m (OAuth2Result a)
putOAuth2 mgr AuthToken{..} url = methodJSONOAuth mgr "PUT" Nothing url def (bearer atTokenType atAccessToken)

patchOAuth2 :: (Functor m, MonadIO m, ContentEncoder m b, MonadThrow m, FromJSON a) => Manager -> AuthToken -> String -> QueryE -> RequestHeadersE -> b -> m (OAuth2Result a)
patchOAuth2 mgr AuthToken{..} url qq hh = methodJSONOAuth mgr "PATCH" Nothing url qq (hh <> bearer atTokenType atAccessToken)

postOAuth2 :: (Functor m, MonadIO m, ContentEncoder m b, MonadThrow m, FromJSON a) => Manager -> AuthToken -> String -> b -> m (OAuth2Result a)
postOAuth2 mgr AuthToken{..} url= methodJSONOAuth mgr "POST" Nothing url def (bearer atTokenType atAccessToken)

getOAuth2BSL mgr AuthToken{..} url = methodBSL mgr "GET" Nothing url def (bearer atTokenType atAccessToken) (EmptyBody def)

methodJSONOAuth :: (MonadIO m, ContentEncoder m b, MonadThrow m, Functor m, DA.FromJSON a) => Manager -> Method -> Maybe CookieJar -> String -> QueryE -> RequestHeadersE -> b -> m (OAuth2Result a)
methodJSONOAuth  a b c d e f g  =
    methodJSON a b c d e f g >>= \case
        Left (s,i) -> let s1 = AuthError (decodeUtf8 $ BSL.toStrict s) (Just i) in {-- (liftIO $ print s1) >>  --} (return $ Left s1)
        Right (Nothing,_,_) -> return $ Left $  AuthError "error parsing JSON" (Just 200)
        Right (Just z,_,_) -> return $ Right z
