{-# Language OverloadedStrings
           , ExistentialQuantification #-}

module Network.HTTP.ClientExtra
 ( -- RequestType (..)
   QueryE (..)
 , ToQueryE (..)
 , RequestHeadersE (..)
 , HH.RequestHeaders
 --, Part (..)
 , methodBSL
 , methodJSON
 , fromQueryE
 , fromQueryE'
 ) where

import Network.HTTP.Client
 
-- import Network.HTTP.Client.MultipartFormData
import Network.HTTP.ClientExtra.Multipart()
import Network.HTTP.ClientExtra.Types
import Network.HTTP.Types.Method (Method)
import qualified Network.HTTP.Types.Header as HH
import qualified Network.HTTP.Types.Status as HS
-- import qualified Network.HTTP.Types.URI    as HU
-- import qualified Data.Text.Encoding as DTE

import qualified Data.Aeson as DA
-- import Data.CaseInsensitive (mk)

import Control.Applicative ((<$>))
-- import Control.Arrow (first, (***))
import Control.Monad.IO.Class (MonadIO, liftIO)
-- import qualified Control.Exception as Ex
import Control.Monad.Catch (MonadThrow (..))

import Data.Monoid
import Data.Either
-- import Data.Default
-- import Data.Text
import Prelude

import Debug.Trace

import qualified Data.ByteString.Lazy as BSL
-- import qualified Data.ByteString as BS
-- import qualified Data.Text.Encoding as TE
-- import Blaze.ByteString.Builder -- (toByteString)

methodBSL :: (MonadIO m, ContentEncoder m b, MonadThrow m) => Manager -> Method -> Maybe CookieJar -> String -> QueryE -> RequestHeadersE -> b -> m (Either (BSL.ByteString, Int) (BSL.ByteString, CookieJar, HH.ResponseHeaders))
methodBSL manager m j url extraQuery extraHeaders reqBody = do
   initReq <- parseUrl url
   (bb,eh) <- buildBody reqBody
   let req = initReq
              { method = m
              , requestHeaders = unRequestHeaders $ eh <> extraHeaders
              , queryString = fromQueryE . (<> extraQuery) . toQueryE . queryString $ initReq
              , requestBody = bb
              , cookieJar=j
              , checkStatus = \_ _ _ ->  Nothing -- cc (checkStatus initReq)
              }
   liftIO $ withResponse req manager $ \rb' -> do
          let cj = responseCookieJar rb'
              rh = responseHeaders rb'
          rb <- BSL.fromChunks <$> brConsume (responseBody rb')
          return $ case HS.statusCode (responseStatus rb') of
              200 -> Right (rb, cj, rh)
              s   -> Left  (rb, s )
          --  Status ResponseHeaders CookieJar

methodJSON :: (MonadIO m, ContentEncoder m b, MonadThrow m, Functor m) => (DA.FromJSON a) => Manager -> Method -> Maybe CookieJar -> String -> QueryE -> RequestHeadersE -> b -> m (Either (BSL.ByteString, Int) (Maybe a, CookieJar, HH.ResponseHeaders))
methodJSON a b c d e f g = fmap (\(a1,b1,c1) -> (DA.decode (trace (show a1) a1),b1,c1)) <$> methodBSL a b c d e f g
