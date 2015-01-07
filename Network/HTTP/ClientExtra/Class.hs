{-# Language OverloadedStrings
           , ExistentialQuantification
           , MultiParamTypeClasses
           , FlexibleInstances 
  #-}

module Network.HTTP.ClientExtra.Types where

import Prelude
-- import Blaze.ByteString.Builder (toByteString)
import qualified Network.HTTP.Types.URI    as HU
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8

import qualified Data.Text.Encoding as DTE
import Control.Arrow (first, (***))

import qualified Data.Aeson as DA
import Data.CaseInsensitive (mk)
import Data.Text
import Data.Monoid
import Data.Default
import qualified Data.ByteString.Base64.Lazy as B64

import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Client 

import Blaze.ByteString.Builder

data RawL          = RawL RequestHeadersE BSL.ByteString
data JSON      a   = JSON RequestHeadersE a
data UrlEncode     = UrlEncode  RequestHeadersE QueryE
data UriDataEncode = UriDataEncode RequestHeadersE BSL.ByteString
data EmptyBody     = EmptyBody RequestHeadersE

newtype RequestHeadersE = RequestHeadersE [(Text,Text)] deriving Show
newtype QueryE = QueryE { unQueryE :: HU.QueryText } deriving Show

unRequestHeaders (RequestHeadersE a)= Prelude.map ( (mk . DTE.encodeUtf8) *** DTE.encodeUtf8) a

instance Default RequestHeadersE where
 def = RequestHeadersE []

instance Monoid RequestHeadersE where
  mappend a@(RequestHeadersE _) (RequestHeadersE []) = a
  mappend (RequestHeadersE a) (RequestHeadersE ((bh,bc):bs)) = RequestHeadersE ((bh,bc) : Prelude.filter (\(x, _) -> x /= bh) a) `mappend` RequestHeadersE bs
  mempty = def

class ToQueryE a where
  toQueryE  :: a -> QueryE

class (MonadIO m) => ContentEncoder m a where
  buildBody :: a -> m (RequestBody , RequestHeadersE)
  renderPart :: BS.ByteString -> a -> m RequestBody
  renderPart b part  = do
       (body,eh) <- buildBody part
       return $ renderHeader eh <> body <> renderBoundary b
    where
      renderBoundary b = cp "\r\n--" <> cp b
      renderHeader (RequestHeadersE a) = go a
        where
          go [] = cp "\r\n\r\n"
          go ((t,c):as) = cp "\r\n" <> RequestBodyBS (DTE.encodeUtf8 t) <> cp ": " <> RequestBodyBS (DTE.encodeUtf8 c) <> go as
