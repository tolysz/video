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
import Data.CaseInsensitive (CI(..))
import qualified Data.Text.Encoding as DTE
import Control.Arrow ((***))

import qualified Data.Aeson as DA
import Data.CaseInsensitive (mk)
import Data.Text
import Data.Monoid
import Data.Default
import qualified Data.ByteString.Base64.Lazy as B64

import Control.Monad.IO.Class (MonadIO)
import Network.HTTP.Client 

import Blaze.ByteString.Builder

data RawL          = RawL RequestHeadersE BSL.ByteString
data JSON      a   = JSON RequestHeadersE a
data UrlEncode     = UrlEncode  RequestHeadersE QueryE
data UriDataEncode = UriDataEncode RequestHeadersE BSL.ByteString
data EmptyBody     = EmptyBody RequestHeadersE

newtype RequestHeadersE = RequestHeadersE [(Text,Text)] deriving Show
newtype QueryE = QueryE { unQueryE :: HU.QueryText } deriving Show

unRequestHeaders :: RequestHeadersE -> [(CI BS.ByteString, BS.ByteString)]
unRequestHeaders (RequestHeadersE a)= Prelude.map ( (mk . DTE.encodeUtf8) *** DTE.encodeUtf8) a

instance Default RequestHeadersE where
 def = RequestHeadersE []

instance Monoid RequestHeadersE where
  mappend a@(RequestHeadersE _) (RequestHeadersE []) = a
  mappend (RequestHeadersE a) (RequestHeadersE ((bh,bc):bs)) = RequestHeadersE ((bh,bc) : Prelude.filter (\(x, _) -> x /= bh) a) `mappend` RequestHeadersE bs
  mempty = def

class ToQueryE a where
  toQueryE  :: a -> QueryE

instance Default QueryE where
 def = QueryE []
instance Monoid QueryE where
 (QueryE a) `mappend` (QueryE b) = QueryE (a ++ b)
 mempty = def

instance ToQueryE BS.ByteString where
  toQueryE    =  QueryE . HU.parseQueryText

fromQueryE :: QueryE -> BS.ByteString
fromQueryE  = toByteString . HU.renderQueryText True  . unQueryE

fromQueryE' :: QueryE -> BS.ByteString
fromQueryE' = toByteString . HU.renderQueryText False . unQueryE

class (MonadIO m) => ContentEncoder m a where
  buildBody :: a -> m (RequestBody , RequestHeadersE)
  renderPart :: BS.ByteString -> a -> m RequestBody
  renderPart b part  = do
       (body,eh) <- buildBody part
       return $ renderHeader eh <> body <> renderBoundary b
    where
      renderBoundary b1 = cp "\r\n--" <> cp b1
      renderHeader (RequestHeadersE a) = go a
        where
          go [] = cp "\r\n\r\n"
          go ((t,c):as) = cp "\r\n" <> RequestBodyBS (DTE.encodeUtf8 t) <> cp ": " <> RequestBodyBS (DTE.encodeUtf8 c) <> go as

{-# INLINE cp #-}
cp :: BS.ByteString -> RequestBody
cp bs = RequestBodyBuilder (fromIntegral $ BS.length bs) $ copyByteString bs

instance  (MonadIO m) => ContentEncoder m RawL where
  buildBody (RawL eh a)   = return (RequestBodyLBS a , eh )

instance  (MonadIO m) => ContentEncoder m UriDataEncode where
  buildBody (UriDataEncode eh ct) = buildBody $ RawL (eh <> RequestHeadersE [("Content-Type", dte)] ) b64
     where
       (dte,b64) = ( DTE.decodeUtf8 . BSL.toStrict . fst . BSL8.break (==';') . BSL.drop 1 . snd . BSL8.break (==':') *** B64.decodeLenient . BSL.drop 1) . BSL8.break (== ',') $ ct

instance  (MonadIO m) => ContentEncoder m EmptyBody where
  buildBody (EmptyBody eh)   = return (RequestBodyLBS BSL.empty      , eh )

instance  (MonadIO m, DA.ToJSON a) => ContentEncoder m (JSON a) where
  buildBody (JSON eh a)      = return (RequestBodyLBS $ DA.encode a , eh <> RequestHeadersE [("Content-Type", "application/json")])

instance  (MonadIO m) => ContentEncoder m UrlEncode  where
  buildBody (UrlEncode eh q) = return (RequestBodyBS $ fromQueryE' q, eh <> RequestHeadersE [("Content-Type", "application/x-www-form-urlencoded")])

