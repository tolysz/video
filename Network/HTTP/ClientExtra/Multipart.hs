{-# LANGUAGE CPP
           , OverloadedStrings
           , FlexibleInstances
           , MultiParamTypeClasses
           , RankNTypes
           #-}
-- | This module handles building multipart/form-data. Example usage:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import Network
-- > import Network.HTTP.Conduit
-- > import Network.HTTP.Conduit.MultipartFormData
-- >
-- > import Data.Text.Encoding as TE
-- >
-- > import Control.Monad
-- >
-- > main = withSocketsDo $ withManager $ \m -> do
-- >     req1 <- parseUrl "http://random-cat-photo.net/cat.jpg"
-- >     res <- httpLbs req1 m
-- >     req2 <- parseUrl "http://example.org/~friedrich/blog/addPost.hs"
-- >     flip httpLbs m =<<
-- >         (formDataBody [partBS "title" "Bleaurgh"
-- >                       ,partBS "text" $ TE.encodeUtf8 "矢田矢田矢田矢田矢田"
-- >                       ,partFileSource "file1" "/home/friedrich/Photos/MyLittlePony.jpg"
-- >                       ,partFileRequestBody "file2" "cat.jpg" $ RequestBodyLBS $ responseBody res]
-- >             req2)
module Network.HTTP.ClientExtra.Multipart
    ( webkitBoundary
    , webkitBoundaryPure
    , Multipart (..)
    , renderParts
    , CE(..)
    ) where

import Prelude

import Network.HTTP.Client (RequestBody(..))
import Network.HTTP.ClientExtra.Types (ContentEncoder(..), cp, RequestHeadersE (..))

import Data.Monoid ((<>), Monoid(..))
import Data.Text (Text)
import qualified Data.ByteString as BS

import Control.Monad.Trans.State.Strict (state, runState)
import Control.Monad.IO.Class
import System.Random
import Data.Array.Base
import Data.Bits
import Data.Word (Word8)

import Control.Monad (replicateM, liftM)
import qualified Data.Text.Encoding as DTE (decodeUtf8)

data CE m where
  CE :: (MonadIO m, ContentEncoder m a) => a -> CE m

instance (MonadIO m) => ContentEncoder m (CE m) where
  renderPart b (CE a) = renderPart b a
  buildBody (CE a) = buildBody a

data Multipart m where
    Multipart :: (MonadIO m) => Text -> RequestHeadersE -> [CE m] -> Multipart m

instance  (MonadIO m) => ContentEncoder m (Multipart m) where
  renderPart _ (Multipart _ _ _) = return $ RequestBodyBS "" -- or maybe join it recursive
  buildBody (Multipart ct eh parts) = do
          boundary <- webkitBoundary
          body <- renderParts boundary parts
          return (body, eh <> (RequestHeadersE [("Content-Type", ct <> "; boundary=" <> DTE.decodeUtf8 boundary)]) )

renderParts :: (MonadIO m) => BS.ByteString -> [CE m] -> m RequestBody
renderParts boundary parts = (fin . mconcat) `liftM` mapM (renderPart boundary) parts
   where fin = (<> cp "--\r\n")

webkitBoundary :: MonadIO m => m BS.ByteString
webkitBoundary = liftIO $ getStdRandom webkitBoundaryPure

webkitBoundaryPure :: RandomGen g => g -> (BS.ByteString, g)
webkitBoundaryPure g = (`runState` g) $ do
    fmap (BS.append prefix . BS.pack . Prelude.concat) $ replicateM 4 $ do
        randomness <- state $ random
        return [ unsafeAt alphaNumericEncodingMap $ randomness `shiftR` 24 .&. 0x3F
               , unsafeAt alphaNumericEncodingMap $ randomness `shiftR` 16 .&. 0x3F
               , unsafeAt alphaNumericEncodingMap $ randomness `shiftR` 8 .&. 0x3F
               , unsafeAt alphaNumericEncodingMap $ randomness .&. 0x3F]
  where
    prefix = "WebKitFormBoundary"
    alphaNumericEncodingMap :: UArray Int Word8
    alphaNumericEncodingMap = listArray (0, 63)
        [ 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48
        , 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F, 0x50
        , 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58
        , 0x59, 0x5A, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66
        , 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E
        , 0x6F, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76
        , 0x77, 0x78, 0x79, 0x7A, 0x30, 0x31, 0x32, 0x33
        , 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x41, 0x42
        ]
