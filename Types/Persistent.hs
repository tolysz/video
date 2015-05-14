module Types.Persistent where

import Database.Persist.Sql
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Data.Either
import ClassyPrelude

import Network.Google.Api.Youtube.Videos

instance PersistFieldSql YoutubeVideo where
   sqlType _ = SqlOther "JSON"

instance PersistField YoutubeVideo where
  toPersistValue = PersistDbSpecific . L.toStrict . A.encode
  fromPersistValue (PersistByteString bs) = case A.decode' $ L.fromStrict bs of
         Just v -> Right v
         Nothing -> Left "error parsing json value"
  fromPersistValue (PersistDbSpecific bs) = case A.decode' $ L.fromStrict bs of
       Just v -> Right v
       Nothing -> Left "error parsing json value"