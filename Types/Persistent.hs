{-# LANGUAGE  FlexibleInstances #-}
module Types.Persistent where

import Database.Persist.Sql
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Data.Either
import ClassyPrelude
import Types

import Database.PostgreSQL.Simple.FromField as PGS
import Database.PostgreSQL.Simple.Types as PGS
import Network.Google.Api.Youtube.Videos

instance (A.ToJSON a, A.FromJSON a) => PersistFieldSql (TC a) where
   sqlType _ = SqlOther "JSON"

instance (A.ToJSON a, A.FromJSON a) => PersistField (TC a) where
  toPersistValue (TC a) = PersistDbSpecific . L.toStrict . A.encode $ a
  fromPersistValue (PersistByteString bs) = case A.decode' $ L.fromStrict bs of
         Just v -> Right (TC v)
         Nothing -> Left "error parsing json value"
  fromPersistValue (PersistDbSpecific bs) = case A.decode' $ L.fromStrict bs of
       Just v -> Right (TC v)
       Nothing -> Left "error parsing json value"

instance (Typeable a, PGS.FromField a ) => PGS.FromField [a] where
    fromField f dat = PGS.fromPGArray <$> PGS.pgArrayFieldParser PGS.fromField f dat

instance (A.FromJSON a, Typeable a) => PGS.FromField (TC a) where
    fromField f dat = TC <$> PGS.fromJSONField f dat
