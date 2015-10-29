{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Types.VideoTag where

import Prelude
import Data.Typeable
import GHC.Generics
import Control.Monad()
-- import Data.Default

-- import qualified Data.Text as T
import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as L
import Network.Google.Api.Utils
import Database.Persist.Sql
import Database.PostgreSQL.Simple.FromField as PGS
-- import Database.PostgreSQL.Simple.Types as PGS

import Data.Aeson.TH       (deriveJSON)

data VideoTagRole
  = VTPresident
  | VTGramarian
  | VTSpeach
  | VTTimekeeper
  | VTTopicMaster
  | VTTableTopicEval
  | VTEval
  | VTGenEval
  | VTAhCounter
  | VTTableTopicAnswer
  | VTHospitality
  | VTHospitalityAnswer
  | VTUnknown

     deriving (Show, Eq, Typeable, Generic)

deriveJSON (optsL 2)  ''VideoTagRole

data VideoTag = VideoTag
 { vtRole    :: [(VideoTagRole,DA.Value)]
 }
  deriving (Show, Eq, Typeable, Generic)

-- deriveJSON (optsL 2)  ''VideoTag

instance DA.FromJSON VideoTag where
 parseJSON (DA.Object v) =
     VideoTag <$> v DA..:? "role" DA..!= []

instance DA.ToJSON VideoTag where
 toJSON (VideoTag role ) =
    DA.object [ "role"  DA..= DA.toJSON role ]

instance PersistFieldSql VideoTag where
   sqlType _ = SqlOther "JSON"

instance PersistField VideoTag where
  toPersistValue a = PersistDbSpecific . L.toStrict . DA.encode $ a
  fromPersistValue (PersistByteString bs) = case DA.decode' $ L.fromStrict bs of
         Just v -> Right v
         Nothing -> Left "error parsing json value"
  fromPersistValue (PersistDbSpecific bs) = case DA.decode' $ L.fromStrict bs of
       Just v -> Right v
       Nothing -> Left "error parsing json value"

instance PGS.FromField VideoTag where
    fromField f dat = PGS.fromJSONField f dat
