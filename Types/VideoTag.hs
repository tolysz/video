{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Types.VideoTag where

import Prelude
import Data.Typeable
import GHC.Generics
import Control.Monad
import Data.Default

import qualified Data.Text as T
import qualified Data.Aeson as DA
import qualified Data.ByteString.Lazy as L
import Network.Google.Api.Utils
import Database.Persist.Sql

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
 { vtRole    :: VideoTagRole
 , vtDetails :: DA.Value
 }
  deriving (Show, Eq, Typeable, Generic)

deriveJSON (optsL 2)  ''VideoTag

instance PersistField VideoTag where
  toPersistValue a = PersistDbSpecific . L.toStrict . DA.encode $ a
  fromPersistValue (PersistByteString bs) = case DA.decode' $ L.fromStrict bs of
         Just v -> Right v
         Nothing -> Left "error parsing json value"
  fromPersistValue (PersistDbSpecific bs) = case DA.decode' $ L.fromStrict bs of
       Just v -> Right v
       Nothing -> Left "error parsing json value"
