{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Google.Api.Mirror.Location where

import           Data.Aeson -- .TH                 (defaultOptions, deriveJSON)
import           Data.Text                     (Text)
import           Prelude                       hiding (id)
import Data.Maybe
--import qualified Prelude                       as P (id)
import Google.Api.Kinds
import Google.Api.Transport
import Data.Default
import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar( Day( ModifiedJulianDay ) )
import Control.Applicative
import Data.Possible


data Location = Location
             { locationId          :: Possible Text
             , locationTimestamp   :: Possible UTCTime
             , locationLatitude    :: Possible Double
             , locationLongitude   :: Possible Double
             , locationAccuracy    :: Possible Double
             , locationDisplayName :: Possible Text
             , locationAddress     :: Possible Text
             } deriving (Show)

{--
  "kind": "mirror#location",
  "id": string,
  "timestamp": datetime,
  "latitude": double,
  "longitude": double,
  "accuracy": double,
  "displayName": string,
  "address": string
--}

instance Default Location where
  def = Location MissingData MissingData MissingData MissingData MissingData MissingData MissingData

instance Default Locations where
  def = Locations []

instance ApiKind Location where
  apiKind _ = "mirror#location"

instance ToJSON Location where
  toJSON a@Location{..} = object
     [ "id"          .= locationId
     , "timestamp"   .= locationTimestamp
     , "latitude"    .= locationLatitude
     , "longitude"   .= locationLongitude
     , "accuracy"    .= locationAccuracy
     , "displayName" .= locationDisplayName
     , "address"     .= locationAddress
     , "kind"        .= apiKind a
     ]

instance FromJSON Location where
   parseJSON (Object v) = do
       kd <- v .: "kind"
       if kd == apiKind (undefined :: Location)
         then 
           Location <$>  v .:?? "id"
                    <*>  v .:?? "timestamp"
                    <*>  v .:?? "latitude"
                    <*>  v .:?? "longitude"
                    <*>  v .:?? "accuracy"
                    <*>  v .:?? "displayName"
                    <*>  v .:?? "address"
         else
           fail "wrong kind"
   parseJSON _ = fail "Not a Setting object"

newtype Locations = Locations [Location] deriving Show

instance ApiKind Locations where
  apiKind _ = "mirror#locationList"

instance ToJSON Locations where
  toJSON a@(Locations l) = object
     [ "items"       .= toJSON l
     , "kind"        .= apiKind a
     ]
instance FromJSON Locations where
   parseJSON (Object v) = do
       kd <- v .: "kind"
       if kd == apiKind (undefined :: Locations)
         then
           do
             l <- v .:? "items" .!= []
             return $ Locations l
         else
           fail "wrong kind"
   parseJSON _ = fail "Not a Setting object"
