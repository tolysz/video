{-# Language RecordWildCards #-}

module Database.Neo.NeoResponse where

import Data.Possible
import Data.Aeson
import Data.Default
import Data.Aeson.Types
import Control.Applicative
import Data.Text (Text)
import Prelude (Show)

data NeoResponse a = NeoResponse
  { nrColumns :: [Text]
  , nrData    :: [a]
  , nrStats   :: Possible Value
  } deriving Show

instance Default (NeoResponse a) where
  def = NeoResponse [] [] MissingData

instance FromJSON a => FromJSON (NeoResponse a) where
   parseJSON (Object v) = NeoResponse
      <$> v .:   "columns"
      <*> v .:   "data"
      <*> v .:?? "stats"

instance (ToJSON a) => ToJSON (NeoResponse a) where
  toJSON NeoResponse{..} = object
     [ "columns" .= nrColumns
     , "data"    .= toJSON nrData
     , "stats"   .= nrStats
     ]
