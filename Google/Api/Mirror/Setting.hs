{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Google.Api.Mirror.Setting where

import           Data.Aeson -- .TH                 (defaultOptions, deriveJSON)
import           Data.Text                     (Text)
import           Prelude                       hiding (id)
import Data.Maybe
import Data.Possible
--import qualified Prelude                       as P (id)
import Google.Api.Kinds
import Google.Api.Transport
import Data.Default

data Setting = Setting
             { settingId    :: Possible Text
             , settingValue :: Text
             } deriving (Show)

instance Default Setting where
  def = Setting MissingData ""

instance ApiKind Setting where
  apiKind _ = "mirror#setting"

instance ToJSON Setting where
  toJSON a@Setting{..} = object
     [ "value"  .= settingValue
     , "id"     .= settingId
     , "kind"   .= apiKind a
     ]

instance FromJSON Setting where
   parseJSON (Object v) = do
          vi <- v .:?? "id"
          vv <- v .: "value"
          kd <- v .: "kind"
          if kd == apiKind (undefined :: Setting)
             then
               return $ Setting vi vv
             else
               fail "wrong kind"
   parseJSON _ = fail "Not a Setting object"

