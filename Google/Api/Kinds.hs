{-# LANGUAGE DataKinds, ViewPatterns, ScopedTypeVariables, DeriveGeneric, TypeOperators #-}
module Google.Api.Kinds (ApiKind (..),ListResponse (..)) where

import Data.Text (Text)
import Data.Text as T

import GHC.TypeLits
import Prelude
import Data.Aeson
import Control.Monad
import Prelude     hiding (id)
import Data.Aeson.Types
import Google.Api.Utils
import Data.Possible
import Control.Lens       (makeLenses)
import Data.Typeable
import GHC.Generics


data ApiKind (sym :: Symbol) = ApiKind
data ApiKindR (sym :: Symbol) = ApiKindR

-- type family ApiTag a :: *
-- type family ApiTagR a :: *
-- instance Show a => Show (ApiTagR a)
-- type instance ApiTagR (ApiKind (sym :: Symbol)) = (ApiKindR sym)

instance KnownSymbol sym => Show (ApiKind sym ) where
  show a = symbolVal a

instance KnownSymbol sym => Show (ApiKindR sym ) where
  show a = symbolVal a ++ "ListResponse"

instance KnownSymbol sym => ToJSON (ApiKind sym ) where
  toJSON = toJSON . show

instance KnownSymbol sym => ToJSON (ApiKindR sym ) where
  toJSON = toJSON . show

instance KnownSymbol sym => FromJSON (ApiKind sym ) where
  parseJSON (String ( T.unpack -> a))
     | show (ApiKind :: ApiKind sym) == a = return ApiKind
  parseJSON  _ = mzero

instance KnownSymbol sym => FromJSON (ApiKindR sym ) where
  parseJSON (String ( T.unpack -> a))
     | show (ApiKindR :: ApiKindR sym) == a = return ApiKindR
  parseJSON  _ = mzero


data PageInfo = PageInfo
 { _piTotalResults   :: Int
 , _piResultsPerPage :: Int
 } deriving  (Show, Typeable, Generic)
 
optsPI = defaultOptions { fieldLabelModifier = dropL 3 }

instance FromJSON PageInfo where parseJSON = genericParseJSON optsPI
instance ToJSON   PageInfo where toJSON    = genericToJSON    optsPI
makeLenses ''PageInfo

data  ListResponse a sym = ListResponse
 { _lrKind          :: ApiKindR sym   -- kind of content ++ ListResponse.
 , _lrEtag          :: Text           --
 , _lrNextPageToken :: Possible Text  --
 , _lrPrevPageToken :: Possible Text
 , _lrPageInfo      :: Possible PageInfo
 , _lrItems         :: [a]
 } deriving  (Show, Typeable, Generic)

optsLR = defaultOptions { fieldLabelModifier = dropL 3 }

instance (FromJSON a, KnownSymbol s) => FromJSON (ListResponse a s) where parseJSON = genericParseJSON optsLR
instance (ToJSON a  , KnownSymbol s) => ToJSON   (ListResponse a s) where toJSON    = genericToJSON    optsLR
makeLenses ''ListResponse

-- instance Default YoutubeChannelLR where
   -- def = YoutubeChannelLR ApiKind "" "" MissingData MissingData -- MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData

