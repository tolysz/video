{-# LANGUAGE DataKinds, ViewPatterns, ScopedTypeVariables, DeriveGeneric, TypeOperators #-}
module Google.Api.Kinds -- (ApiKind (..),ListResponse (..), AsStr(..))
 (module Google.Api.Kinds )
 where

import Data.Text (Text)
import Data.Text as T

import Prelude
import Data.Aeson
import Data.Aeson.Types
-- import Data.Coerce
import Data.Possible
import Data.Typeable
import Control.Applicative
import Control.Lens       (makeLenses)
import Control.Monad
import Google.Api.Utils
import GHC.TypeLits
import GHC.Generics


data ApiKind (sym :: Symbol) = ApiKind

instance KnownSymbol sym => Show (ApiKind sym ) where
  show = symbolVal

instance KnownSymbol sym => ToJSON (ApiKind sym ) where
  toJSON = toJSON . show
instance KnownSymbol sym => FromJSON (ApiKind sym ) where
  parseJSON (String ( T.unpack -> a))
     | show (ApiKind :: ApiKind sym) == a = return ApiKind
  parseJSON  _ = mzero


newtype AsStr a = AsStr a

instance (Show a) => Show (AsStr a) where
  show (AsStr a) = show a

instance (Show a) => ToJSON (AsStr a) where
  toJSON = toJSON . show

instance (Read a) => FromJSON (AsStr a) where
  parseJSON (String ( read . T.unpack -> a)) = pure (AsStr a)
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
 { _lrKind          :: ApiKind sym   -- kind of content ++ ListResponse. -- we need typelevel string concatenation
 , _lrEtag          :: Text           --
 , _lrNextPageToken :: Possible Text  --
 , _lrPrevPageToken :: Possible Text
 , _lrPageInfo      :: Possible PageInfo
 , _lrItems         :: [a]
 } deriving  (Show, Typeable, Generic)

optsLR = defaultOptions { fieldLabelModifier = dropL 3 }


-- instance type ListBase

-- class ChunkedList a where
--   type typ = ListResponse a sym
--   getList a

instance (FromJSON a, KnownSymbol s) => FromJSON (ListResponse a s) where parseJSON = genericParseJSON optsLR
instance (ToJSON a  , KnownSymbol s) => ToJSON   (ListResponse a s) where toJSON    = genericToJSON    optsLR
makeLenses ''ListResponse

-- instance Default YoutubeChannelLR where
   -- def = YoutubeChannelLR ApiKind "" "" MissingData MissingData -- MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData

