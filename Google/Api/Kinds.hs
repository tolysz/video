{-# LANGUAGE DataKinds, ViewPatterns, ScopedTypeVariables #-}
module Google.Api.Kinds (ApiKind (..)) where

import Data.Text (Text)
import Data.Text as T

import GHC.TypeLits
-- import Data.Proxy
import Prelude
import Data.Aeson
import Control.Monad
import Prelude     hiding (id)
import Data.Aeson.Types

-- class ApiKind a where
--     apiKind :: a -> Text
--     scopeRead  :: a -> [Text]
--     scopeRead = const []
--     scopeWrite :: a -> [Text]
--     scopeWrite = const []
--

data ApiKind (sym :: Symbol) = ApiKind

instance KnownSymbol sym => Show (ApiKind sym ) where
  show a = symbolVal a


instance KnownSymbol sym => ToJSON (ApiKind sym ) where
  toJSON = toJSON . show

instance KnownSymbol sym => FromJSON (ApiKind sym ) where
  parseJSON (String ( T.unpack -> a)) = 
         let ak = (ApiKind :: ApiKind sym)
         in
         if show ak == a
          then
            return ak
          else
            mzero
  parseJSON  _ = mzero
