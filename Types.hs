{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, RankNTypes   #-}

module Types where

-- import qualified Data.Text as T

import Prelude -- (drop, (.), (++), String)
import Data.Aeson
import Data.Aeson.TH
import Data.Bool
import Data.Char (toLower, isUpper)
import Data.Text (Text)
import Data.Typeable
import Data.Default
import Data.Monoid
import Data.Possible

import Yesod.Core.Content
import Database.Persist.TH
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import GHC.Generics
import Text.Blaze
-- import Data.String

-- import Control.Applicative ((<$>))

-- import Types.MsgBus
-- import Types.Lang

-- import Data.Maybe (fromMaybe)

type GUUID = Text
data OAuth2Google = OAuth2Google
  { gaClientId            :: Text
  , gaClientSecret        :: Text
  , gaClientEmail         :: Maybe Text
  , gaClientX509CertUrl   :: Maybe Text
  } deriving (Show, Eq, Typeable, Generic)

instance FromJSON OAuth2Google where
  parseJSON = genericParseJSON
     (defaultOptions
      { fieldLabelModifier = fromCamel 2
      })

fromCamel :: Int -> String -> String
fromCamel n = worker True . drop n
  where
   worker _     []     = []
   worker lastUp (c:cs) =
        bool
           (bool [c] ['_' , toLower c] (isUpper c))
           [toLower c]
           lastUp
         ++ worker (isUpper c) cs


data TC a = TC a deriving Functor
data PC a = PC a deriving Functor

instance (ToJSON a) => ToJSON (TC a) where
  toJSON (TC a) = toJSON a

instance (ToJSON a) => ToContent (TC a) where
  toContent (TC a) = toContent $ toJSON a

instance (ToJSON a) => ToTypedContent (TC a) where
  toTypedContent a = TypedContent typeJson (toContent a )

instance (ToJSON a) => ToJSON (PC a) where
  toJSON (PC a) = toJSON a

instance (ToJSON a) => ToContent (PC a) where
  toContent (PC a) = toContent $ toJSON a

instance (ToJSON a) => ToTypedContent (PC a) where
  toTypedContent a = TypedContent typeJson (toContent a )


-- data ViewPerm = Owner
--               | Granted
--               | Channel
--               | World
--     deriving (Show, Read, Eq, Typeable, Generic)
-- derivePersistField "ViewPerm"

data ViewChan = Owner    -- Puts Channel offline
              | Invites  -- Owner needs to explicitly add members
              | Requests -- People can request join / which owner needs to approve
                         --  channel is listed
              | World    -- everyone lgged can see and enter channel
    deriving (Show, Read, Eq, Typeable, Generic)

instance FromJSON ViewChan
instance ToJSON   ViewChan

-- instance FromJSON YTVideo
-- instance ToJSON   YTVideo

derivePersistField "ViewChan"

data PlaylistType = PLRaw
                  | PLPresent
                  | PLMambers
                  | PLGroup
                  | PLSite
                  | PLWorld
    deriving (Show, Read, Eq, Typeable, Generic)

derivePersistField "PlaylistType"
instance ToJSON PlaylistType

type ShortName = Text

type EmailQuery = Text



--   deriving (Show, Read, Eq, Typeable, Generic)

-- instance ToJSON ShortName
-- instance FromJSON ShortName

-- formal informal rude
-- aerial diffusion

data Permssions = Permssions
  { isAdmin   :: Bool
  , userGroup :: !(Map Text Bool)
  }
   deriving (Show, Typeable, Generic)

instance Default Permssions where
  def = Permssions False Map.empty

instance FromJSON Permssions
instance ToJSON   Permssions


-- newtype Photo = Photo ByteString

data DBAction
  = DBNoop
  | DBAdd
  | DBUpdate
  | DBDelete
  | DBApiFail

 deriving (Show, Typeable, Generic)

instance FromJSON DBAction
instance ToJSON   DBAction

instance ToMarkup a => ToMarkup (Possible a) where
  toMarkup = possible "" "???" toMarkup

instance ToMarkup a => ToMarkup (Maybe a) where
  toMarkup = maybe "" toMarkup

instance ToMarkup Value where
  toMarkup = unsafeLazyByteString . encode

instance ToJSON a => ToMarkup (TC a) where
  toMarkup (TC a)= toMarkup (toJSON a :: Value)

instance (ToMarkup a, ToMarkup b) => ToMarkup (a,b) where
  toMarkup (a,b) = unsafeLazyByteString "(" <> toMarkup a <> unsafeLazyByteString ", " <> toMarkup b <> unsafeLazyByteString ")"