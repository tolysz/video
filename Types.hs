{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances, RankNTypes   #-}

module Types where

import Data.Text (Text)
-- import qualified Data.Text as T

import Data.Char (toLower, isUpper)
import Data.Bool
import Prelude -- (drop, (.), (++), String)
import Data.Typeable
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson
import Yesod.Core.Content
-- import Data.String
import Database.Persist.TH

import Control.Applicative ((<$>))

import Types.MsgBus

import Data.Maybe (fromMaybe)


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


data TC a = TC a

instance (ToJSON a) => ToJSON (TC a) where
  toJSON (TC a) = toJSON a

instance (ToJSON a) => ToContent (TC a) where
  toContent (TC a) = toContent $ toJSON a

instance (ToJSON a) => ToTypedContent (TC a) where
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

