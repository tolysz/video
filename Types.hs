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
import Data.String
import Control.Applicative ((<$>))
import qualified Data.Aeson as DA

import Data.Maybe (fromMaybe)

import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL
-- import Yesod.WebSockets
import qualified Network.WebSockets as WS
data MsgBus = Other Text
    deriving (Show, Eq, Typeable, Generic)

instance FromJSON MsgBus
instance ToJSON   MsgBus


instance IsString MsgBus where
  fromString = Other . fromString

instance WS.WebSocketsData MsgBus where
  fromLazyByteString a = fromMaybe (Other . TL.toStrict . TL.decodeUtf8 $ a) (DA.decode a)
  toLazyByteString   = DA.encode


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
--   deriving (Show, Read, Eq, Typeable, Generic)

-- instance ToJSON ShortName
-- instance FromJSON ShortName
