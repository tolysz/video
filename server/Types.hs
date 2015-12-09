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
import Control.Applicative ((<$>))

import Yesod.Core.Content
import Database.Persist.TH
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import GHC.Generics
import Text.Blaze

-- import Control.Lens              (makeLenses)
import Network.Google.Api.Utils  (optsL)-- import Data.String

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

instance Show a => Show (TC a) where
  show (TC a) = show a

instance (FromJSON a) => FromJSON (TC a) where
  parseJSON = fmap TC <$> parseJSON

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
derivePersistField "ViewChan"

-- instance FromJSON YTVideo
-- instance ToJSON   YTVideo

data ThemeRules = ThemeRules
  { themeRulesPrimary    :: !(Maybe Text)
  , themeRulesAccent     :: !(Maybe Text)
  , themeRulesWarn       :: !(Maybe Text)
  , themeRulesBackground :: !(Maybe Text)
  , themeRulesDark       :: !(Maybe Bool)
  }
  deriving (Show, Read, Eq, Typeable, Generic)

deriveJSON (optsL 10) ''ThemeRules

newtype Theme = Theme (Map Text ThemeRules)
  deriving (Show, Read, Eq, Typeable, Generic)

instance ToJSON    Theme
instance FromJSON  Theme

data PlaylistType
  = PLRaw
  | PLPresent
  | PLMambers
  | PLGroup
  | PLSite
  | PLWorld
    deriving (Show, Read, Eq, Typeable, Generic)

derivePersistField "PlaylistType"
instance ToJSON    PlaylistType
instance FromJSON  PlaylistType

type ShortName = Text
type EmailQuery = Text

data EventParticipants
  = EventParticipantsRegardless
  | EventParticipantsOnly
  | DoNotCare
  deriving (Show, Read, Eq, Typeable, Generic)

derivePersistField "EventParticipants"
instance ToJSON    EventParticipants
instance FromJSON  EventParticipants

data ViewPerm
   = VPOnlyMe
   | VPChanMembers
   | VPGuests
   | VPWorld
  deriving (Show, Read, Eq, Typeable, Generic)

derivePersistField "ViewPerm"
instance ToJSON    ViewPerm
instance FromJSON  ViewPerm


--   deriving (Show, Read, Eq, Typeable, Generic)

-- instance ToJSON ShortName
-- instance FromJSON ShortName

-- formal informal rude
-- aerial diffusion

data Permssions = Permssions
  { isAdmin    :: !Bool
  , isLogged   :: !Bool
  , isDebugger :: !Bool
  , permVers   :: !Text
  , userGroup  :: !(Map Text PermissionGroup)
  }
   deriving (Show, Typeable, Generic)

data PermissionGroup = PermissionGroup
  { pgVideoAdmin :: !Bool
  , pgVideoOAuth :: !Bool
  , pgUserAdmin  :: !Bool
  , pgMember     :: !Bool
  }
   deriving (Show, Typeable, Generic)

instance Default Permssions where
  def = Permssions False False False "" Map.empty

instance FromJSON PermissionGroup
instance ToJSON   PermissionGroup

instance FromJSON Permssions
instance ToJSON   Permssions

data EmailLogin = EmailLogin
  { elUser     :: Text
  , elPassword :: Text
  , elName     :: Maybe Text
  , elEmail    :: Text
  }
 deriving (Show, Typeable, Generic)

deriveJSON (optsL 2) ''EmailLogin

-- newtype Photo = Photo ByteString

data DBAction
  = DBNoop
  | DBAdd
  | DBUpdate
  | DBDelete
  | DBMissing
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

