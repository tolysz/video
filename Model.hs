{-# Language DeriveGeneric, FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
-- import Database.Persist.MongoDB hiding (master)
-- import Language.Haskell.TH.Syntax
import Types
-- import Data.UUID
import Network.Google.Api.Utils
import Network.Google.Api.Youtube.Videos
import Network.Google.Api.Youtube.Playlists
import Network.Google.Api.Youtube.PlaylistItems


import Data.Aeson.TH       (deriveJSON)
import Types.Persistent

-- import Database.Persist.Sql.Class

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

-- instance PersistFieldSql UUID where
--   sqlType _ = SqlBlob


-- instance FromJSON YTChannel

-- instance FromJSON YoutubeVideo

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- todo: revisit
-- instance FromJSON YTChannel
-- instance ToJSON   YTChannel

deriveJSON (optsL 4)  ''Users
deriveJSON (optsL 9)  ''SiteGroup
deriveJSON (optsL 15) ''SiteGroupMember

data SiteGroupMemberResolved =
   SiteGroupMemberResolved
     { siteGroupMemberResolvedGroup       :: Maybe ShortName
     , siteGroupMemberResolvedUser        :: Maybe Text
     , siteGroupMemberResolvedFullMember  :: Bool
     , siteGroupMemberResolvedUserAdmin   :: Bool
     , siteGroupMemberResolvedVideoAdmin  :: Bool
     }

deriveJSON (optsL 23) ''SiteGroupMemberResolved

data Backup = Backup
  { bMembers         :: [Users          ]
  , bSiteGroup       :: [SiteGroup      ]
  , bSiteGroupMember :: [SiteGroupMember]
  }

deriveJSON (optsL 1)  ''Backup


