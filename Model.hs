{-# Language DeriveGeneric, FlexibleInstances #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import Database.Persist.MongoDB hiding (master)
import Language.Haskell.TH.Syntax
import Types
import Google.Api.Utils
import Data.Aeson.TH       (deriveJSON)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
let mongoSettings = mkPersistSettings (ConT ''MongoContext)
 in share [mkPersist mongoSettings]
    $(persistFileWith upperCaseSettings "config/models")


instance FromJSON YTChannel
instance ToJSON   YTChannel

deriveJSON optsL4 ''User
deriveJSON optsL9 ''SiteGroup

instance ToContent SiteGroup where
  toContent = toContent . TC

instance ToTypedContent SiteGroup where
  toTypedContent = toTypedContent . TC

instance ToContent User where
  toContent = toContent . TC

instance ToTypedContent User where
  toTypedContent = toTypedContent . TC

instance ToTypedContent (Entity SiteGroup) where
  toTypedContent (Entity _ s) = toTypedContent s

instance ToTypedContent (Entity User) where
    toTypedContent (Entity _ s) = toTypedContent s

instance ToContent (Entity SiteGroup) where
  toContent (Entity _ s) = toContent s

instance ToContent (Entity User) where
    toContent (Entity _ s) = toContent s