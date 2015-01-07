{-# Language OverloadedStrings #-}

module Google.Api.Scope  where

import qualified Network.HTTP.Types as H
import Prelude
import Data.Monoid
import Network.HTTP.OAuth2
import Network.HTTP.ClientExtra
-- import Data.String


--------------------------------------------------
-- Google API

-- | This is special for google Gain read-only access to the user's email address.

all :: H.Query
all = H.queryTextToQuery . unQueryE . toQueryE $ email <> userInfo <> userInfo <> glassTimeline <> glassLocation

allScopes = email <> userInfo <> userInfo <> glassTimeline <> glassLocation

email :: Scope
--email = "https://www.googleapis.com/auth/userinfo.email"
email = "email"

-- | Gain read-only access to basic profile information, including a
userInfo :: Scope
--userInfo = "https://www.googleapis.com/auth/userinfo.profile"
userInfo = "profile"

glassTimeline :: Scope
glassTimeline = "https://www.googleapis.com/auth/glass.timeline"

glassLocation :: Scope
glassLocation = "https://www.googleapis.com/auth/glass.location"

-- | Access offline
googleAccessOffline :: H.Query
googleAccessOffline = [("access_type"    , Just  "offline")
                      ,("approval_prompt", Just  "force"  )
                      ]
