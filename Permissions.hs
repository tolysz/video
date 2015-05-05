{-# Language DeriveGeneric #-}
module Permissions where

import Import
import Data.Bool
import Data.Default

-- import GHC.Generics
import Data.Aeson
--
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

{--
  import Data.Set { (Set), qualified as Set}
-}

-- | gather user's all permissions


requirePerms _ = return ()

restPermsM :: (FromJSON i, ToJSON o) => Permssions -> (i -> AppM (Maybe o)) -> AppM o
restPermsM p f = hasPerm p >> restOpenM f

restOpenM :: (FromJSON i, ToJSON o) => (i -> AppM (Maybe o)) -> AppM o
restOpenM f = requireJsonBody >>= f >>= \case
         Just output -> returnJson output >>= sendResponse
         Nothing -> notFound

hasPerm :: Permssions -> AppM ()
hasPerm p = hasPermR (fromString $ show p) (validPerm p)

hasPermR :: Text -> AppM Bool -> AppM ()
hasPermR f p = bool (permissionDenied f) (return ()) =<< p


validPerm :: Permssions -> AppM Bool
validPerm _ = getUserAdmin

 -- return True
-- | Specifc permissions

permInsertUser, permUpdateUser, permListUsers  :: Permssions


permAllAdmin :: Permssions
permAllAdmin = def{isAdmin = True }

permInsertUser = def
permUpdateUser = def
permListUsers  = def

guardAllAdmin :: AppM ()
guardAllAdmin  = hasPermR "Not a site admin, sorry ;)" getUserAdmin
