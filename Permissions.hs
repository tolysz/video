module Permissions where

import Import
import Data.Bool

data Permssions = Permssions
   deriving Show

requirePerms _ = return ()

restPermsM :: (FromJSON i, ToJSON o) => Permssions -> (i -> AppM (Maybe o)) -> AppM Value
restPermsM p f = hasPerm p >> restOpenM f

restOpenM :: (FromJSON i, ToJSON o) => (i -> AppM (Maybe o)) -> AppM Value
restOpenM f = requireJsonBody >>= f >>= \case
         Just output -> returnJson output >>= sendResponse
         Nothing -> notFound

hasPerm :: Permssions -> AppM ()
hasPerm p = bool (permissionDenied $ fromString $ show p) (return ()) =<< validPerm p

validPerm :: Permssions -> AppM Bool
validPerm _ = getUserAdmin
 -- return True
-- | Specifc permissions

permInsertUser, permUpdateUser, permListUsers  :: Permssions

permInsertUser = Permssions
permUpdateUser = Permssions
permListUsers  = Permssions
