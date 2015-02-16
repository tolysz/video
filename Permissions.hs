{-# Language DeriveGeneric #-}
module Permissions where

import Import
import Data.Bool
import Data.Default

import Data.Set (Set)
import qualified Data.Set as Set

{--
  import Data.Set { (Set), qualified as Set}
-}

data Permssions = Permssions
  { isAdmin   :: Possible Bool
  , userGroup :: Set ShortName
  }
   deriving (Show, Typeable, Generic)

instance Default Permssions where
  def = Permssions MissingData Set.empty

-- | gather user's all permissions
userPerms :: AppM Permssions
userPerms = do
         isAdmin <- HaveData <$> getUserAdmin
         return Permssions{..}

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
permAllAdmin = def{ isAdmin = HaveData True}

permInsertUser = def
permUpdateUser = def
permListUsers  = def

guardAllAdmin :: AppM ()
guardAllAdmin  = hasPermR "Not a site admin, sorry ;)" getUserAdmin
