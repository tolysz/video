{-# LANGUAGE ScopedTypeVariables #-}
module Handler.DB where

import Import
import Permissions
import Data.Aeson.Lens
import Control.Lens ((^?)) -- , (^.))
-- import Data.Aeson.Types (emptyObject)

-- Module dedicated to accessing Database
getUserChannelsR :: ApiReq [YTChannel]
getUserChannelsR =
  TC . catMaybes <$> do
        uid <- getUserIdent
        runDB $ selectList [ChannelMemberUser ==. uid] []
          >>= mapM (\(Entity _ q) -> get $ channelMemberRef q )

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- Insert -- post
insertUserR :: AppM Value
insertUserR =
        restPermsM permInsertUser $ \(v :: Value) -> runMaybeT $ do
            email <- liftMaybe (v ^? key "email" . _String )
            MaybeT $ runDB $
                getBy (UniqueUser email) >>= \case
                    Just (Entity uid _) -> return $ Just uid
                    Nothing -> Just <$> insert User
                              { userIdent = email
                              , userName      = Nothing
                              , userFriendly  = Nothing
                              , userSiteAdmin = False
                              }
getUserR :: ApiReq [User]
getUserR = do
       hasPerm permListUsers
       TC . map (\(Entity _ v) -> v) <$> runDB (selectList [] [])

putUserR :: AppM ()
putUserR =
    hasPerm permUpdateUser

    