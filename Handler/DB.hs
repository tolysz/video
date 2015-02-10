{-# LANGUAGE ScopedTypeVariables #-}
module Handler.DB where

import Import
import Permissions
import Data.Aeson.Lens
import Data.Aeson.Types
import Control.Lens ((^?)) -- , (^.))

-- import Database.MongoDB.Query (MongoContext(..))
-- import Data.Aeson.Types (emptyObject)


-- Module dedicated to accessing Database
getUserChannelsR :: ApiReq [YTChannel]
getUserChannelsR =
  TC . catMaybes <$> do
        uid <- getUserIdent
        runDB $ selectList [ChannelMemberUser ==. uid] []
          >>= mapM (\(Entity _ q) -> get $ channelMemberRef q )

-- liftMaybe :: Monad m => Maybe a -> MaybeT m a
-- liftMaybe = MaybeT . return

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

-- Insert New user-- post

getAllUserR = listsOfAll :: ApiReq [     User      ]
postAllSiteGroupR :: AppM Value
postAllSiteGroupR = do
        guardAllAdmin

        -- name     siteGroupName            Text
        -- short    siteGroupShort           ShortName
        -- notes    siteGroupNotes           Text Maybe
        -- public   siteGroupPublic          Bool
        -- url      siteGroupUrl             Text Maybe

        restOpenM $ \(v :: Value) -> runMaybeT $ do
            siteGroupName  <-          liftMaybe (v ^? key "name"   . _String )
            siteGroupShort <-          liftMaybe (v ^? key "short"  . _String )
            siteGroupNotes <- Just <$> liftMaybe (v ^? key "notes"  . _String )
            siteGroupPublic<-          liftMaybe (v ^? key "public" . _Bool   )
            siteGroupUrl   <- Just <$> liftMaybe (v ^? key "url"    . _String )

            MaybeT $ runDB $ getBy (UniqueSiteGroup siteGroupShort) >>= \case
                Just (Entity uid _) -> return $ Just uid
                Nothing -> Just <$> insert SiteGroup {..}

getAllSiteGroupR      = listsOfAll :: ApiReq [   SiteGroup   ]
postAllUserR :: AppM Value
postAllUserR = do
        guardAllAdmin
        restOpenM $ \(v :: Value) -> runMaybeT $ do
            email <- liftMaybe (v ^? key "email" . _String )
            MaybeT $ runDB $ getBy (UniqueUser email) >>= \case
                Just (Entity uid _) -> return $ Just uid
                Nothing -> Just <$> insert User
                          { userIdent     = email
                          , userName      = Nothing
                          , userFriendly  = Nothing
                          , userSiteAdmin = False
                          , userAvatar    = Nothing
                          }



-- | todo: find out how to cut this boilerplate!
--   force compiler not to disply signature missing if the type is fully defined otherwise
-- | debug stuff -- mongo admin?
getAllOAuthAccess     = listsOfAll :: ApiReq [  OAuthAccess  ]
getAllEmail           = listsOfAll :: ApiReq [     Email     ]
getAllYTChannel       = listsOfAll :: ApiReq [   YTChannel   ]
getAllYTPlaylist      = listsOfAll :: ApiReq [   YTPlaylist  ]
getAllYTVideoPlaylist = listsOfAll :: ApiReq [YTVideoPlaylist]
getAllYTVideo         = listsOfAll :: ApiReq [    YTVideo    ]
getAllYTVideoUser     = listsOfAll :: ApiReq [  YTVideoUser  ]
getAllChannelMember   = listsOfAll :: ApiReq [ ChannelMember ]
getAllSiteGroupMember = listsOfAll :: ApiReq [SiteGroupMember]
getAllSiteGroup       = listsOfAll :: ApiReq [   SiteGroup   ]
getAllVirtualVideo    = listsOfAll :: ApiReq [ VirtualVideo  ]
getAllEvent           = listsOfAll :: ApiReq [     Event     ]
getAllPlaylistEvent   = listsOfAll :: ApiReq [ PlaylistEvent ]

-- | type magic
--   convert any list of all into a respoce; too many things to import
--   just to make this line a happy line
listsOfAll = do
    guardAllAdmin
    TC . map (\(Entity _ v) -> v) <$> runDB (selectList [] [])

-- | Add some anonymous user, without adding her to any group

putAllUserR :: AppM Value
putAllUserR =
    restPermsM permAllAdmin $ \(u :: Value) -> runMaybeT $ do
           email <- liftMaybe (u ^? key "userIdent" . _String )
           MaybeT $ runDB $ getBy (UniqueUser email) >>= \case
               Just (Entity uid _) -> return $ Just uid
               Nothing -> Just <$> insert User
                                  { userIdent     = email
                                  , userName      = Nothing
                                  , userFriendly  = Nothing
                                  , userSiteAdmin = False
                                  , userAvatar    = Nothing
                                  }

-- putAllUserR :: AppM ()
-- putAllUserR = guardAllAdmin

{-
postAllUserR :: AppM Value
postAllUserR =
        restPermsM permAllAdmin $ \(v :: Value) -> runMaybeT $ do
            email <- liftMaybe (v ^? key "email" . _String )
            MaybeT $ runDB $ getBy (UniqueUser email) >>= \case
                Just (Entity uid _) -> return $ Just uid
                Nothing -> Just <$> insert User
                          { userIdent = email
                          , userName      = Nothing
                          , userFriendly  = Nothing
                          , userSiteAdmin = False
                          }
-}

assessValue :: (ToJSON s, FromJSON a) => Permssions -> s -> a
assessValue _ _ = undefined

patchValue :: (ToJSON a, FromJSON a) => a -> Value -> Either String a
patchValue t p = resultToEither . fromJSON $ toJSON t

resultToEither (Error a) = Left a
resultToEither (Success a) = Right a
