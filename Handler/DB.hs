{-# LANGUAGE ScopedTypeVariables #-}
module Handler.DB where

import Import
import Permissions
import Data.Aeson.Lens
import Data.Aeson.Types
import Control.Lens ((^?) , (^.))
import Control.Lens.Iso (non)

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
liftJust :: (MonadPlus m) => a -> m a
liftJust = return
-- Insert New user-- post

getSiteGroupR      = listsOfAll :: ApiReq [   SiteGroup   ]
-- postSiteGroupR :: AppM Value
postSiteGroupR = do
        guardAllAdmin
        restOpenM $ \(v :: Value) -> runMaybeT $ do
            siteGroupName   <-          liftMaybe (v ^? key "name"   . _String )
            siteGroupShort  <-          liftMaybe (v ^? key "short"  . _String )
            siteGroupNotes  <-          liftJust  (v ^? key "notes"  . _String )
            siteGroupPublic <-          liftJust  (v ^? key "public" . _Bool   ^. non False)
            siteGroupUrl    <-          liftJust  (v ^? key "url"    . _String )
            let sg = SiteGroup {..}
            MaybeT $ runDB $ getBy (UniqueSiteGroup siteGroupShort) >>= \case
                Just (Entity uid _) -> replace uid sg >> return (Just $ TC sg)
                Nothing -> insert sg >> return (Just $ TC sg)

oneOr404 [a] = return a
oneOr404 _ = notFound

getSiteGroup1R :: ShortName -> ApiReq SiteGroup
getSiteGroup1R = jsonDB1 . getBy404 . UniqueSiteGroup

deleteSiteGroup1R :: ShortName -> ApiReq SiteGroup
deleteSiteGroup1R u1 =
 jsonDB1 $ do
   u@(Entity k _) <- getBy404 (UniqueSiteGroup u1)
   delete k
   return u

getUserR = listsOfAll :: ApiReq [     User      ]
-- postUserR :: AppM Value
-- postUserR :: ApiReq User
postUserR = do
        guardAllAdmin
        restOpenM $ \(v :: Value) -> runMaybeT $ do
            userIdent     <- liftMaybe (v ^? key "ident"    . _String )
            userName      <- liftJust  (v ^? key "name"     . _String )
            userFriendly  <- liftJust  (v ^? key "friendly" . _String )
            userSiteAdmin <- liftJust  False -- (v ^? key "siteAdmin" . _String )
            userAvatar    <- liftJust  (v ^? key "avatar"   . _String )
            let us = User {..}
            MaybeT $ runDB $ getBy (UniqueUser userIdent) >>= \case
                 -- keep the old admin privs
                Just (Entity uid old) -> let nx = us{userSiteAdmin = Import.userSiteAdmin old} in replace uid nx >> return (Just $ TC nx)
                Nothing -> insert us >> return (Just $ TC us)
getUser1R :: EmailQuery -> ApiReq User
getUser1R = jsonDB1 . getBy404 . UniqueUser

deleteUser1R :: EmailQuery -> ApiReq User
deleteUser1R u1 =
 jsonDB1 $ do
   u@(Entity k _) <- getBy404 (UniqueUser u1)
   delete k
   return u

getSiteGroupUserR gid =
   jsonDB $ do
     Entity ent _ <- getBy404 (UniqueSiteGroup gid)
     selectList [SiteGroupMemberGroup ==. ent] []

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
listsOfAll = jsonDB (selectList [] [])

jsonDB q = do
  guardAllAdmin
  TC . map (\(Entity _ v) -> v) <$> runDB q

jsonDB1 q = do
  guardAllAdmin
  TC . (\(Entity _ v) -> v) <$> runDB q

eToTC (Entity _ v) = TC v

-- | Add some anonymous user, without adding her to any group

assessValue :: (ToJSON s, FromJSON a) => Permssions -> s -> a
assessValue _ _ = undefined

patchValue :: (ToJSON a, FromJSON a) => a -> Value -> Either String a
patchValue t p = resultToEither . fromJSON $ toJSON t

resultToEither (Error a) = Left a
resultToEither (Success a) = Right a
