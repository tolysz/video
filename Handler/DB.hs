{-# LANGUAGE ScopedTypeVariables #-}
module Handler.DB where

import Import
import Permissions
import Data.Aeson.Lens
import Data.Aeson.Types
import Control.Lens ((^?) , (^.))
import Control.Lens.Iso (non)
-- import Database.Persist.MongoDB
import Data.ByteString.UTF8 (toString)

import qualified Database.Esqueleto as E

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

getBackupR :: ApiReq Backup
getBackupR = TC <$> ( Backup <$> listsOfAllNaked
                             <*> listsOfAllNaked
                             <*> listsOfAllNaked
                             )

postBackupR :: ApiReq Bool
postBackupR = undefined

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
liftJust :: (MonadPlus m) => a -> m a
liftJust = return
-- Insert New user-- post

-- | REST For Group
getSiteGroupR :: ApiReq [   SiteGroup   ]
getSiteGroupR = listsOfAll

postSiteGroupR :: ApiReq SiteGroup
postSiteGroupR = do
        guardAllAdmin
        restOpenM $ \(v :: Value) -> runMaybeT $ do
            siteGroupName   <- liftMaybe (v ^? key "name"   . _String )
            siteGroupShort  <- liftMaybe (v ^? key "short"  . _String )
            siteGroupNotes  <- liftJust  (v ^? key "notes"  . _String )
            siteGroupPublic <- liftJust  (v ^? key "public" . _Bool   ^. non False)
            siteGroupUrl    <- liftJust  (v ^? key "url"    . _String )
            let sg = SiteGroup {..}
            MaybeT $ runDB $ getBy (UniqueSiteGroup siteGroupShort) >>= \case
                Just (Entity uid _) -> replace uid sg >> return (Just $ TC sg)
                Nothing -> insert sg >> return (Just $ TC sg)

getSiteGroup1R :: ShortName -> ApiReq SiteGroup
getSiteGroup1R = jsonDB1 . getBy404 . UniqueSiteGroup

deleteSiteGroup1R :: ShortName -> ApiReq SiteGroup
deleteSiteGroup1R = deleteByReturn UniqueSiteGroup

-- | REST For User
getUserR :: ApiReq [ User ]
getUserR = listsOfAll

postUserR :: ApiReq User
postUserR = do
        guardAllAdmin
        restOpenM $ \(v :: Value) -> runMaybeT $ do
            userIdent     <- liftMaybe (v ^? key "ident"    . _String )
            userName      <- liftJust  (v ^? key "name"     . _String )
            userFriendly  <- liftJust  (v ^? key "friendly" . _String )
--             userSiteAdmin <- liftJust  False -- (v ^? key "siteAdmin" . _Bool )
            userAvatar    <- liftJust  (v ^? key "avatar"   . _String )
            let us = User {..}
            MaybeT $ runDB $ getBy (UniqueUser userIdent) >>= \case
                 -- keep the old admin privs
                Just (Entity uid old) -> replace uid us >> return (Just $ TC us)
                Nothing -> insert us >> return (Just $ TC us)

getUser1R :: EmailQuery -> ApiReq User
getUser1R = jsonDB1 . getBy404 . UniqueUser

deleteUser1R :: EmailQuery -> ApiReq User
deleteUser1R = deleteByReturn UniqueUser

-- | REST For Group membreship
postSiteGroupUserR :: ShortName -> ApiReq SiteGroupMember
postSiteGroupUserR gid = do
   guardAllAdmin
   restOpenM $ \(v :: Value) -> runMaybeT $ do
       textGroup <- liftMaybe (v ^? key "group"      . _String )
       guard (textGroup == gid)
       textUser  <- liftMaybe  (v ^? key "user"       . _String )
       siteGroupMemberFullMember <- liftJust  (v ^? key "fullMember" . _Bool ^. non False)
       siteGroupMemberUserAdmin  <- liftJust  (v ^? key "userAdmin"  . _Bool ^. non False)
       siteGroupMemberVideoAdmin <- liftJust  (v ^? key "videoAdmin" . _Bool ^. non False)
       Just (siteGroupMemberGroup, siteGroupMemberUser) <- MaybeT $ runDB $ return . Just <$> (
              (,) <$> getDBKey (UniqueSiteGroup textGroup)
                  <*> getDBKey (UniqueUser textUser)
                  )
       let us = SiteGroupMember {..}
       MaybeT $ runDB $ do
         getBy (UniqueSiteGroupMember siteGroupMemberGroup siteGroupMemberUser) >>= \case
           Just (Entity uid _) -> replace uid us
           Nothing             -> void $ insert us
         return (Just $ TC us)

getSiteGroupUserR :: ShortName -> ApiReq [SiteGroupMemberResolved]
getSiteGroupUserR gid =
   jsonDBNaked $ do
     groupKey <- getDBKey (UniqueSiteGroup gid)
     memberList <- selectList [SiteGroupMemberGroup ==. groupKey] []
     forM memberList $ \m@(Entity k SiteGroupMember{..}) -> do
          us <- fmap userIdent <$> get siteGroupMemberUser
          return (SiteGroupMemberResolved (Just gid) us siteGroupMemberFullMember siteGroupMemberUserAdmin siteGroupMemberVideoAdmin)
{-
     rawrecs <- runDB $ find (select
     ["loc" =: [
       "$near" =: [
         "$geometry" =: [
           "type" =: ("Point"::String),
           "coordinates" =: [ (28.483334::Double),(49.233334::Double) ]
         ],
         "$maxDistance" =: (1000::Int)
       ]
     ]] "points") { limit = 10 } >>= rest
     mapM_ (liftIO . putStrLn . show) rawrecs
-}

getSiteGroupUser1R :: ShortName -> EmailQuery -> ApiReq SiteGroupMember
getSiteGroupUser1R gid e = do
  (groupKey, userKey) <- runDB $ (,) <$> getDBKey (UniqueSiteGroup gid)
                                     <*> getDBKey (UniqueUser e)
  jsonDB1 . getBy404 $ UniqueSiteGroupMember groupKey userKey

deleteSiteGroupUser1R :: ShortName -> EmailQuery -> ApiReq SiteGroupMember
deleteSiteGroupUser1R gid e = do
  (groupKey, userKey) <- runDB $ (,) <$> getDBKey (UniqueSiteGroup gid)
                                     <*> getDBKey (UniqueUser e)
  deleteByReturn (UniqueSiteGroupMember groupKey) userKey

-- REST for events
postEventR :: ApiReq Event
postEventR = badMethod

getEventR :: ApiReq [ Event ]
getEventR = listsOfAll

-- getEvent1R :: ObjectId -> ApiReq Event
-- getEvent1R = undefined
-- getEvent1R (oidToKey -> eid) = runDB $ TC <$> get404 eid

-- deleteEvent1R :: ObjectId -> ApiReq Event
-- deleteEvent1R = undefined
-- deleteEvent1R (oidToKey -> eid) = deleteReturn eid


-- | todo: find out how to cut this boilerplate!
--   force compiler not to disply signature missing if the type is fully defined otherwise
-- | debug stuff -- mongo admin?
getAllOAuthAccess     :: ApiReq [  OAuthAccess  ]
getAllOAuthAccess     = listsOfAll

getAllEmail           :: ApiReq [     Email     ]
getAllEmail           = listsOfAll

getAllYTChannel       :: ApiReq [   YTChannel   ]
getAllYTChannel       = listsOfAll

getAllYTPlaylist      :: ApiReq [   YTPlaylist  ]
getAllYTPlaylist      = listsOfAll

getAllYTVideoPlaylist :: ApiReq [YTVideoPlaylist]
getAllYTVideoPlaylist = listsOfAll

getAllYTVideo         :: ApiReq [    YTVideo    ]
getAllYTVideo         = listsOfAll

getAllYTVideoUser     :: ApiReq [  YTVideoUser  ]
getAllYTVideoUser     = listsOfAll

getAllChannelMember   :: ApiReq [ ChannelMember ]
getAllChannelMember   = listsOfAll

getAllSiteGroupMember :: ApiReq [SiteGroupMember]
getAllSiteGroupMember = listsOfAll

getAllSiteGroup       :: ApiReq [   SiteGroup   ]
getAllSiteGroup       = listsOfAll

getAllVirtualVideo    :: ApiReq [ VirtualVideo  ]
getAllVirtualVideo    = listsOfAll

getAllPlaylistEvent   :: ApiReq [ PlaylistEvent ]
getAllPlaylistEvent   = listsOfAll

-- | type magic
--   convert any list of all into a respoce; too many things to import
--   just to make this line a happy line

listsOfAllNaked = jsonDB $ selectList [] []
listsOfAll = TC <$> listsOfAllNaked

jsonDB q = do
  guardAllAdmin
  map (\(Entity _ v) -> v) <$> runDB q

jsonDBNaked q = do
  guardAllAdmin
  TC <$> runDB q

jsonDB1 q = do
  guardAllAdmin
  TC . (\(Entity _ v) -> v) <$> runDB q

deleteByReturn f u1 = jsonDB1 $ do
   u@(Entity k _) <- getBy404 (f u1)
   delete k
   return u

deleteReturn k = jsonDBNaked $ do
   u <- get404 k
   delete k
   return u

getDBKey f = do
  Entity ent _ <- getBy404 f
  return ent

eToTC (Entity _ v) = TC v

oneOr404 [a] = return a
oneOr404 _ = notFound

-- | Add some anonymous user, without adding her to any group

assessValue :: (ToJSON s, FromJSON a) => Permssions -> s -> a
assessValue _ _ = undefined

patchValue :: (ToJSON a, FromJSON a) => a -> Value -> Either String a
patchValue t p = resultToEither . fromJSON $ toJSON t

resultToEither (Error a) = Left a
resultToEither (Success a) = Right a

-- | Return a 405 method not supported page.
-- notImplemented :: (RequestReader m, Failure ErrorResponse m) => m a
-- notImplemented = do
--     w <- waiRequest
--     failure $ notImplemented501 $ toString $ requestMethod w
