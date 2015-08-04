{-# LANGUAGE ScopedTypeVariables #-}
--language="PostgreSQL" Pattern=/Query\ \[qq\|(.*)\|\])/

module Handler.DB where

import Import
import Permissions
import Data.Aeson.Lens
import Data.Int
-- import qualified Data.Maybe DM
import qualified Data.Aeson as A
import Data.Aeson.Types
import Control.Lens ((^?) , (^.), (&))
import Control.Lens.Iso (non)
import Data.ByteString.UTF8 (toString)
import Model as M
import Control.Arrow ((***))


import Data.Maybe (fromJust, Maybe (..))

import Network.Google.Api.Youtube.Videos
import Network.Google.Api.Youtube.Playlists
import Network.Google.Api.Youtube.PlaylistItems
import qualified Database.Esqueleto as E

import Data.String.QM
import qualified Database.PostgreSQL.Simple     as TQ
import qualified Database.PostgreSQL.Simple.TypedQuery   as TQ
import qualified Database.Persist.Sql as P
import qualified Data.Text as T
import qualified Data.Vector as V

-- Module dedicated to accessing Database
getUserChannelsR :: ApiReq [YTChannel]
getUserChannelsR =
  TC . catMaybes <$> do
        uid <- getUserIdent
        runDB $ selectList [ChannelMemberUserId ==. uid] []
          >>= mapM (\(Entity _ q) -> get $ channelMemberRef q )

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

-- todo: now broken
postSiteGroupR :: ApiReq SiteGroup
postSiteGroupR = do
        guardAllAdmin
        restOpenM $ \(v :: Value) -> runMaybeT $ do
            siteGroupName   <- liftMaybe (v ^? key "name"   . _String )
            siteGroupShort  <- liftMaybe (v ^? key "short"  . _String )
            siteGroupNotes  <- liftJust  (v ^? key "notes"  . _String )
            siteGroupPublic <- liftJust  (v ^? key "public" . _Bool   ^. non False)
            siteGroupUrl    <- liftJust  (v ^? key "url"    . _String )
            siteGroupUuidM  <- liftJust  (v ^? key "uuid"   . _String )
            -- we want it to be lazy
            MaybeT $ runDB $
              maybe (return Nothing) (getBy . UniqueSiteGroup) siteGroupUuidM >>= \case
                Just (Entity uid _) ->
                   let
                     Just siteGroupUuid = siteGroupUuidM
                     sg = SiteGroup {..}
                   in
                   replace uid sg >> return (Just $ TC sg)
                Nothing -> do
                   siteGroupUuid <- newUUID
                   let sg = SiteGroup {..}
                   insert sg >> return (Just $ TC sg)

getSiteGroup1R :: Text -> ApiReq SiteGroup
getSiteGroup1R = jsonDB1 . getBy404 . UniqueSiteGroup

deleteSiteGroup1R :: Text  -> ApiReq SiteGroup
deleteSiteGroup1R = deleteByReturn UniqueSiteGroup

-- | REST For User
getUserR :: ApiReq [Value]
getUserR = do
      guardAllAdmin
      TC <$> runRawDB $(TQ.genJsonQuery [qq|
       select uuid     as uuid     -- Text
            , name     as name     -- Maybe  Text
            , friendly as friendly -- Maybe  Text
            , avatar   as avatar   -- Maybe  Text
            , deleted  as deleted  -- Bool
            , emails   as emails   -- Maybe [Text]
       from users
       left outer join (select email.user_id as id
             , array_agg(email.email) as emails
             from email
             group by email.user_id
             ) as em on users.id = em.id
      |])
-- listsOfAll


getUserbyEmail :: Maybe Text -> Handler (Maybe Text)
getUserbyEmail Nothing = return Nothing
getUserbyEmail (Just email) = do
   uid <- P.fromSqlKey <$> requireAuthId
   safeHead <$> runRawDB $(TQ.genTypedQuery [qq|
    select uuid  as uuid   -- Text
    from users
    left join email on users.id = email.user_id
    where
     email.email = ?  -- < email
   |])

safeHead []    = Nothing
safeHead (a:_) = Just a

postUserR :: ApiReq Users
postUserR = do
        guardAllAdmin
        restOpenM $ \(v :: Value) -> do
         liftIO $ print v
         ~uem <- getUserbyEmail (v ^? key "email" . _String )
         ~uuu <- (Just <$> newUUID)
         let userUuid' = (v ^? key "uuid" . _String ) <|> uem <|> uuu

         runMaybeT $ do
            usersUuid      <- liftMaybe userUuid'
            usersName      <- liftJust  (v ^? key "name"     . _String )
            usersFriendly  <- liftJust  (v ^? key "friendly" . _String )
--             userSiteAdmin <- liftJust  False -- (v ^? key "siteAdmin" . _Bool )
            usersAvatar    <- liftJust  (v ^? key "avatar"   . _String )
            let usersDeleted = False
            let us = Users {..}
            MaybeT $ runDB $ getBy (UniqueUsers usersUuid) >>= \case
                 -- keep the old admin privs
                Just (Entity uid old) -> replace uid us >> return (Just $ TC us)
                Nothing -> do
                  let Just usersEmail  =  (v ^? key "email" . _String )
                  uu <- insert us
                  insert $ Email usersEmail uu
                  return (Just $ TC us)

getUser1R :: GUUID -> ApiReq Value
getUser1R uuid = do
   guardAllAdmin
   runRawDB $(TQ.genJsonQuery [qq|
    select uuid     as uuid     -- Text
         , name     as name     -- Maybe  Text
         , friendly as friendly -- Maybe  Text
         , avatar   as avatar   -- Maybe  Text
         , emails   as emails   -- Maybe [Text]
    from users
    left outer join (select email.user_id as id
          , array_agg(email.email) as emails
          from email
          group by email.user_id
          ) as em on users.id = em.id
    where
     users.uuid = ? -- Text -- < uuid
   |]) >>= \case
    [u] -> return $ TC u
    _   -> notFound

-- jsonDB1 . getBy404 . UniqueUsers

deleteUser1R :: EmailQuery -> ApiReq Users
deleteUser1R em = jsonDB1 $ do
     us@(Entity userId user) <- getBy404 $ UniqueUsers em
     update userId [UsersDeleted =. True]
--      deleteWhere [EmailUser  ==. userId]
--      deleteWhere [SiteGroupMemberUser  ==. userId]
--      deleteWhere [UserThemeUser  ==. userId]
--      deleteWhere [SiteAdminUser  ==. userId]
--      deleteWhere [OAuthAccessUser  ==. userId]
--      deleteWhere [GroupFriendUser  ==. userId]
--      delete userId
--      se
     return us

-- | REST For Group membreship
deleteSiteGroupUser0R :: ApiReq SiteGroupMember
deleteSiteGroupUser0R = do
   guardAllAdmin
   restOpenM $ \(v :: Value) -> do
    runMaybeT $ do
       textGroup <- liftMaybe (v ^? key "group"      . _String )
--        guard (textGroup == gid)
       usersUuid  <- liftMaybe  (v ^? key "user"       . _String )
       siteGroupMemberFullMember <- liftJust  (v ^? key "fullMember" . _Bool ^. non False)
       siteGroupMemberUserAdmin  <- liftJust  (v ^? key "userAdmin"  . _Bool ^. non False)
       siteGroupMemberVideoAdmin <- liftJust  (v ^? key "videoAdmin" . _Bool ^. non False) -- False if not a site admin
       Just (siteGroupMemberGroupId, siteGroupMemberUserId) <- MaybeT $ runDB $ return . Just <$> (
              (,) <$> getDBKey (UniqueSiteGroup textGroup)
                  <*> getDBKey (UniqueUsers usersUuid)
                  )
       let us = SiteGroupMember {..}
       MaybeT $ runDB $ do
         deleteBy (UniqueSiteGroupMember siteGroupMemberGroupId siteGroupMemberUserId)
         return (Just $ TC us)

-- createUserFromEmail :: Maybe Text -> AppM Maybe

getUserbyEmailOrUUIDorCreate :: Maybe Text -> Maybe Text -> AppM Text
-- user uuid -- email
getUserbyEmailOrUUIDorCreate Nothing Nothing   = invalidArgs ["user", "email"]
getUserbyEmailOrUUIDorCreate (Just u) Nothing  = return u
getUserbyEmailOrUUIDorCreate (Just u) memail = getUserbyEmail memail >>= \case
   Just euuid -> if u == euuid
                  then
                    return u
                  else
                    notFound -- maybe merge users
   Nothing -> notFound -- maybe add email to user
getUserbyEmailOrUUIDorCreate Nothing memail@(Just email) = getUserbyEmail memail >>= \case
   Just euuid -> return euuid
   Nothing -> do
      uuu <- newUUID
      runRawDBT $ \ c -> do
       $(TQ.genTypedQuery [qq|
          insert into users
            (uuid -- Text -- < uuu
            )
         |]) c
       $(TQ.genTypedQuery [qq|
          insert into email
              ( email   -- Text -- < email
              , user_id -- ~ currval(pg_get_serial_sequence('users', 'id'))
              ) |]) c
      return uuu

postSiteGroupUser0R :: ApiReq SiteGroupMember
postSiteGroupUser0R = do
   guardAllAdmin
   restOpenM $ \(v :: Value) -> do
    userUuid <- getUserbyEmailOrUUIDorCreate (v ^? key "user"  . _String ) (v ^? key "email" . _String )
    runMaybeT $ do
       textGroup <- liftMaybe (v ^? key "group"      . _String )
--        guard (textGroup == gid)
       siteGroupMemberFullMember <- liftJust  (v ^? key "fullMember" . _Bool ^. non False)
       siteGroupMemberUserAdmin  <- liftJust  (v ^? key "userAdmin"  . _Bool ^. non False)
       siteGroupMemberVideoAdmin <- liftJust  (v ^? key "videoAdmin" . _Bool ^. non False) -- False if not a site admin
       Just (siteGroupMemberGroupId, siteGroupMemberUserId) <- MaybeT $ runDB $ return . Just <$> (
              (,) <$> getDBKey (UniqueSiteGroup textGroup)
                  <*> getDBKey (UniqueUsers userUuid)
                  )
       let us = SiteGroupMember {..}
       MaybeT $ runDB $ do
         getBy (UniqueSiteGroupMember siteGroupMemberGroupId siteGroupMemberUserId) >>= \case
           Just (Entity uid _) -> replace uid us
           Nothing             -> void $ insert us
         return (Just $ TC us)

getSiteGroupUserR :: Text -> ApiReq [Value]
getSiteGroupUserR gid = do
   guardAllAdmin >> TC <$> runRawDB $(TQ.genJsonQuery [qq|
     select g.uuid      as group        -- Text
          , u.uuid      as user         -- Text
          , full_member as fullMember   -- Bool
          , video_admin as videoAdmin   -- Bool
          , user_admin  as userAdmin    -- Bool
          , u.name      as name         -- Maybe  Text
          , friendly    as friendly     -- Maybe  Text
          , avatar      as avatar       -- Maybe  Text
          , emails      as emails       -- Maybe [Text]
     from site_group_member as gm
     left join site_group   as g on g.id = gm.group_id
     left join users         as u on u.id = gm.user_id
     left outer join (select e.user_id as id
          , array_agg(e.email) as emails
          from email as e
          group by e.user_id
          ) as em on u.id = em.id
     where
      g.uuid = ? -- < gid
      and not u.deleted
   |])

getUserGroupsR :: ApiReq [(SiteGroup,SiteGroupMember)]
getUserGroupsR =
  TC . map (E.entityVal *** E.entityVal) <$> do
  aid <- requireAuthId
  runDB $
     E.select (
     E.from   $ \(sgm `E.InnerJoin` sg) -> do
     E.on     $ sg  E.^. SiteGroupId         E.==. sgm E.^. SiteGroupMemberGroupId
     E.where_ $ sgm E.^. SiteGroupMemberUserId E.==. E.val aid
     return (sg, sgm)
     )


postVideoUser0R :: Handler Text
postVideoUser0R = do
--     $(logWarn) =<< requestBodyText
    restOpenM $ \(v :: Value) -> runMaybeT $ do
        users  <- liftMaybe (catMaybes . map (parseMaybe parseJSON) . V.toList <$> v ^? key "user_uuids" . _Array )
        video  <- liftMaybe (v ^? key "video_uuid" . _String )
        $(logWarn) ( T.pack $ show  (users ::  [Text], video))
        runRawDB ( \c -> do
             qr <- $(TQ.genTypedQuery [qq|
    select u.id    -- Int64
         , v.id    -- Int64
      from users as u
         , y_t_video as v
     where u.uuid in ? -- < users
       and v.uuid =  ? -- < video
       and u.id not in (select id from y_t_video_user as vu where vu.video = v.id)
         |]) c
             liftIO $ print "insert"
             TQ.executeMany c "insert into y_t_video_user ( user_id, video ) values (?,?)" qr
         )

        return $ Just ()

--         $(logWarn) ( T.pack $ show qr )
    return ""

-- { "user_uuids":
--     [ "9cdf7979-2dd6-4107-b9a3-20b3bc2ab4d4"
--     , "dcd5c0a5-ef5e-4bc1-a0d7-a79ee7245367"
--     ]
-- , "video_uuid": "eb8e6ee9-027c-4ca4-9ba1-8bcd3f18821c"
-- , "playlist_uuid":"fc830ae6-4c71-4548-a085-a35d6ea7f4b0"
-- , "group_uuid":"7590f9b6-9422-4218-baaa-1d29b8eafa56"
-- }
 {-
    video         YTVideoId
    userId        UsersId
    created       UTCTime default=now()
    eventPerm     EventParticipants default='EventParticipantsRegardless'
    viewPerm
    -}
getVideoUserR :: Text -> ApiReq [Text]
getVideoUserR vid = do
  guardAllAdmin >> TC <$> runRawDB $(TQ.genTypedQuery [qq|
    select u.uuid
      from y_t_video_user as vu
 left join users          as u on vu.user_id = u.id
 left join y_t_video      as v on vu.video   = v.id
     where
      v.uuid = ? -- < vid
 |])

-- Misc
defTheme :: Theme
defTheme = fromJust $ A.decode [qq|{
 "main-menu": {
  "dark": false,
  "accent": "purple",
  "primary": "deep-purple",
  "warn": "cyan",
  "background": "blue"
 },
 "sub-menu": {
  "dark": false,
  "accent": "purple",
  "primary": "pink",
  "warn": "light-blue",
  "background": "light-blue"
 },
 "default": {
  "dark": false,
  "accent": "pink",
  "primary": "teal",
  "warn": "lime",
  "background": "amber"
 }
}|]


getUserThemeR :: ApiReq Theme
getUserThemeR = maybeAuthId >>= \case
  Nothing -> rtd
  Just u ->  runDB ( E.select $
                 E.from   $ \yv -> do
                 E.where_
                    (yv E.^. UserThemeUserId E.==. E.val u)
                 return yv )
              >>= \case
               [Entity _ (UserTheme _ (Just t)) ] -> return t
               _        -> rtd
 where
    rtd = return  $ TC defTheme

deleteUserThemeR :: AppM ()
deleteUserThemeR = do
  uid <- requireAuthId
  runDB $ deleteBy (UniqueUserTheme uid)

postUserThemeR :: ApiReq Theme
postUserThemeR = do
        guardAllAdmin
        uid <- requireAuthId
        restOpenM $ \(v :: Theme) -> runMaybeT $ do
            MaybeT $ runDB $ do
              getBy (UniqueUserTheme uid)>>= \case
                Just (Entity k _) -> repsert k (UserTheme uid (Just $ TC v))
                Nothing           -> insert_ (UserTheme uid (Just $ TC v))
              return $ Just (TC v)

getUserGroupsPublicR :: ApiReq [SiteGroup]
getUserGroupsPublicR =
  TC . map E.entityVal <$> (
  maybeAuthId >>= \case
   Just aid ->
      runDB $
         E.select $
         E.from   $ \sg -> do
         E.where_ $ (sg  E.^. SiteGroupPublic E.==. E.val True)
            E.&&. E.notExists ( E.from $ \sgm -> E.where_ (
               (sg  E.^. SiteGroupId  E.==. sgm E.^. SiteGroupMemberGroupId)
               E.&&. (sgm E.^. SiteGroupMemberUserId E.==. E.val aid) ))
         return sg
   Nothing ->
      runDB $
         E.select $
            E.from   $ \sg -> do
            E.where_ $ sg  E.^. SiteGroupPublic E.==. E.val True
            return sg
    )


-- todo: agregate queries
updateYTVideo :: Key SiteGroup -> Text -> Text -> Text -> ApiReq (Maybe YoutubeVideo) -> ApiReq (DBAction,Text)
updateYTVideo gr gu i e rq =
  runDB ( E.select $
     E.from   $ \yv -> do
     E.where_
        (yv E.^. YTVideoRef E.==. E.val i)
     return yv )
  >>= \case
   [] ->
      rq >>= \case
        TC (Just v) -> do
            runRawDB $(TQ.genJsonQuery [qq|
            insert into y_t_video
              ( ref         -- Text  -- < i
              , etag        -- Text  -- < e
              , snippet     -- Value -- < toJSON (Just $ TC v)
              , google_user -- Text  -- < gu
              , group_id    -- Int64 -- < P.fromSqlKey gr
              ) |])
            return $ TC (DBAdd, i)
        _ -> return $ TC (DBApiFail, i)
   (a:_) -> if (==) e . yTVideoEtag . entityVal $ a
      then
        return $ TC (DBNoop, i)
      else
        rq >>= \case
                TC (Just v) -> do
                    runDB $ update (entityKey a) [YTVideoEtag =. e, YTVideoSnippet =. Just (TC v)]
                    return $ TC (DBUpdate, i)
                _ -> return $ TC (DBApiFail, i)

updateYTPlaylist :: Key SiteGroup -> Text -> Text -> Text -> ApiReq (Maybe YoutubePlaylist) -> ApiReq (DBAction,Text)
updateYTPlaylist gr gu i e rq =
  runDB ( E.select $
     E.from   $ \yv -> do
     E.where_
        (yv E.^. YTPlaylistRef E.==. E.val i)
     return yv )
  >>= \case
   [] ->
      rq >>= \case
        TC (Just v) -> do
            runRawDB $(TQ.genJsonQuery [qq|
            insert into y_t_playlist
              ( ref         -- Text  -- < i
              , etag        -- Text  -- < e
              , snippet     -- Value -- < toJSON (Just $ TC v)
              , google_user -- Text  -- < gu
              , group_id    -- Int64 -- < P.fromSqlKey gr
              ) |])
--             runDB $ insert $ YTPlaylist i e (Just $ TC v) gu gr
            return $ TC (DBAdd, i)
        _ -> return $ TC (DBApiFail, i)
   (a:_) -> if (==) e . yTPlaylistEtag . entityVal $ a
      then
        return $ TC (DBNoop, i)
      else
        rq >>= \case
                TC (Just v) -> do
                    runDB $ update (entityKey a) [YTPlaylistEtag =. e, YTPlaylistSnippet =. Just (TC v)]
                    return $ TC (DBUpdate, i)
                _ -> return $ TC (DBApiFail, i)

{--
-- un classified
select *
from y_t_video as v
 where id not in (select video from y_t_video_playlist as pl where pl.group_id = v.group_id)
 and v.group_id = 2
 and (v.snippet->'status'->>'uploadStatus') = 'processed'


-}
updateYTPlaylistItms :: Key SiteGroup -> Text -> Text -> Text -> ApiReq [YoutubePlaylistItem] -> ApiReq [(DBAction,Text)]
updateYTPlaylistItms gr gu i _ rq =
  rq >>= \case
   (TC lis) -> do
     TC <$> forM lis ( \li@YoutubePlaylistItem{..} -> do
       runRawDB $(TQ.genTypedQuery [qq|
          select v.id, pl.id
           from y_t_video as v
              , y_t_playlist as pl
          where v.ref  = ? -- < (toJSON _ypiSnippet) ^? key "resourceId" . key "videoId" . _String ^. non "fail"
            and pl.ref = ? -- < i
                |])
       >>= \case
           []              -> return (DBNoop, _ypiId)
           ((vid, plid):_) ->
               runDB ( E.select $
                    E.from   $ \yv -> do
                    E.where_
                       (yv E.^. YTVideoPlaylistRef E.==. E.val _ypiId)
                    return yv )
                 >>= \case
                   [] -> do
                        runRawDB $(TQ.genJsonQuery [qq|
                             insert into y_t_video_playlist
                               ( ref         -- Text    -- < _ypiId
                               , etag        -- Text    -- < _ypiEtag
                               , snippet     -- Value   -- < toJSON (Just $ TC li)
                               , video       -- Integer -- < vid
                               , playlist    -- Integer -- < plid
                               , google_user -- Text    -- < gu
                               , group_id    -- Int64 -- < P.fromSqlKey gr
                               ) |])
                        return $ (DBAdd, _ypiId)

                   (a:_) -> if (==) _ypiEtag . yTVideoPlaylistEtag . entityVal $ a
                             then
                               return $ (DBNoop, _ypiId)
                             else do
                               runDB $ update (entityKey a) [YTVideoPlaylistEtag =. _ypiEtag, YTVideoPlaylistSnippet =. Just (TC li)]
                               return $ (DBUpdate, _ypiId)



      )
   _ -> return $ TC [(DBApiFail, i)]

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

getSiteGroupUser1R :: GUUID -> GUUID -> ApiReq SiteGroupMember
getSiteGroupUser1R gid e = do
  (groupKey, userKey) <- runDB $ (,) <$> getDBKey (UniqueSiteGroup gid)
                                     <*> getDBKey (UniqueUsers e)
  jsonDB1 . getBy404 $ UniqueSiteGroupMember groupKey userKey

deleteSiteGroupUser1R :: GUUID -> GUUID -> ApiReq SiteGroupMember
deleteSiteGroupUser1R gid e = do
  (groupKey, userKey) <- runDB $ (,) <$> getDBKey (UniqueSiteGroup gid)
                                     <*> getDBKey (UniqueUsers e)
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

-- getAllPlaylistEvent   :: ApiReq [ PlaylistEvent ]
-- getAllPlaylistEvent   = listsOfAll

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

