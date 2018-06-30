-- language="PostgreSQL" Pattern=/Query\ \[qq\|(.*)\|\])/
module Handler.DBRaw where

import           Import
import           Permissions
import           Types
import           Data.String.QM

import qualified Database.PostgreSQL.Simple              as TQ
import qualified Database.PostgreSQL.Simple.TypedQuery   as TQ
import           Network.Google.Api.Youtube.Videos
import           Network.Google.Api.Youtube.Playlists
import qualified Database.Persist.Sql                    as P


getUserMeR :: ApiReq [Value]
getUserMeR = do
   uid <- P.fromSqlKey <$> requireAuthId
   TC <$> runRawDB $(TQ.genJsonQuery [qq|
    select name     as name     -- Maybe  Text
         , friendly as friendly -- Maybe  Text
         , avatar   as avatar   -- Maybe  Text
         , emails   as emails   -- Maybe [Text]
         , groups   as groups   -- Maybe [Text]
    from users
    left outer join (select email.user_id as id
          , array_agg(email.email) as emails
          from email
          group by email.user_id
          ) as em on users.id = em.id
    left outer join (select sgm.user_id as id
          , array_agg(sgi.uuid) as groups
          from site_group_member as sgm
     left join site_group as sgi on sgm.group_id = sgi.id
          group by sgm.user_id
          ) as sg on users.id = sg.id
    where
     users.id = ? -- < uid
   |])


-- | return all videos for a given user.
-- | logged user i.e. mis elf
getUserMeVideo0R :: ApiReq [Value]
getUserMeVideo0R =
    sqlGetUserVideo =<< P.fromSqlKey <$> requireAuthId

userUUIDtoId :: GUUID -> Handler Int
userUUIDtoId u = runRawDB $(TQ.genTypedQuery [qq|
     select id        -- Int
       from users
      where uuid = ? -- < u
  |]) >>= \case
    [x]  -> return x
    _ -> notFound

getUserVideo1R :: GUUID -> ApiReq [Value]
getUserVideo1R uuid = do
   guardAllAdmin
   userUUIDtoId uuid >>= sqlGetUserVideo


sqlGetUserVideo uid =
    TC <$> runRawDB $(TQ.genJsonQuery [qq|
      select v.ref                            as id          -- Text
           , v.snippet->'snippet'->>'title'   as title       -- Text
           , p.snippet->'snippet'->>'title'   as playlist    -- Text
           , v.uuid                           as uuid        -- Text
           , v.snippet->'snippet'->'thumbnails' as thumbnails  -- Maybe Value
           , vu.event_perm                    as event_perms -- Text
           , vu.view_perm                     as view_perms  -- Text
           , vu.tag                           as tag         -- VideoTag
        from y_t_video_user as vu
   left join y_t_video      as v on vu.video = v.id
   left join y_t_video_playlist as vp on vu.video = vp.video
   left join y_t_playlist as p on vp.playlist = p.id
       where vu.user_id = ? -- < uid
   order by playlist desc
    |])

-- video-user
-- users for each video

-- user-video
-- videos per each user

{--
  input list of IL_IDs

  DB_IDs set inside the DB

  remove := DB_IDs \ IL_IDs
  insert := IL_IDs \ DB_IDs
  update := DB_IDs \ (insert u remove)
-}

getUserPlaylistsGroupR :: GUUID -> ApiReq [Value]
getUserPlaylistsGroupR gr = do
    uid <- P.fromSqlKey <$> requireAuthId
    TC <$> runRawDB $(TQ.genJsonQuery [qq|
   select id         as id  -- Text
        , uuid       as uuid -- Text
        , thumbnails as thumbnails  -- Maybe Value
        , title      as title -- Maybe Text
        , count      as count -- Int
     from getUserPlaylistsGroupR
          ( ? -- < gr
          , ? -- < uid
          )
  |])

getUserPlaylistsGroupItemsR :: GUUID -> GUUID -> ApiReq [Value]
getUserPlaylistsGroupItemsR _ pli = do
    uid <- P.fromSqlKey <$> requireAuthId
    TC . map toJSON <$> runRawDB $(TQ.genTypedQuery [qq|
         select snippet  -- Value
              , uuid     -- Maybe Text
              , pos      -- Maybe Int
         from getUserPlaylistsItemsR
              ( ? -- Text -- < pli
              , ?         -- < uid
              )
    |])

{-

    TC <$> runRawDB $(TQ.genJsonQuery [qq|
     select vp.ref                                     as id         -- Text
          , vp.uuid                                    as uuid       -- Text
          , vp.snippet->'snippet'->'thumbnails'        as thumbnails -- Maybe Value
          , vp.snippet->'snippet'->>'title'            as title      -- Text
          , (vp.snippet->'contentDetails'->>'itemCount') :: integer as count -- Int
          , vp.snippet                                 as full_dump  -- Value
     from y_t_video_playlist as vp
left join site_group as sg on sg.id = vp.group_id
left join y_t_playlist as pl on pl.id = playlist
    where sg.uuid = ? -- < gr
      and pl.uuid = ? -- < pli
  |])

-}





getTest0R :: ApiReq [Int]
getTest0R = TC <$> runRawDB $(TQ.genTypedQuery "select 1+1")

getTest1R :: ApiReq [Text]
getTest1R = TC <$> runRawDB $(TQ.genTypedQuery "select '1' :: TEXT")

getTest2R :: ApiReq [Value]
getTest2R = TC <$> runRawDB $(TQ.genJsonQuery "select 1 as one -- Int")

getTest3R :: ApiReq [Value]
getTest3R = TC <$> runRawDB $(TQ.genJsonQuery "select '1' :: TEXT as one -- Text")

getTest4R :: ApiReq [Value]
getTest4R =
   TC <$> runRawDB (\c -> $(TQ.genJsonQuery [qq|
  select
    json_extract_path(snippet,'snippet') as snippet -- Maybe (TC YVSnippet)
  from y_t_video
  where
   ref = ? -- Text
  |]) c "jr3Sm-cnl7w")

-- getTest4R :: ApiReq [Value]
-- getTest4R = undefined
getTest5R :: ApiReq [Value]
getTest5R = TC <$> runRawDB (\c -> $(TQ.genJsonQuery [qq|
              select 'bla bla:' || (snippet->'snippet'->'thumbnails'->'maxres'->>'url')  -- Maybe Text
                   , rand        -- Double
                   , |/ 4        -- Double
                   , |/ rand     -- Double
                   , two * two   -- Double
                   , two - two   -- Double
                   , two / two   -- Double
                   , two >> two  -- Double
                   , two << two  -- Double
                   , two % two   -- Double
                   , two # two   -- Double
                   , two ^ three -- Double
                   , two * three -- Double
                   , two & three -- Double
                   , two | three -- Double
              from y_t_video
                 ,( SELECT random() as rand, 2 as two, 3 as three ) as c
              where
               ref =  ? -- Text
              |]) c "5IY1Vr42cfA")

getTest6R :: ApiReq [Value]
getTest6R = undefined
getTest7R :: ApiReq [Value]
getTest7R = undefined
getTest8R :: ApiReq [Value]
getTest8R = undefined
getTest9R :: ApiReq [Value]
getTest9R = undefined
getTest10R :: ApiReq [Value]
getTest10R = undefined
getTest11R :: ApiReq [Value]
getTest11R = undefined
getTest12R :: ApiReq [Value]
getTest12R = undefined
getTest13R :: ApiReq [Value]
getTest13R = undefined
getTest14R :: ApiReq [Value]
getTest14R = undefined
getTest15R :: ApiReq [Value]
getTest15R = undefined
getTest16R :: ApiReq [Value]
getTest16R = undefined
getTest17R :: ApiReq [Value]
getTest17R = undefined
