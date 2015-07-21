module Handler.DBRaw where

import Import
import Types

import Data.String.QM

import qualified Database.PostgreSQL.Simple     as TQ
import qualified Database.PostgreSQL.Simple.TypedQuery   as TQ
import Network.Google.Api.Youtube.Videos
import Network.Google.Api.Youtube.Playlists
import qualified Database.Persist.Sql as P


getUserMeR :: ApiReq [Value]
getUserMeR = do
   uid <- P.fromSqlKey <$> requireAuthId
   TC <$> runRawDB $(TQ.genJsonQuery [qq|
    select name     as name     -- Maybe  Text
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
     users.id = ?  -- < uid
   |])

getUserPlaylistsGroupR :: GUUID -> ApiReq [Value]
getUserPlaylistsGroupR gr = do
    uid <- P.fromSqlKey <$> requireAuthId
    gid <- P.fromSqlKey <$> getGroupKey gr
    TC <$> runRawDB $(TQ.genJsonQuery [qq|
    select ref                                     as id         -- Text
         , uuid                                    as uuid       -- Text
         , snippet->'snippet'->'thumbnails'        as thumbnails -- Maybe Value
         , snippet->'snippet'->>'title'            as title      -- Text
         , (snippet->'contentDetails'->>'itemCount') :: integer as count -- Int
    from y_t_playlist
    where
      group_id = ? -- < gid
  |])

getUserPlaylistsGroupItemsR :: GUUID -> GUUID -> ApiReq [Value]
getUserPlaylistsGroupItemsR gr pli = do
    uid <- P.fromSqlKey <$> requireAuthId
--     guard =<< getUserAdmin
    TC . map toJSON <$> runRawDB $(TQ.genTypedQuery [qq|
         select vp.snippet     -- Value
              , yv.ref         -- Maybe Text
         from y_t_video_playlist as vp
    left join site_group         as sg  on sg.id  = vp.group_id
    left join y_t_playlist       as pl  on pl.id  = playlist
    left join site_group_member  as sgm on sg.id  = sgm.group_id
    left join y_t_video          as yv  on yv.ref = vp.snippet->'snippet'->'resourceId'->>'videoId'
        where sg.uuid     = ? -- Text -- < gr
          and pl.uuid     = ? -- Text -- < pli
          and sgm.user_id = ?         -- < uid
          and sgm.video_admin = true
      order by (vp.snippet->'snippet'->>'position')
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

