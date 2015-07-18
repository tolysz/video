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
    select name     as name     -- Text
         , friendly as friendly -- Text
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

