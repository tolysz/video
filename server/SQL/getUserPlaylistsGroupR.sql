-- drop function getuserplaylistsgroupr(character varying,bigint);
-- drop function getUserPlaylistsGroupR ( varchar, bigint);
-- drop function getUserPlaylistsGroupR ( text, bigint);

CREATE OR REPLACE
FUNCTION getUserPlaylistsGroupR
  ( gr  varchar
    , uid BIGINT
  )
  RETURNS TABLE
  ( id         varchar
  , uuid       varchar
  , thumbnails json
  , title      text
  , count      integer
  )
AS $$
DECLARE
  --   gid BIGINT;
BEGIN
  RETURN QUERY
  select pl.ref                               -- :: varchar as id         -- Text
    , pl.uuid                                 -- :: varchar as uuid       -- Text
    , snippet->'snippet'->'thumbnails'        -- as thumbnails -- Maybe Value
    , snippet->'snippet'->>'title'             as title      -- Maybe Text
    , (snippet->'contentDetails'->>'itemCount') :: integer -- as count -- Int
  from y_t_playlist as pl
    left join site_group        as sg  on sg.id       = pl.group_id
  where sg.uuid = gr -- gr
        and (
          exists(select * from site_admin where user_id = uid and is_admin = true )  -- global admin
          or exists( select *
                     from site_group_member as sgm
                     where (
                             ( pl.share_perm = 'PSGroup' and sgm.full_member = true)
                             or (sgm.video_admin = true)
                           )
                           and sgm.group_id = pl.group_id
                           and sgm.user_id = uid
          )
          or exists( select *                                                      -- user had a role
                     from y_t_video_user as ytvu
                       left join y_t_video_playlist as ytvp on ytvp.video = ytvu.video
                     where ytvp.playlist = pl.id
                           and ytvu.user_id = uid
          )
          or exists( select *                                                      -- explicitly added user at playlist level
                     from explicit_playlist_user as epu
                     where epu.playlist=pl.id
                           and epu.user=uid
          )

        )
  order by title desc;
END;
$$ LANGUAGE plpgsql;

select * from
  getUserPlaylistsGroupR('7590f9b6-9422-4218-baaa-1d29b8eafa56',3);
select * from
  getUserPlaylistsGroupR('7590f9b6-9422-4218-baaa-1d29b8eafa56',43);
select * from
  getUserPlaylistsGroupR('7590f9b6-9422-4218-baaa-1d29b8eafa56',19);
