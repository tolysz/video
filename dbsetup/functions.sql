
create function getuserplaylistsitemsr(pli character varying, uid bigint) returns TABLE(snippet json, uuid character varying, pos integer)
  language plpgsql
as
$$
DECLARE
BEGIN
  RETURN QUERY select vp.snippet                                        -- Value
                 , yv.uuid                                              -- Maybe Text
                 , (vp.snippet->'snippet'->>'position')::integer as pos -- Maybe Int
               from y_t_video_playlist as vp
                 left join site_group         as sg  on sg.id = vp.group_id
                 left join y_t_playlist       as pl  on pl.id = vp.playlist
                 left join y_t_video          as yv  on yv.id = vp.video
                 left join site_group_member  as sgm on sg.id = sgm.group_id
               where pl.uuid     = pli -- Text -- < pli
                 -- and sg.uuid     = gr -- Text -- < gr
                 and sgm.user_id = uid        -- < uid
                 and sgm.video_admin = true
               order by pos;
END;
$$;


create function getuserplaylistsgroupr(gr character varying, uid bigint) returns TABLE(id character varying, uuid character varying, thumbnails json, title text, count integer)
  language plpgsql
as
$$
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
$$;


create function getuserplaylistsgroupitemsr(gr character varying, pli character varying, uid bigint) returns TABLE(snippet json, uuid character varying, pos integer)
  language plpgsql
as
$$
DECLARE
BEGIN
   RETURN QUERY select vp.snippet                                           -- Value
              , yv.uuid                                              -- Maybe Text
              , (vp.snippet->'snippet'->>'position')::integer as pos -- Maybe Int
         from y_t_video_playlist as vp
    left join site_group         as sg  on sg.id = vp.group_id
    left join y_t_playlist       as pl  on pl.id = vp.playlist
    left join y_t_video          as yv  on yv.id = vp.video
    left join site_group_member  as sgm on sg.id = sgm.group_id
        where pl.uuid     = pli -- Text -- < pli
          and sg.uuid     = gr -- Text -- < gr
          and sgm.user_id = uid         -- < uid
          and sgm.video_admin = true
      order by pos;
 END;
$$;
