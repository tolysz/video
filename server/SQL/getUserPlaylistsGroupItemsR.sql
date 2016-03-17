CREATE OR REPLACE
FUNCTION getUserPlaylistsItemsR
  ( pli VARCHAR(32)
  , uid BIGINT
  )
  RETURNS TABLE
  ( snippet JSON
  , uuid VARCHAR
  , pos INTEGER
  )
AS $$
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
$$ LANGUAGE plpgsql;

select * from
  getUserPlaylistsItemsR('3a300a91-4c50-4be5-bfe2-f653c9341b2a',3)

