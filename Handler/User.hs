{-# LANGUAGE  DataKinds, ViewPatterns, OverloadedStrings #-}
module Handler.User where

import Handler.OAuth2
import Import

import Data.Possible
import Data.String.QM
import Control.Lens

import Google.Api.Kinds
import Google.Api.Types.GoogleUser
import Google.Api.Youtube.Channels
import Google.Api.Youtube.Playlists
import Google.Api.Youtube.PlaylistItems
import Google.Api.Youtube.Videos
import Data.Text as T
-- import qualified Data.List as DL (intercalate)
{-
import Google.Api{Kinds, Types.GoogleUser, Youtube{Channels, Playlists, Videos}}
-}

-- Requires OAuth2
-- get information about logged user
getGoogleUserR     :: ApiReq GoogleUser
getGoogleUserR     =  "https://www.googleapis.com/oauth2/v2/userinfo"

-- get all videos by id
handleYTVideoBaseR :: Handler Html
handleYTVideoBaseR = defaultLayout [whamlet||]

getYTVideoR :: [Text] -> ApiReq [YoutubeVideo]
getYTVideoR (T.unpack . T.intercalate "," -> vid) =  TC <$> next HaveNull []
    where
      req = "snippet,contentDetails,status,statistics,recordingDetails,fileDetails"
      base = [qm|https://www.googleapis.com/youtube/v3/videos?part=$req&id=$vid|]
      next MissingData a   = return a
      next (HaveData "") a = return a
      next n a = do -- HaveNull is our start
         TC one <- fromString (base <> (possible "" "" ("&pageToken=" <>) n)) :: ApiReq YoutubeVideos
         next (fetchNext one) (a ++ (one ^. lrItems))

-- how to merge this and the nent function ?? any help welcome! type class with an internal type/data family perhaps?

-- all channels for a given user
handleYTChannelsR  :: ApiReq [YoutubeChannel]
handleYTChannelsR  = TC <$> next HaveNull []
  where
    req = "brandingSettings,contentDetails,contentOwnerDetails,id,invideoPromotion,snippet,statistics,status,topicDetails"
    base = [qm|https://www.googleapis.com/youtube/v3/channels?part=$req&mine=true|]
    next :: Possible String -> [YoutubeChannel] -> Handler [YoutubeChannel]
    next MissingData a = return a
    next n a = do
           TC one <- fromString (base <> (possible "" "" ("&nextToken=" <>) n)) :: ApiReq YoutubeChannels
           next (fetchNext one) (a ++ (one ^. lrItems))

-- all playlists for a given channel
handleYTPlaylistsBaseR :: Handler Html
handleYTPlaylistsBaseR = defaultLayout [whamlet||]

handleYTPlaylistsR :: String -> ApiReq [YoutubePlaylist]
handleYTPlaylistsR cid = TC <$> next HaveNull []
  where
    req = "id,snippet,contentDetails,player,status"
    base = [qm|https://www.googleapis.com/youtube/v3/playlists?part=$req&channelId=$cid&maxResults=50|]
    next MissingData a   = return a
    next (HaveData "") a = return a
    next n a = do
       TC one <- fromString (base <> (possible "" "" ("&pageToken=" <>) n)) :: ApiReq YoutubePlaylists
       next (fetchNext one) (a ++ (one ^. lrItems))

handleYTPlaylistItemBaseR :: Handler Html
handleYTPlaylistItemBaseR = defaultLayout [whamlet||]

handleYTPlaylistItemR :: String -> ApiReq [YoutubePlaylistItem]
handleYTPlaylistItemR pid = TC <$> next HaveNull []
  where
    req = "id,snippet,contentDetails,status"
    base = [qm|https://www.googleapis.com/youtube/v3/playlistItems?part=$req&playlistId=$pid&maxResults=50|]
    next MissingData a   = return a
    next (HaveData "") a = return a
    next n a = do
       TC one <- fromString (base <> (possible "" "" ("&pageToken=" <>) n)) :: ApiReq YoutubePlaylistItems
       next (fetchNext one) (a ++ (one ^. lrItems))

-- get id, etag if etag does not match get ->
-- auditDetails,brandingSettings,contentDetails,contentOwnerDetails,id,invideoPromotion,snippet,statistics,status,topicDetails

-- all videos for a given user
handleYTAllVideosR   :: ApiReq [Value]
handleYTAllVideosR = TC <$> next HaveNull [] -- if there be 'null' in the result add extra field init 
  where
    req = "id,snippet"
    base = "https://www.googleapis.com/youtube/v3/search?part=" <> req <> "&forMine=true&type=video&maxResults=50"
    next MissingData a   = return a
    next (HaveData "") a = return a
    next n a = do
       TC one <- fromString (base <> (possible "" "" ("&pageToken=" <>) n)) :: ApiReq (ListResponse Value "youtube#searchListResponse")
       next (fetchNext one) (a ++ (one ^. lrItems))

-- example how rto use lenses, kill soon
-- vvv ^.. ggrResults . traverse . ggrtGeometry . to ( \v -> ( v ^. gggLocation, v ^. gggLocationType  ))
-- Prelude.and $ Prelude.map isJust (Prelude.map decode dir :: [Maybe GGResponse])
-- Prelude.map decode dir :: [Maybe GGResponse]
-- dir <- (getDirectoryContents "data/geoCache/"  >>= mapM ( B.readFile . ("data/geoCache/" ++ ))  . Prelude.filter (\a -> Prelude.length a > 2 ))
-- let dd = Prelude.map (fromJust . decode) dir :: [GGResponse]

-- dd ^.. traverse .  ggrResults . traverse . ggrtGeometry . to ( \v -> ( v ^. gggLocation, v ^. gggLocationType  ))
-- dd ^.. traverse .  ggrResults . traverse . ggrtGeometry . gggLocationType . to Prelude.show ^. to Data.List.sort


{--
id


Pubblic - visible to all


snippet.categoryId

status.embeddable
status.license
status.publicStatsViewable


status.publishAt

-- all / most the same place
recordingDetails.locationDescription
recordingDetails.location.latitude
recordingDetails.location.longitude


snippet.title
snippet.description
status.privacyStatus
snippet.tags[]
recordingDetails.recordingDate


private
public
unlisted

--

PUT https://www.googleapis.com/youtube/v3/videos?part=status&fields=status&key={YOUR_API_KEY}

Content-Type:  application/json
Authorization:  Bearer ya29.8gD_7gLL14dOD0cujDiz-VP-OGPM-9Mp331oSGRQBnpcoocJEGAhOj3aBqmc8rnoeBj7ThZgVlOjew
X-JavaScript-User-Agent:  Google APIs Explorer

{
 "id": "ACft-tpu47g",
 "status": {
  "privacyStatus": "private"
 }
}


200 OK

- Hide headers -

Cache-Control:  no-cache, no-store, max-age=0, must-revalidate
Content-Encoding:  gzip
Content-Type:  application/json; charset=UTF-8
Date:  Mon, 05 Jan 2015 02:25:16 GMT
ETag:  "F9iA7pnxqNgrkOutjQAa9F2k8HY/rMMyhSGqlP7XmNPDmwtACS4DhUw"
Expires:  Fri, 01 Jan 1990 00:00:00 GMT
Pragma:  no-cache
Server:  GSE
Transfer-Encoding:  chunked
Vary:  Origin, X-Origin

{
 "status": {
  "uploadStatus": "processed",
  "privacyStatus": "private",
  "license": "youtube",
  "embeddable": true,
  "publicStatsViewable": true
 }
}
-}

