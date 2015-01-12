module Handler.User where

import Handler.OAuth2
import Import
import Types

import Data.String.QM
import Google.Api.Types.GoogleUser
import Google.Api.Youtube.Channels

-- Module dedicated to accessing Data?

getUserChannelsR :: ApiReq [YTChannel]
getUserChannelsR =
  TC . catMaybes <$> do
        uid <- getUserIdent
        runDB $ selectList [ChannelMemberUser ==. uid] []
          >>= mapM (\(Entity _ q) -> get $ channelMemberRef q )

getGoogleUserR     :: ApiReq GoogleUser
getGoogleUserR     =  "https://www.googleapis.com/oauth2/v2/userinfo"

getListVideosR     :: ApiReq Value
getListVideosR     = "https://www.googleapis.com/youtube/v3/videos?part=snippet,contentDetails,status,statistics,recordingDetails,fileDetails&id=ACft-tpu47g"

handleYTChannelsR  :: ApiReq YoutubeChannels
--handleYTChannelsR  = "https://www.googleapis.com/youtube/v3/channels?part=snippet&mine=true"
handleYTChannelsR  = "https://www.googleapis.com/youtube/v3/channels?part=brandingSettings,contentDetails,contentOwnerDetails,id,invideoPromotion,snippet,statistics,status,topicDetails&mine=true"
-- UCSkMt4A9QMBnuFVrmPHQ1iw
handleYTPlaylistsR :: String -> String -> ApiReq Value
handleYTPlaylistsR cid part = [qm|https://www.googleapis.com/youtube/v3/playlists?part=$part&channelId=$cid&maxResults=50|]

-- get id, etag if etag does not match get ->
-- auditDetails,brandingSettings,contentDetails,contentOwnerDetails,id,invideoPromotion,snippet,statistics,status,topicDetails

handleYTAllVideosR :: ApiReq Value
handleYTAllVideosR = "https://www.googleapis.com/youtube/v3/search?part=snippet&forMine=true&type=video&maxResults=50"


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

