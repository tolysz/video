module Handler.User where

import Handler.OAuth2
import Import
import Types

import Google.Api.Types.GoogleUser

-- Module dedicated to accessing Data?
getUserChannelsR :: Handler (TC [YTChannel])
getUserChannelsR =
  TC . catMaybes <$> do
        uid <- getUserIdent
        runDB $ selectList [ChannelMemberUser ==. uid] []
          >>= mapM (\(Entity _ q) -> get $ channelMemberRef q )

getGoogleUserR     :: ApiReq GoogleUser
getGoogleUserR     =  "https://www.googleapis.com/oauth2/v2/userinfo"

getListVideosR     :: ApiReq Value
getListVideosR     = "https://www.googleapis.com/youtube/v3/videos?part=snippet,contentDetails,status,statistics,recordingDetails,fileDetails&id=ACft-tpu47g"

handleYTChannelsR  :: ApiReq Value
handleYTChannelsR  = "https://www.googleapis.com/youtube/v3/channels?part=snippet&mine=true"

handleYTPlaylistsR :: ApiReq Value
handleYTPlaylistsR = "https://www.googleapis.com/youtube/v3/playlists?part=snippet&channelId=UCSkMt4A9QMBnuFVrmPHQ1iw&maxResults=50"

handleYTAllVideosR :: ApiReq Value
handleYTAllVideosR = "https://www.googleapis.com/youtube/v3/search?part=snippet&forMine=true&type=video&maxResults=50"


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

