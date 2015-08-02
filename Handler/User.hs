{-# LANGUAGE  DataKinds, ViewPatterns, OverloadedStrings #-}
module Handler.User where

import Handler.OAuth2
import Import

import Data.Possible
import Data.String.QM
import Control.Lens
import Data.Aeson.Lens

import Network.Google.Api.Kinds
import Network.Google.Api.Types.GoogleUser
import Network.Google.Api.Youtube.Channels
import Network.Google.Api.Youtube.Playlists
import Network.Google.Api.Youtube.PlaylistItems
import Network.Google.Api.Youtube.Videos
import Data.Text as T

import qualified Data.ByteString.Lazy as L
import Data.Enumerator.List (consume)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Handler.DB

-- import qualified Data.List as DL (intercalate)
{-
import Google.Api{Kinds, Types.GoogleUser, Youtube{Channels, Playlists, Videos}}
-}

-- Requires OAuth2
-- get information about logged user
getGoogleUserR :: GUUID -> ApiReq GoogleUser
getGoogleUserR =  "https://www.googleapis.com/oauth2/v2/userinfo"

getUpdateVideosBaseR :: Handler Html
getUpdateVideosBaseR = defaultLayout [whamlet||]

handleUserRootR:: Handler Html
handleUserRootR = defaultLayout [whamlet||]


postWatchVideosR :: GUUID -> Handler Text
postWatchVideosR gid = do
  bss <- lift consume
  let requestBody = decodeUtf8 $ L.fromChunks bss
  $(logWarn) requestBody
  return ""


getUpdateVideosR :: GUUID -> ApiReq [(DBAction, Text)]
getUpdateVideosR gid = do
  gr      <- getGroupKey gid
  TC guid <- getGoogleUserR gid

  vds  <- processV  <$> handleYTAllVideosR    gid
  pls  <- processPL <$> handleYTAllPlaylistsR gid

  let ggg = possible (error "no user") (error "no user") id $ guid ^. googleUserId
  -- todo: unsafe
  TC u1 <- unTC <$> (forM vds (\(e,i) -> updateYTVideo         gr ggg i e (fmap listToMaybe <$> getYTVideoR    gid [i])))
  TC u2 <- unTC <$> (forM pls (\(e,i) -> updateYTPlaylist      gr ggg i e (fmap listToMaybe <$> getYTPlaylistR gid [i])))
  TC u3 <- unTC <$> (forM pls (\(e,i) -> updateYTPlaylistItms  gr ggg i e (handleYTPlaylistItemR gid (T.unpack i))))
----

---
  return $ TC (u1 ++ u2 ++ Import.concat u3)

   where
     unTC a = TC (Import.map (\(TC a)-> a) a)
     processV  (TC a) = catMaybes (Import.map extr a)
     processPL (TC a) = catMaybes (Import.map extrPL a)
     extr :: Value -> Maybe (Text,Text)
     extr a = do
         ma <- a ^? key "etag"               . _String
         mb <- a ^? key "id" . key "videoId" . _String
         return (ma,mb)
     extrPL :: Value -> Maybe (Text,Text)
     extrPL a = do
         ma <- a ^? key "etag" . _String
         mb <- a ^? key "id" . _String
--          mb <- a ^? key "id" . key "playlistId" . _String
         return (ma,mb)

-- get all videos by id
handleYTVideoBaseR :: Handler Html
handleYTVideoBaseR = defaultLayout [whamlet||]

getYTVideoR :: GUUID -> [Text] -> ApiReq [YoutubeVideo]
getYTVideoR gid (T.unpack . T.intercalate "," -> vid) =  TC <$> next HaveNull []
    where
      req = "snippet,contentDetails,status,statistics,recordingDetails,fileDetails"
      base = [qm|https://www.googleapis.com/youtube/v3/videos?part=$req&id=$vid|]
      next MissingData a   = return a
      next (HaveData "") a = return a
      next n a = do -- HaveNull is our start
         TC one <- (fromString (base <> (possible "" "" ("&pageToken=" <>) n)) :: GUUID -> ApiReq YoutubeVideos) gid
         next (fetchNext one) (a ++ (one ^. lrItems))

getYTPlaylistR :: GUUID -> [Text] -> ApiReq [YoutubePlaylist]
getYTPlaylistR gid (T.unpack . T.intercalate "," -> vid) =  TC <$> next HaveNull []
    where
      req = "snippet,contentDetails,status,id,player"
      base = [qm|https://www.googleapis.com/youtube/v3/playlists?part=$req&id=$vid|]
      next MissingData a   = return a
      next (HaveData "") a = return a
      next n a = do -- HaveNull is our start
         TC one <- (fromString (base <> (possible "" "" ("&pageToken=" <>) n)) :: GUUID -> ApiReq YoutubePlaylists) gid
         next (fetchNext one) (a ++ (one ^. lrItems))


-- how to merge this and the nent function ?? any help welcome! type class with an internal type/data family perhaps?

-- all channels for a given user
handleYTChannelsBaseR :: Handler Html
handleYTChannelsBaseR = defaultLayout [whamlet||]

handleYTChannelsR  :: GUUID -> ApiReq [YoutubeChannel]
handleYTChannelsR  gid = TC <$> next HaveNull []
  where
    req = "brandingSettings,contentDetails,contentOwnerDetails,id,invideoPromotion,snippet,statistics,status,topicDetails"
    base = [qm|https://www.googleapis.com/youtube/v3/channels?part=$req&mine=true|]
    next :: Possible String -> [YoutubeChannel] -> Handler [YoutubeChannel]
    next MissingData a = return a
    next n a = do
           TC one <- (fromString (base <> (possible "" "" ("&nextToken=" <>) n)) :: Text -> ApiReq YoutubeChannels) gid
           next (fetchNext one) (a ++ (one ^. lrItems))

-- all playlists for a given channel
handleYTPlaylistsBaseR :: Handler Html
handleYTPlaylistsBaseR = defaultLayout [whamlet||]

handleYTPlaylistsR :: GUUID -> String -> ApiReq [YoutubePlaylist]
handleYTPlaylistsR gid cid = TC <$> next HaveNull []
  where
    req = "id,snippet,contentDetails,player,status"
    base = [qm|https://www.googleapis.com/youtube/v3/playlists?part=$req&channelId=$cid&maxResults=50|]
    next MissingData a   = return a
    next (HaveData "") a = return a
    next n a = do
       TC one <- (fromString (base <> (possible "" "" ("&pageToken=" <>) n)) :: Text -> ApiReq YoutubePlaylists) gid
       next (fetchNext one) (a ++ (one ^. lrItems))

handleYTPlaylistItemBaseR :: Handler Html
handleYTPlaylistItemBaseR = defaultLayout [whamlet||]

handleYTPlaylistItemR :: GUUID -> String -> ApiReq [YoutubePlaylistItem]
handleYTPlaylistItemR gid pid = TC <$> next HaveNull []
  where
    req = "id,snippet,contentDetails,status"
    base = [qm|https://www.googleapis.com/youtube/v3/playlistItems?part=$req&playlistId=$pid&maxResults=50|]
    next MissingData a   = return a
    next (HaveData "") a = return a
    next n a = do
       TC one <- (fromString (base <> (possible "" "" ("&pageToken=" <>) n)) :: Text -> ApiReq YoutubePlaylistItems) gid
       next (fetchNext one) (a ++ (one ^. lrItems))

-- get id, etag if etag does not match get ->
-- auditDetails,brandingSettings,contentDetails,contentOwnerDetails,id,invideoPromotion,snippet,statistics,status,topicDetails

-- all videos for a given user
handleYTAllVideosR   :: GUUID -> ApiReq [Value]
handleYTAllVideosR gid = TC <$> next HaveNull [] -- if there be 'null' in the result add extra field init
  where
    req = "id,snippet"
--     req = "id"
    base = "https://www.googleapis.com/youtube/v3/search?part=" <> req <> "&forMine=true&type=video&maxResults=50"
    next MissingData a   = return a
    next (HaveData "") a = return a
    next n a = do
       TC one <- (fromString (base <> (possible "" "" ("&pageToken=" <>) n)) :: Text -> ApiReq (ListResponse Value "youtube#searchListResponse")) gid
       next (fetchNext one) (a ++ (one ^. lrItems))


handleYTAllPlaylistsR :: GUUID -> ApiReq [Value]
handleYTAllPlaylistsR gid = TC <$> next HaveNull []
  where
    req = "id,snippet"
    base = [qm|https://www.googleapis.com/youtube/v3/playlists?part=$req&mine=true&maxResults=50|]
    next MissingData a   = return a
    next (HaveData "") a = return a
    next n a = do
       TC one <- (fromString (base <> (possible "" "" ("&pageToken=" <>) n)) :: Text -> ApiReq (ListResponse Value "youtube#playlistListResponse")) gid
       next (fetchNext one) (a ++ (one ^. lrItems))

-- example how rto use lenses, kill soon
-- vvv ^.. ggrResults . traverse . ggrtGeometry . to ( \v -> ( v ^. gggLocation, yesod lov ^. gggLocationType  ))
-- Prelude.and $ Prelude.map isJust (Prelude.map decode dir :: [Maybe GGResponse])
-- Prelude.map decode dir :: [Maybe GGResponse]
-- dir <- (getDirectoryContents "data/geoCache/"  >>= mapM ( B.readFile . ("data/geoCache/" ++ ))  . Prelude.filter (\a -> Prelude.length a > 2 ))
-- let dd = Prelude.map (fromJust . decode) dir :: [GGResponse]

-- dd ^.. traverse .  ggrResults . traverse . ggrtGeometry . to ( \v -> ( v ^. gggLocation, v ^. gggLocationType  ))
-- dd ^.. traverse .  ggrResults . traverse . ggrtGeometry . gggLocationType . to Prelude.show ^. to Data.List.sort

{--
 Events

 Users
   -- user all videos
--}
{--
add Tags
PUT https://www.googleapis.com/youtube/v3/playlists?part=snippet&key={YOUR_API_KEY}

{
 "snippet": {
  "title": "Tim Minchin",
  "tags": [
   "RAW",
   "SEXY"
  ]
 },
 "id": "PLD1E5DFF19FF9C747"
}
--}

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

