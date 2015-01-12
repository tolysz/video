{-# Language DataKinds
           , OverloadedStrings
           , RecordWildCards
           , DeriveDataTypeable
           , GeneralizedNewtypeDeriving
           , DeriveGeneric
           , TemplateHaskell
           #-}
module Google.Api.Youtube.Channels 
 -- (YoutubeChannel (..), YoutubeChannels)
 -- go for everything as TH does not export itself
 where

 -- https://developers.google.com/youtube/v3/docs/#Channels

import Prelude             (Bool, Integer, Int, Show(..))
import Data.Aeson          (Value)
import Data.Aeson.TH       (deriveJSON)
import Data.Text           (Text)
import Data.Possible       (Possible)
import Data.Typeable       (Typeable)
import Data.Time.Clock     (UTCTime(..))
import Data.HashMap.Strict (HashMap)
import Control.Lens        (makeLenses)
import GHC.Generics        (Generic)
import Google.Api.Utils    (optsL3, optsL4, optsL5)
import Google.Api.Kinds    (AsStr, ListResponse, ApiKind)
import Google.Api.Youtube.Common

data YCContentDetails = YCContentDetails
  { _yccdGooglePlusUserId :: Text
  , _yccdRelatedPlaylists :: HashMap Text PlaylistId
                                    -- \_> likes,favorites,uploads,watchHistory,watchLater -> playlistId
  } deriving  (Show, Typeable, Generic)

data YCSnippet = YCSnippet
  { _ycsnPublishedAt  :: UTCTime
  , _ycsnChannelId    :: Text
  , _ycsnTitle        :: Text
  , _ycsnDescription  :: Text
  } deriving  (Show, Typeable, Generic)

data YCStatistics = YCStatistics
  { _ycstViewCount             :: AsStr Integer
  , _ycstCommentCount          :: AsStr Integer
  , _ycstSubscriberCount       :: AsStr Integer
  , _ycstHiddenSubscriberCount :: Bool
  , _ycstVideoCount            :: AsStr Integer
  } deriving  (Show, Typeable, Generic)

data YCTopicDetails = YCTopicDetails
  { _yctdTopicIds :: [ Text ]
  } deriving  (Show, Typeable, Generic)

data YCStatus = YCStatus
  { _ycsPrivacyStatus     :: Text
  , _ycsIsLinked          :: Bool
  , _ycsLongUploadsStatus :: Text
  } deriving  (Show, Typeable, Generic)

data YCAuditDetails = YCAuditDetails
  { _ycaOverallGoodStanding             :: Bool
  , _ycaCommunityGuidelinesGoodStanding :: Bool
  , _ycaCopyrightStrikesGoodStanding    :: Bool
  , _ycaContentIdClaimsGoodStanding     :: Bool
  } deriving  (Show, Typeable, Generic)

data YCContentOwnerDetails = YCContentOwnerDetails
  { _ycodContentOwner :: Text
  , _ycodTimeLinked   :: UTCTime
  } deriving  (Show, Typeable, Generic)

type YoutubeChannels = ListResponse YoutubeChannel "youtube#channelListResponse"

data YoutubeChannel = YoutubeChannel
  { _ycKind                :: ApiKind "youtube#channel"     -- present
  , _ycEtag                :: Text                          -- present
  , _ycId                  :: Text                          -- present
  , _ycSnippet             :: Possible YCSnippet            -- present
  , _ycContentDetails      :: Possible YCContentDetails     -- present
  , _ycStatistics          :: Possible YCStatistics         -- present
  , _ycTopicDetails        :: Possible YCTopicDetails       -- missing
  , _ycStatus              :: Possible YCStatus             -- present
  , _ycContentOwnerDetails :: Possible YCContentOwnerDetails -- missing did not see a response
  , _ycBrandingSettings    :: Possible YCBrandingSettings   -- todo: implement
  , _ycInvideoPromotion    :: Possible YCInvideoPromotion   -- todo: implement
  } deriving  (Show, Typeable, Generic)


type PlaylistId         = Text
type YCBrandingSettings = Value
type YCInvideoPromotion = Value

--instance Default YoutubeChannel where
--  def = YoutubeChannel ApiKind "" ""  MissingData MissingData MissingData MissingData MissingData MissingData MissingData -- MissingData MissingData MissingData

{--
{

  "brandingSettings": {
    "channel": {
      "title": string,
      "description": string,
      "keywords": string,
      "defaultTab": string,
      "trackingAnalyticsAccountId": string,
      "moderateComments": boolean,
      "showRelatedChannels": boolean,
      "showBrowseView": boolean,
      "featuredChannelsTitle": string,
      "featuredChannelsUrls": [
        string
      ],
      "unsubscribedTrailer": string,
      "profileColor": string
    },
    "watch": {
      "textColor": string,
      "backgroundColor": string,
      "featuredPlaylistId": string
    },
    "image": {
      "bannerImageUrl": string,
      "bannerMobileImageUrl": string,
      "backgroundImageUrl": {
        "default": string,
        "localized": [
          {
            "value": string,
            "language": string
          }
        ]
      },
      "largeBrandedBannerImageImapScript": {
        "default": string,
        "localized": [
          {
            "value": string,
            "language": string
          }
        ]
      },
      "largeBrandedBannerImageUrl": {
        "default": string,
        "localized": [
          {
            "value": string,
            "language": string
          }
        ]
      },
      "smallBrandedBannerImageImapScript": {
        "default": string,
        "localized": [
          {
            "value": string,
            "language": string
          }
        ]
      },
      "smallBrandedBannerImageUrl": {
        "default": string,
        "localized": [
          {
            "value": string,
            "language": string
          }
        ]
      },
      "watchIconImageUrl": string,
      "trackingImageUrl": string,
      "bannerTabletLowImageUrl": string,
      "bannerTabletImageUrl": string,
      "bannerTabletHdImageUrl": string,
      "bannerTabletExtraHdImageUrl": string,
      "bannerMobileLowImageUrl": string,
      "bannerMobileMediumHdImageUrl": string,
      "bannerMobileHdImageUrl": string,
      "bannerMobileExtraHdImageUrl": string,
      "bannerTvImageUrl": string,
      "bannerTvLowImageUrl": string,
      "bannerTvMediumImageUrl": string,
      "bannerTvHighImageUrl": string,
      "bannerExternalUrl": string
    },
    "hints": [
      {
        "property": string,
        "value": string
      }
    ]
  },
  "invideoPromotion": {
    "defaultTiming": {
      "type": string,
      "offsetMs": unsigned long,
      "durationMs": unsigned long
    },
    "position": {
      "type": string,
      "cornerPosition": string
    },
    "items": [
      {
        "id": {
          "type": string,
          "videoId": string,
          "websiteUrl": string,
          "recentlyUploadedBy": string
        },
        "timing": {
          "type": string,
          "offsetMs": unsigned long,
          "durationMs": unsigned long
        },
        "customMessage": string,
        "promotedByContentOwner": boolean
      }
    ],
    "useSmartTiming": boolean
  },
}

--}

-- Borring TH stuff
deriveJSON optsL5 ''YCSnippet
makeLenses        ''YCSnippet

deriveJSON optsL4 ''YCStatus
makeLenses        ''YCStatus

deriveJSON optsL5 ''YCTopicDetails
makeLenses        ''YCTopicDetails

deriveJSON optsL5 ''YCStatistics
makeLenses        ''YCStatistics

deriveJSON optsL5 ''YCContentDetails
makeLenses        ''YCContentDetails

deriveJSON optsL4 ''YCAuditDetails
makeLenses        ''YCAuditDetails

deriveJSON optsL5 ''YCContentOwnerDetails
makeLenses        ''YCContentOwnerDetails

deriveJSON optsL3 ''YoutubeChannel
makeLenses        ''YoutubeChannel
