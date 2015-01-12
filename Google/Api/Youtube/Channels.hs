{-# Language DataKinds
           , OverloadedStrings
           , RecordWildCards
           , DeriveDataTypeable
           , GeneralizedNewtypeDeriving
           , DeriveGeneric
           , TemplateHaskell
           #-}
module Google.Api.Youtube.Channels (YoutubeChannel (..), YoutubeChannels) where

 -- https://developers.google.com/youtube/v3/docs/#Channels

import Data.Aeson
import Prelude     hiding (id)
import Data.Aeson.Types
import Data.Aeson.TH
import Data.Text          (Text)

import Data.Possible
import Data.Default
import Control.Lens       (makeLenses) -- , set)
import Data.Typeable
import GHC.Generics
import Google.Api.Utils
import Google.Api.Kinds
import Data.Time.Clock (UTCTime (..))

import Data.HashMap.Strict (HashMap(..))

data YCSnippet = YCSnippet
 { _ycsnTitle       :: Text
 , _ycsnDescription :: Text
 , _ycsnPublishedAt :: UTCTime
--  , _ycsnthumbnails  :: YCSNThumb
 } deriving  (Show, Typeable, Generic)
deriveJSON optsL5 ''YCSnippet
makeLenses        ''YCSnippet

data YCContentDetails = YCContentDetails
 { _yccdGooglePlusUserId :: Text
 , _yccdRelatedPlaylists :: HashMap Text Text
   -- likes,favorites,uploads,watchHistory,watchLater -> playlistId
 } deriving  (Show, Typeable, Generic)
deriveJSON optsL5 ''YCContentDetails
makeLenses        ''YCContentDetails

data YTStatistic = YTStatistics
  { _ycstViewCount             :: Integer
  , _ycstCommentCount          :: Integer
  , _ycstSubscriberCount       :: Integer
  , _ycstHiddenSubscriberCount :: Bool
  , _ycstVideoCount            :: Integer
  } deriving  (Show, Typeable, Generic)
deriveJSON optsL5 ''YTStatistic
makeLenses        ''YTStatistic

type YoutubeChannels = ListResponse YoutubeChannel "youtube#channel"
data YoutubeChannel = YoutubeChannel
 { _ycKind :: ApiKind "youtube#channel"
 , _ycEtag :: Text
 , _ycId   :: Text
 , _ycSnippet         :: Possible YCSnippet
 , _ycContentDetails  :: Possible YCContentDetails
 , _ycStatistics      :: Possible YTStatistic
--  , _ycTopicDetails        :: Possible TopicDetails
--  , _ycStatus              :: Possible Status
--  , _ycBrandingSettings    :: Possible BrandingSettings
--  , _ycContentOwnerDetails :: Possible ContentOwnerDetails
--  , _ycInvideoPromotion    :: Possible InvideoPromotion
--  , _ycAuditDetails        :: Possible AuditDetails
 } deriving  (Show, Typeable, Generic)
deriveJSON optsL3 ''YoutubeChannel
makeLenses        ''YoutubeChannel
instance Default YoutubeChannel where
  def = YoutubeChannel ApiKind "" ""  MissingData MissingData MissingData -- MissingData MissingData MissingData MissingData MissingData MissingData MissingData


{--
{
  "snippet": {
    "thumbnails": {
      (key): {
        "url": string,
        "width": unsigned integer,
        "height": unsigned integer
      }
    }
  },
  "topicDetails": {
    "topicIds": [
      string
    ]
  },
  "status": {
    "privacyStatus": string,
    "isLinked": boolean,
    "longUploadsStatus": string
  },
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
  "auditDetails": {
    "overallGoodStanding": boolean,
    "communityGuidelinesGoodStanding": boolean,
    "copyrightStrikesGoodStanding": boolean,
    "contentIdClaimsGoodStanding": boolean
  },
  "contentOwnerDetails": {
    "contentOwner": string,
    "timeLinked": datetime
  }
}

--}