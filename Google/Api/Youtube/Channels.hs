{-# Language DataKinds
           , OverloadedStrings
           , RecordWildCards
           , DeriveDataTypeable
           , GeneralizedNewtypeDeriving
           , DeriveGeneric
           , TemplateHaskell
           #-}
module Google.Api.Youtube.Channels (YoutubeChannel (..)) where

 -- https://developers.google.com/youtube/v3/docs/#Channels


import Data.Aeson
import Prelude     hiding (id)
import Data.Aeson.Types
import Data.Text          (Text)

import Data.Possible
import Data.Default
import Control.Lens       (makeLenses) -- , set)
import Data.Typeable
import GHC.Generics
import Google.Api.Utils
import Google.Api.Kinds


data YoutubeChannel = YoutubeChannel
 { _youtubeChannelKind :: ApiKind "youtube#channel"
 } deriving  (Show, Typeable, Generic)

opts = defaultOptions { fieldLabelModifier = fromCamel 15 }

instance FromJSON YoutubeChannel where parseJSON = genericParseJSON opts
instance ToJSON   YoutubeChannel where toJSON    = genericToJSON    opts

instance Default YoutubeChannel where
  def = YoutubeChannel ApiKind -- MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData

makeLenses ''YoutubeChannel


{--
{
  "kind": "youtube#channel",
  "etag": etag,
  "id": string,
  "snippet": {
    "title": string,
    "description": string,
    "publishedAt": datetime,
    "thumbnails": {
      (key): {
        "url": string,
        "width": unsigned integer,
        "height": unsigned integer
      }
    }
  },
  "contentDetails": {
    "relatedPlaylists": {
      "likes": string,
      "favorites": string,
      "uploads": string,
      "watchHistory": string,
      "watchLater": string
    },
    "googlePlusUserId": string
  },
  "statistics": {
    "viewCount": unsigned long,
    "commentCount": unsigned long,
    "subscriberCount": unsigned long,
    "hiddenSubscriberCount": boolean,
    "videoCount": unsigned long
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