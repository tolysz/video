{-# Language DataKinds
           , OverloadedStrings
           , RecordWildCards
           , DeriveDataTypeable
           , GeneralizedNewtypeDeriving
           , DeriveGeneric
           , TemplateHaskell
           #-}

module Google.Api.Youtube.Playlists where

 -- https://developers.google.com/youtube/v3/docs/#Playlists

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
import Google.Api.Utils    (optsL)
import Google.Api.Kinds    (AsStr, ListResponse, ApiKind)
import Google.Api.Youtube.Common

type YoutubePlaylists = ListResponse YoutubePlaylist "youtube#playlistListResponse"

data YPSnippet = YPSnippet
  { _ypsnPublishedAt  :: UTCTime
  , _ypsnChannelId    :: Text
  , _ypsnTitle        :: Text
  , _ypsnDescription  :: Text
  , _ypsnThumbnails   :: HashMap Text YThumbnail
  , _ypsnTags         :: Possible [Text]
  , _ypsnChannelTitle   :: Text
  } deriving  (Show, Typeable, Generic)

data YoutubePlaylist = YoutubePlaylist
  { _ypKind           :: ApiKind "youtube#playlist"     -- present
  , _ypEtag           :: Text                           -- present
  , _ypId             :: Text
  , _ypSnippet        :: Possible YPSnippet             -- present
  , _ypStatus         :: Possible YPStatus
  , _ypContentDetails :: Possible YPContentDetails
  , _ypPlayer         :: Possible YPPlayer
  } deriving  (Show, Typeable, Generic)

type YPStatus         = Value
type YPContentDetails = Value
type YPPlayer         = Value

deriveJSON (optsL 5) ''YPSnippet
makeLenses           ''YPSnippet

deriveJSON (optsL 3) ''YoutubePlaylist
makeLenses           ''YoutubePlaylist
