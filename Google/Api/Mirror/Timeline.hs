{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Google.Api.Mirror.Timeline where

import           Data.Aeson -- .TH                 (defaultOptions, deriveJSON)
import           Data.Aeson.Types
import           Data.Text                     (Text)
import           Prelude                       hiding (id)
--import Data.Maybe
import Data.Possible
--import qualified Prelude                       as P (id)
import Google.Api.Kinds
import Google.Api.Transport
import Data.Default
import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar( Day( ModifiedJulianDay ) )
import Control.Applicative

import Google.Api.Mirror.Contact
import Google.Api.Mirror.Location


data Att = EB Attachment
         | UP UploadAtt
         | HR RemoteAtt
           deriving Show
data UploadAtt = UploadAtt
             { uaData :: Text
             , uaMime :: Text
             , uaName :: String
             } deriving Show

data RemoteAtt = RemoteAtt
             { raUrl   :: String
             , raEmbed :: Bool
             } deriving Show

data Attachment = Attachment
             { attachmentId                  :: Possible Text
             , attachmentContentType         :: Possible Text
             , attachmentContentUrl          :: Possible Text
             , attachmentIsProcessingContent :: Possible Bool
             } deriving (Show)

{-- Attachment
  "id": string,
  "contentType": string,
  "contentUrl": string,
  "isProcessingContent": boolean
--}

instance Default Attachment where
  def = Attachment MissingData MissingData MissingData MissingData

instance ApiKind Attachment where -- not used
  apiKind _ = "mirror#attachment"

instance ToJSON Attachment where
  toJSON a@Attachment{..} = object
     [ "id"                  .= toJSON attachmentId
     , "contentType"         .= toJSON attachmentContentType
     , "contentUrl"          .= toJSON attachmentContentUrl
     , "isProcessingContent" .= toJSON attachmentIsProcessingContent
     ]

instance FromJSON Attachment where
   parseJSON (Object v) =
         Attachment <$>  v .:?? "id"
                    <*>  v .:?? "contentType"
                    <*>  v .:?? "contentUrl"
                    <*>  v .:?? "isProcessingContent"
   parseJSON _ = fail "Not a Setting object"


newtype Attachments = Attachments [Attachment] deriving Show
instance Default Attachments where
  def = Attachments []

instance ApiKind Attachments where
  apiKind _ = "mirror#attachmentsList"

instance ToJSON Attachments where
  toJSON a@(Attachments l) = object
     [ "items"       .= toJSON l
     , "kind"        .= apiKind a
     ]
instance FromJSON Attachments where
   parseJSON (Object v) = do
       kd <- v .: "kind"
       if kd == (apiKind (undefined :: Attachments))
         then
           do
             atts <- parseJSON =<< (v .:? "items" .!= (emptyArray))
             return $ Attachments atts
         else
           fail "wrong kind"
   parseJSON _ = fail "Not a Setting object"

{--
  "kind": "mirror#attachmentsList",
  "items": [
    timeline.attachments Resource
  ]
--}

{--
  "notification": {
    "level": string,
    "deliveryTime": datetime
  }
--}

data MenuNotification = MenuNotification
   { notificationLevel        :: Possible Text
   , notificationDeliveryTime :: Possible UTCTime
   }
   deriving (Show)
instance ToJSON MenuNotification where
  toJSON (MenuNotification l t) = object
     [ "level"         .= toJSON l
     , "deliveryTime"  .= toJSON t
     ]

instance FromJSON MenuNotification where
   parseJSON (Object v) =
      MenuNotification <$> v .:?? "level"
                       <*> v .:?? "deliveryTime"
   parseJSON _ = fail "Not a Notification object"

data MenuItemValue = MenuItemValue
       { menuItemValueState       :: Possible Text
       , menuItemValueDisplayName :: Possible Text
       , menuItemValueIconUrl     :: Possible Text
       } deriving (Show)

instance ToJSON MenuItemValue where
  toJSON (MenuItemValue {..}) = object
     [ "state"       .= toJSON menuItemValueState
     , "displayName" .= toJSON menuItemValueDisplayName
     , "iconUrl"     .= toJSON menuItemValueIconUrl
     ]

instance FromJSON MenuItemValue where
   parseJSON (Object v) =
      MenuItemValue <$> v .:?? "state"
                    <*> v .:?? "displayName"
                    <*> v .:?? "iconUrl"
   parseJSON _ = fail "Not a Notification object"

data MenuItem = MenuItem
    { menuItemId                 :: Possible  Text
    , menuItemAction             :: Possible  Text
    , menuItemValues             :: Possible [MenuItemValue]
    , menuItemRemoveWhenSelected :: Possible  Bool
    , menuItemPayload            :: Possible  Text
    }  deriving Show

instance ToJSON MenuItem where
  toJSON (MenuItem {..}) = object
     [ "id"                 .= toJSON menuItemId
     , "action"             .= toJSON menuItemAction
     , "values"             .= toJSON menuItemValues
     , "removeWhenSelected" .= toJSON menuItemRemoveWhenSelected
     , "payload"            .= toJSON menuItemPayload
     ]
instance FromJSON MenuItem where
   parseJSON (Object v) =
      MenuItem <$> v .:?? "id"
               <*> v .:?? "action"
               <*> v .:?? "values"
               <*> v .:?? "removeWhenSelected"
               <*> v .:?? "payload"
   parseJSON _ = fail "Not a MenuItem object"


{--

{
  "kind": "mirror#timelineItem",
  "id": string,
  "sourceItemId": string,
  "canonicalUrl": string,
  "bundleId": string,
  "isBundleCover": boolean,
  "selfLink": string,
  "created": datetime,
  "updated": datetime,
  "displayTime": datetime,
  "isPinned": boolean,
  "pinScore": integer,
  "isDeleted": boolean,
  "etag": etag,
  "creator": contacts Resource,
  "recipients": [
    contacts Resource
  ],
  "inReplyTo": string,
  "title": string,
  "text": string,
  "html": string,
  "speakableType": string,
  "speakableText": string,
  "attachments": [
    timeline.attachments Resource
  ],
  "location": locations Resource,
  "menuItems": [
    {
      "id": string,
      "action": string,
      "values": [
        {
          "state": string,
          "displayName": string,
          "iconUrl": string
        }
      ],
      "removeWhenSelected": boolean,
      "payload": string
    }
  ],
  "notification": {
    "level": string,
    "deliveryTime": datetime
  }
}
--}
type Etag = Text
instance ApiKind TimelineItem where
  apiKind _ = "mirror#timelineItem"

instance Default TimelineItem where
  def = TimelineItem MissingData MissingData MissingData MissingData MissingData MissingData
                     MissingData MissingData MissingData MissingData MissingData MissingData
                     MissingData MissingData MissingData MissingData MissingData MissingData
                     MissingData MissingData MissingData MissingData MissingData MissingData
                     MissingData

data TimelineItem = TimelineItem
   { timelineItemId            :: Possible  Text 
   , timelineItemSourceItemId  :: Possible  Text
   , timelineItemCanonicalUrl  :: Possible  Text
   , timelineItemBundleId      :: Possible  Text
   , timelineItemIsBundleCover :: Possible  Bool
   , timelineItemSelfLink      :: Possible  String
   , timelineItemCreated       :: Possible  UTCTime
   , timelineItemUpdated       :: Possible  UTCTime
   , timelineItemDisplayTime   :: Possible  UTCTime
   , timelineItemIsPinned      :: Possible  Bool
   , timelineItemPinScore      :: Possible  Int
   , timelineItemIsDeleted     :: Possible  Bool
   , timelineItemEtag          :: Possible  Etag
   , timelineItemCreator       :: Possible  Contact
   , timelineItemRecipients    :: Possible [Contact] -- or Contacts?
   , timelineItemInReplyTo     :: Possible  Text
   , timelineItemTitle         :: Possible  Text
   , timelineItemText          :: Possible  Text
   , timelineItemHtml          :: Possible  Text
   , timelineItemSpeakableType :: Possible  Text
   , timelineItemSpeakableText :: Possible  Text
   , timelineItemAttachments   :: Possible [Attachment]
   , timelineItemLocation      :: Possible  Location
   , timelineItemMenuItems     :: Possible [MenuItem]
   , timelineItemNotification  :: Possible  MenuNotification
   } deriving Show

instance ToJSON TimelineItem where
  toJSON a@(TimelineItem {..}) = object
     [ "id"            .= toJSON timelineItemBundleId
     , "sourceItemId"  .= toJSON timelineItemSourceItemId
     , "canonicalUrl"  .= toJSON timelineItemCanonicalUrl
     , "bundleId"      .= toJSON timelineItemBundleId
     , "isBundleCover" .= toJSON timelineItemIsBundleCover
     , "selfLink"      .= toJSON timelineItemSelfLink
     , "created"       .= toJSON timelineItemCreated
     , "updated"       .= toJSON timelineItemUpdated
     , "displayTime"   .= toJSON timelineItemDisplayTime
     , "isPinned"      .= toJSON timelineItemIsPinned
     , "pinScore"      .= toJSON timelineItemPinScore
     , "isDeleted"     .= toJSON timelineItemIsDeleted
     , "etag"          .= toJSON timelineItemEtag
     , "creator"       .= toJSON timelineItemCreator
     , "recipients"    .= toJSON timelineItemRecipients
     , "inReplyTo"     .= toJSON timelineItemInReplyTo
     , "title"         .= toJSON timelineItemTitle
     , "text"          .= toJSON timelineItemText
     , "html"          .= toJSON timelineItemHtml
     , "speakableType" .= toJSON timelineItemSpeakableType
     , "speakableText" .= toJSON timelineItemSpeakableText
     , "attachments"   .= toJSON timelineItemAttachments
     , "location"      .= toJSON timelineItemLocation
     , "menuItems"     .= toJSON timelineItemMenuItems
     , "notification"  .= toJSON timelineItemNotification
     , "kind"          .= apiKind a
     ]


instance FromJSON TimelineItem where
   parseJSON (Object v) = do
       kd <- v .: "kind"
       if kd == apiKind (undefined :: TimelineItem)
         then
           TimelineItem <$> v .:?? "id"
                        <*> v .:?? "sourceItemId"
                        <*> v .:?? "canonicalUrl"
                        <*> v .:?? "bundleId"
                        <*> v .:?? "isBundleCover"
                        <*> v .:?? "selfLink"
                        <*> v .:?? "created"
                        <*> v .:?? "updated"
                        <*> v .:?? "displayTime"
                        <*> v .:?? "isPinned"
                        <*> v .:?? "pinScore"
                        <*> v .:?? "isDeleted"
                        <*> v .:?? "etag"
                        <*> v .:?? "creator"
                        <*> v .:?? "recipients"
                        <*> v .:?? "inReplyTo"
                        <*> v .:?? "title"
                        <*> v .:?? "text"
                        <*> v .:?? "html"
                        <*> v .:?? "speakableType"
                        <*> v .:?? "speakableText"
                        <*> v .:?? "attachments"
                        <*> v .:?? "location"
                        <*> v .:?? "menuItems"
                        <*> v .:?? "notification"
         else
           fail "wrong kind of TimelineItem"
   parseJSON _ = fail "Not a TimelineItem object"

instance ApiKind Timeline where
  apiKind _ = "mirror#timeline"

instance Default Timeline where
  def = Timeline MissingData MissingData

data Timeline = Timeline
     { timelineNextPageToken :: Possible  Text
     , timelineItems         :: Possible [TimelineItem]
     } deriving Show

instance FromJSON Timeline where
   parseJSON (Object v) = do
       kd <- v .: "kind"
       if kd == apiKind (undefined :: Timeline)
         then
           Timeline <$> v .:?? "nextPageToken"
                    <*> v .:?? "items"
         else
           fail "wrong kind of Timeline"
   parseJSON _ = fail "Not a Timeline object"

instance ToJSON Timeline where
  toJSON a@(Timeline n l) = object
     [ "items"         .= toJSON l
     , "nextPageToken" .= toJSON n
     , "kind"          .= apiKind a
     ]
{--
  "kind": "mirror#timeline",
  "nextPageToken": string,
  "items": [
    timeline Resource
  ]
--}