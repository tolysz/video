{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Google.Api.Mirror.Contact where

import           Data.Aeson -- .TH                 (defaultOptions, deriveJSON)
import           Data.Aeson.Types
import           Data.Text                     (Text)
import           Prelude                       hiding (id)
import Data.Possible
--import qualified Prelude                       as P (id)
import Google.Api.Kinds
import Google.Api.Transport
import Data.Default
import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar( Day( ModifiedJulianDay ) )
import Control.Applicative


data Command = Command { commandType  :: Possible  Text } deriving (Show)

instance Default Command where
  def = Command MissingData

instance ToJSON Command where
  toJSON (Command c) = object [ "type" .= c ]

instance FromJSON Command where
   parseJSON (Object v) = Command <$>  v .:?? "type"

{--
  "kind": "mirror#contact",
  "source": string,
  "id": string,
  "displayName": string,
  "imageUrls": [
    string
  ],
  "type": string,
  "acceptTypes": [
    string
  ],
  "phoneNumber": string,
  "priority": unsigned integer,
  "acceptCommands": [
    {
      "type": string
    }
  ],
  "speakableName": string,
  "sharingFeatures": [
    string
  ]
--}

data Contact = Contact
             { contactId              :: Possible  Text
             , contactSource          :: Possible  Text
             , contactDisplayName     :: Possible  Text
             , contactImageUrls       :: Possible [Text]
             , contactType            :: Possible  Text
             , contactAcceptTypes     :: Possible [Text]
             , contactPhoneNumber     :: Possible  Text
             , contactPriority        :: Possible  Int
             , contactAcceptCommands  :: Possible [Command]
             , contactSpeakableName   :: Possible  Text
             , contactSharingFeatures :: Possible [Text]
--              , contactSharingKind     ::
             } deriving (Show)


instance Default Contact where
  def = Contact MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData

instance ApiKind Contact where
  apiKind _ = "mirror#contact"

instance ToJSON Contact where
  toJSON a@Contact{..} = object
     [ "source"          .= contactSource
     , "id"              .= contactId
     , "displayName"     .= contactDisplayName
     , "imageUrls"       .= contactImageUrls
     , "type"            .= contactType
     , "acceptTypes"     .= contactAcceptTypes
     , "phoneNumber"     .= contactPhoneNumber
     , "priority"        .= contactPriority
     , "acceptCommands"  .= contactAcceptCommands
     , "speakableName"   .= contactSpeakableName
     , "sharingFeatures" .= contactSharingFeatures
     , "kind"            .= apiKind a
     ]

instance FromJSON Contact where
   parseJSON (Object v) = do
       kd <- v .: "kind"
       if kd == apiKind (undefined :: Contact)
         then 
             Contact <$>  v .:?? "id"
                     <*>  v .:?? "source"
                     <*>  v .:?? "displayName"
                     <*>  v .:?? "imageUrls"
                     <*>  v .:?? "type"
                     <*>  v .:?? "acceptTypes"
                     <*>  v .:?? "phoneNumber"
                     <*>  v .:?? "priority"
                     <*>  v .:?? "contactAcceptCommands"
                     <*>  v .:?? "speakableName"
                     <*>  v .:?? "sharingFeatures"
         else
           fail "wrong kind"
   parseJSON _ = fail "Not a Setting object"

newtype Contacts = Contacts [Contact] deriving Show

{--
  "kind": "mirror#attachmentsList",
  "items": [
    timeline.attachments Resource
  ]
  
Left "Could not decode JSON
{"kind": "mirror#attachmentsList",
 "items": [ { "id":"bs:2b61735b-b4f6-4d1f-9472-fb7f37046a9c"
            , "contentType": "image/jpeg"
            , "contentUrl":  "https://www.googleapis.com/mirror/v1/timeline/35ccf33c-5d55-4970-bb3b-72d85fdf4140/attachments/bs:2b61735b-b4f6-4d1f-9472-fb7f37046a9c?alt=media"
            }]
}"
--}

instance Default Contacts where
  def = Contacts []

instance ApiKind Contacts where
  apiKind _ = "mirror#contacts"

instance ToJSON Contacts where
  toJSON a@(Contacts l) = object
     [ "items"       .= toJSON l
     , "kind"        .= apiKind a
     ]
instance FromJSON Contacts where
   parseJSON (Object v) = do
       kd <- v .: "kind"
       if kd == apiKind (undefined :: Contacts)
         then
           do
             atts <- parseJSON =<< (v .:? "items" .!= emptyArray)
             return $ Contacts atts
         else
           fail "wrong kind"
   parseJSON _ = fail "Not a Setting object"
