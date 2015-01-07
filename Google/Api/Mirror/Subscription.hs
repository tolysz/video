{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}


module Google.Api.Mirror.Subscription where

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

instance ApiKind Subscription where -- not used
  apiKind _ = "mirror#subscription"

data SubNotification = SubNotification
  { subNotificationCollection  :: Possible Text
  , subNotificationItemId      :: Possible Text
  , subNotificationOperation   :: Possible Text
  , subNotificationUserActions :: Possible [NotUserAction]
  , subNotificationVerifyToken :: Possible Text
  , subNotificationUserToken   :: Possible Text
  } deriving (Show)

instance FromJSON SubNotification where
   parseJSON (Object v) =
    SubNotification <$>  v .:?? "collection"
                    <*>  v .:?? "itemId"
                    <*>  v .:?? "operation"
                    <*>  v .:?? "userActions"
                    <*>  v .:?? "verifyToken"
                    <*>  v .:?? "userToken"
   parseJSON _ = fail "Not a Setting object"

instance ToJSON SubNotification where
  toJSON a@SubNotification{..} = object
     [ "collection"  .= subNotificationCollection
     , "itemId"      .= subNotificationItemId
     , "operation"   .= subNotificationOperation
     , "userActions" .= subNotificationUserActions
     , "verifyToken" .= subNotificationVerifyToken
     , "userToken"   .= subNotificationUserToken
     ]

data NotUserAction = NotUserAction 
  { notUAType    :: Possible Text
  , notUAPayload :: Possible Text
  } deriving (Show)

instance FromJSON NotUserAction where
   parseJSON (Object v) =
      NotUserAction <$>  v .:?? "type"
                    <*>  v .:?? "payload"
   parseJSON _ = fail "Not a Setting object"

instance ToJSON NotUserAction where
  toJSON a@NotUserAction{..} = object
     [ "type"      .= toJSON notUAType
     , "payload"   .= toJSON notUAPayload
     ]
{--




--}


{-- subscription
{
  "kind": "mirror#subscription",
  "id": string,
  "updated": datetime,
  "collection": string,
  "operation": [
    string
  ],
  "callbackUrl": string,
  "verifyToken": string,
  "userToken": string,
  "notification": {
    "collection": string,
    "itemId": string,
    "operation": string,
    "userActions": [
      {
        "type": string,
        "payload": string
      }
    ],
    "verifyToken": string,
    "userToken": string
  }
}
--}



data Subscription = Subscription
  { subscriptionId           :: Possible  Text
  , subscriptionUpdated      :: Possible  UTCTime
  , subscriptionCollection   :: Possible  Text
  , subscriptionOperation    :: Possible [Text]
  , subscriptionCallbackUrl  :: Possible  Text
  , subscriptionVerifyToken  :: Possible  Text
  , subscriptionUserToken    :: Possible  Text
  , subscriptionNotification :: Possible  SubNotification
  } deriving (Show)



instance Default Subscription where
  def = Subscription MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData


instance ToJSON Subscription where
  toJSON a@Subscription{..} = object $
     [ "id"           .= subscriptionId
     , "updated"      .= subscriptionUpdated
     , "collection"   .= subscriptionCollection
     , "operation"    .= subscriptionOperation
     , "callbackUrl"  .= subscriptionCallbackUrl
     , "verifyToken"  .= subscriptionVerifyToken
     , "userToken"    .= subscriptionUserToken
     , "notification" .= subscriptionNotification
     , "kind"         .= apiKind a
     ]

instance FromJSON Subscription where
   parseJSON (Object v) =
    do
      kd <- v .: "kind"
      if (kd == (apiKind (undefined :: Subscription)))
        then
         Subscription <$>  v .:?? "id"
                      <*>  v .:?? "updated"
                      <*>  v .:?? "collection"
                      <*>  v .:?? "operation"
                      <*>  v .:?? "callbackUrl"
                      <*>  v .:?? "verifyToken"
                      <*>  v .:?? "userToken"
                      <*>  v .:?? "notification"
        else
          fail "wrong kind"
   parseJSON _ = fail "Not a Subscription object"

newtype Subscriptions = Subscriptions [Subscription] deriving Show
instance Default Subscriptions where
   def = Subscriptions []

instance ApiKind Subscriptions where
  apiKind _ = "mirror#subscriptionsList"

instance ToJSON Subscriptions where
  toJSON a@(Subscriptions l) = object $
     [ "items"       .= toJSON l
     , "kind"        .= apiKind a
     ]

instance FromJSON Subscriptions where
   parseJSON (Object v) = do
       kd <- v .: "kind"
       if (kd == (apiKind (undefined :: Subscriptions)))
         then
           do
             l <- v .:? "items" .!= []
             return $ Subscriptions l
         else
           fail "wrong kind"
   parseJSON _ = fail "Not a Setting object"
