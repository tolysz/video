{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE ScopedTypeVariables
           , TemplateHaskell
           , TypeFamilies
           , GADTs
           , KindSignatures
           , DataKinds
           , PolyKinds
           , TypeOperators
           , FlexibleContexts
           , RankNTypes
           , UndecidableInstances
           , FlexibleInstances
           #-}

module Types.MsgBus where

-- import Yesod.WebSockets
import ClassyPrelude
import Data.String

import qualified Data.Aeson as DA
import qualified Data.Binary as DB

import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy as TL

import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Time

import qualified Network.WebSockets as WS
import Data.Singletons.TH
import Types.Lang

time0 :: UTCTime
time0 = UTCTime (ModifiedJulianDay 0) 0

type Who = Text
data MsgBus
    = Other       !UTCTime !Text
    | Shout  !Who !UTCTime !Text !LangId
    | SystemInfo  !UTCTime !Text
    | MsgInfo     !UTCTime !Text !LangId
    | MsgVersion  !UTCTime !Text
    | SelfEcho    !UTCTime !Text !LangId
    | Transl      !UTCTime !Text !LangId
    | Close  !Who !UTCTime
    | Enter  !Who !UTCTime
        deriving (Show, Eq, Typeable, Generic)

-- singletons [d|
--   data SubChannel
--     = SCOther
--     | SCShout
--     | SCSystemInfo
--     | SCMsgInfo
--     | SCSelfEcho
--      deriving (Enum, Generic)
--   |]

data TimeValue    = TimeValue    UTCTime Text
data TimeValueWho = TimeValueWho UTCTime Text Who

type family WSRequest (chan :: SubChannel) :: * where
  WSRequest 'SCOther      = TimeValue
  WSRequest 'SCShout      = TimeValueWho
  WSRequest 'SCSystemInfo = TimeValue
  WSRequest 'SCMsgInfo    = TimeValue
  WSRequest 'SCSelfEcho   = TimeValue

type family WSResponse (chan :: SubChannel) :: * where
  WSResponse 'SCOther      = ()
  WSResponse 'SCShout      = TimeValue
  WSResponse 'SCSystemInfo = ()
  WSResponse 'SCMsgInfo    = ()
  WSResponse 'SCSelfEcho   = ()

-- forSubscriptionChannel :: SSubChannel c
--   -> (WSRequest c -> m (WSResponse c))
--   -> m ()
-- forSubscriptionChannel = undefined

upTime' :: UTCTime -> MsgBus -> MsgBus
upTime' n ( Other      _ t   ) = Other      n t
upTime' n ( Shout    w _ t l ) = Shout    w n t l
upTime' n ( SystemInfo _ t   ) = SystemInfo n t
upTime' n ( MsgInfo    _ t l ) = MsgInfo    n t l
upTime' n ( MsgVersion   _ t ) = MsgVersion    n t
upTime' n ( SelfEcho   _ t l ) = SelfEcho   n t l
upTime' n ( Transl     _ t l ) = Transl     n t l
upTime' n ( Close    w _     ) = Close    w n
upTime' n ( Enter    w _     ) = Enter    w n

toEcho (Shout _ t m l) = Just (SelfEcho t m l)
toEcho _ = Nothing

-- instance DB.Binary   MsgBus
instance DA.FromJSON MsgBus
instance DA.ToJSON   MsgBus

-- instance DB.Binary Text where
--   put = DB.put . T.encodeUtf8
--   get = T.decodeUtf8 <$> DB.get

instance IsString MsgBus where
  fromString = Other time0 . fromString

instance WS.WebSocketsData MsgBus where
  fromLazyByteString a = fromMaybe (Other time0 . TL.toStrict . TL.decodeUtf8 $ a) (DA.decode a)
  toLazyByteString   = DA.encode
