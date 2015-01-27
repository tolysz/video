{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}

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

time0 :: UTCTime
time0 = UTCTime (ModifiedJulianDay 0) 0

type Who = Text
data MsgBus = Other       UTCTime Text
            | Shout   Who UTCTime Text
            | SystemInfo  UTCTime Text
            | MsgInfo     UTCTime Text
            | SelfEcho    UTCTime Text
    deriving (Show, Eq, Typeable, Generic)


upTime :: UTCTime -> MsgBus -> MsgBus
upTime n ( Other      _ t ) = Other      n t
upTime n ( Shout    w _ t ) = Shout    w n t
upTime n ( SystemInfo _ t ) = SystemInfo n t
upTime n ( MsgInfo    _ t ) = MsgInfo    n t
upTime n ( SelfEcho   _ t ) = SelfEcho   n t

toEcho (Shout _ t m) = Just (SelfEcho t m)
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


