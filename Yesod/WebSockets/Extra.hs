
module Yesod.WebSockets.Extra where

import Prelude
import qualified Network.WebSockets             as WS

import Yesod.WebSockets

import Control.Monad.Trans.Reader     (ReaderT (ReaderT))
import Control.Exception (SomeException)
import Control.Exception.Enclosed (tryAny)
import Control.Monad.IO.Class

wrapWSE :: (MonadIO m, WS.WebSocketsData a) => (WS.Connection -> a -> IO ())-> a -> WebSocketsT m (Either SomeException ())
wrapWSE ws x = ReaderT $ liftIO . tryAny . flip ws x

wrapWS :: (MonadIO m, WS.WebSocketsData a) => (WS.Connection -> a -> IO ()) -> a -> WebSocketsT m ()
wrapWS ws x = ReaderT $ liftIO . flip ws x

sendPing :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendPing = wrapWS WS.sendPing

sendPingE :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m (Either SomeException ())
sendPingE = wrapWSE WS.sendPing

sendClose :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m ()
sendClose = wrapWS WS.sendClose

sendCloseE :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m (Either SomeException ())
sendCloseE = wrapWSE WS.sendClose

sendTextDataE :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m (Either SomeException ())
sendTextDataE = wrapWSE WS.sendTextData

-- Binary
sendBinaryDataE :: (MonadIO m, WS.WebSocketsData a) => a -> WebSocketsT m (Either SomeException ())
sendBinaryDataE = wrapWSE WS.sendBinaryData

-- receiveDataE :: (MonadIO m, WS.WebSocketsData a) => WebSocketsT m (Either E.SomeException ())
receiveDataE :: (MonadIO m, WS.WebSocketsData a) => WebSocketsT m (Either SomeException a)
receiveDataE = ReaderT $ liftIO . tryAny . WS.receiveData

sendDataMessageE :: (MonadIO m) => WS.DataMessage -> WebSocketsT m (Either SomeException ())
sendDataMessageE x = ReaderT $ liftIO . tryAny . (\c -> WS.sendDataMessage c x)
-- wrapWSE WS.sendDataMessage

receiveDataMessageE :: (MonadIO m) => WebSocketsT m (Either SomeException WS.DataMessage)
receiveDataMessageE = ReaderT $ liftIO . tryAny . WS.receiveDataMessage
