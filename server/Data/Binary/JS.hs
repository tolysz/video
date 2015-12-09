{-# LANGUAGE QuasiQuotes   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}


module Data.Binary.JS where

import Prelude
import qualified GHC.Generics as GHC
import Generics.SOP

import Data.Binary.Enum
import Language.Javascript.JMacro
import Data.Monoid

import Data.String.QM

import Data.Binary
import qualified Data.List as DL
class BinaryJS a where
 jsPut :: a -> JExpr
 jsGet :: a -> JExpr
 play  :: a -> String

{--
-- http://stackoverflow.com/questions/15341912/javascript-how-to-go-from-blob-to-arraybuffer
var oBuilder = new BlobBuilder();
read :: Blob a -> CustomEvent a
write a -> Blob a

-}
instance BinaryJS Int

instance (Show a, Enum a, Bounded a, BinaryJS t)=> BinaryJS (BitEnc t a) where
  play (BitEnc a) = main "z" $ map wrapDispatch allValues
    where
   --- let's create angular service
--       main1 f (DL.intercalate "," -> a) = [qm|
-- function(\$q,\$rootScope)\{
--       allValues = [$allTypes];
--
--       \$rootScope.\$broadcast('websocketMsg', messageObj);
--
--       var read = function (blob) \{
--         var deferred = \$q.defer();
--         [$a]
--
--       \};
--
--
--
--       return \{
--                   read: read
--               \};
-- \}
--       |]
      wrapDispatch (show -> a) = [qn|new CustomEvent("${a}",decode${a}) |]
      allTypes = DL.intercalate "," $ map (show . show) allValues
      allValues = tail (a:[minBound .. maxBound])
      -- http://clintberry.com/2013/angular-js-websocket-service/
      main _ _ = let
       url = "ws://localhost:3000/"
       in [qn|
      function($q, $rootScope, $log) {
          // We return this object to anything injecting our service
          var Service = {};
          // Keep all pending requests here until they get responses
          var callbacks = {};
          // Create a unique callback ID to map requests to responses
          var currentCallbackId = 0;
          // Create our websocket object with the address to the websocket
          var ws = new WebSocket("${url}"); // service endpoint

          ws.onopen = function(){
              $log.debug("Socket has been opened!");
          };

          ws.onmessage = function(message) {
              listener(JSON.parse(message.data));
          };

       // need specialised send and get!
          function sendRequest(request) {
            var defer = $q.defer();
            var callbackId = getCallbackId();

            callbacks[callbackId] = {
              time: new Date(),
              cb:defer
            };

            request.callback_id = callbackId;
            $log.debug('Sending request', request);

            ws.send(JSON.stringify(request));

            return defer.promise;
          }

          function listener(data) {
            var messageObj = data;
            $log.debug("Received data from websocket: ", messageObj);
            // If an object exists with callback_id in our callbacks object, resolve it
            if(callbacks.hasOwnProperty(messageObj.callback_id)) {
              $log.debug(callbacks[messageObj.callback_id]);
              $rootScope.$apply(callbacks[messageObj.callback_id].cb.resolve(messageObj.data));
              delete callbacks[messageObj.callbackID];
            }
          }
          // This creates a new callback ID for a request
          function getCallbackId() {
            currentCallbackId += 1;
            if(currentCallbackId > 10000) {
              currentCallbackId = 0;
            }
            return currentCallbackId;
          }

          // Define a "getter" for getting customer data
          Service.getCustomers = function() {
            var request = {
              type: "get_customers" // :: [Customers]
            }
            // Storing in a variable for clarity on what sendRequest returns
            var promise = sendRequest(request);
            return promise;
          }

          return Service;
      }])
   |]

--- test:: putStr $ play (BitEnc True :: BitEnc Int Bool )
