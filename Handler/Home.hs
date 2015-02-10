{-# LANGUAGE  ScopedTypeVariables#-}
module Handler.Home where

import Import
-- import Text.Hamlet
import Text.Julius
import Text.Naked.Coffee

import Yesod.AngularUI
import Yesod.WebSockets

-- import Data.Time

import Debug.Trace

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
handleHomeR :: Handler Html
handleHomeR =  do
           maid <- maybe (permissionDenied "You need to have login") (return . userIdent) =<< runDB . get =<< requireAuthId
           devel <- appDevelopment . appSettings <$> getYesod
           {-- ap :: AuthPerms  <- queryDB sadasd -}
           {- conf <- liftIO getIt -}
           
           ch <- userChannels <$> getYesod
           
           webSockets ( chatApp ch maid)
--            create websocket
           
           genAngularBind maid devel {- -> ap-> conf -> -} (\y x ->
                 angularUILayout y $ do
                   setTitle "Video Selector" -- "Welcome To Yesod!"
                   -- addStylesheetRemote "//fonts.googleapis.com/css?family=Nothing+You+Could+Do"
                  -- toWidget $(juliusFile "angular/tools.julius")
                   -- toWidget [julius| alert("Hello World!"); |]
                   x
                 )

genAngularBind :: Text -> Bool -> {- AuthPerms-> Value ->  -} ( Text -> Widget  ->  Handler Html ) -> Handler Html
genAngularBind maid  development {- (AuthPerms{..}) something -} = -- do
  -- canViewIt <- verifyBool permsViewSomething apSitePerms

  runAngularUI True {- <- maybe change it to debug? to have instant refreh -} (const $ return ()) $ do
--     let angMenu =  $(hamletFile "angular/menu.hamlet")

    addConstant "maid"    [js|#{rawJS $ show maid}|]

    addConfig "$log"      [js|debugEnabled(#{development})|]
    addConfig "$compile"  [js|debugInfoEnabled(#{development})|]
    addConfig "$http"     [js|useApplyAsync(true)|]
    addConfig "$location" [js|html5Mode({rewriteLinks:false, requireBase:false, enabled: false})|]

    addModules [ "ui.router"
               , "ngSanitize"
               , "ngAnimate"
               , "ngAria"
               , "ngCookies"
               , "ngMaterial"
               , "ngWebSocket"
               , "ngResource"
               ]

    $(addStateJ     "demos"            "/demos"          ) -- could work without passwords
    $(addStateJ     "demos.empty"      "/empty"          )
    $(addStateJ     "demos.panel"      "/panel"          )
    $(addStateJ     "demos.button"     "/button"         )
    $(addStateJ     "demos.checkbox"   "/checkbox"       )
    $(addStateJ     "demos.content"    "/content"        )
    $(addStateJ     "demos.dialog"     "/dialog"         )
    $(addStateJ     "demos.slider"     "/slider"         )
    $(addStateJ     "demos.textfield"  "/textfield"      )
    $(addStateJ     "demos.youtube"    "/youtube"        )
    $(addStateJ     "demos.about"      "/about"          )

    $(addStateJ     "oauth2"           "/oauth2"         )  -- show only to channel admin who autenticated oauth
    $(addStateJ     "oauth2.channels"  "/channels"       )
    $(addStateJ     "oauth2.playlists" "/playlists/:cid" )
    $(addStateJ     "oauth2.playlist"  "/playlist/:pid"  )
    $(addStateJ     "oauth2.video"     "/video/:vid"     )

    $(addStateJ     "admin"            "/admin"          ) -- only channel admin
    $(addStateJ     "admin.video"      "/video"          )
    $(addStateJ     "admin.group"      "/group"          ) -- require special permissions
    $(addStateJ     "admin.group.add"  "/add"            ) -- require special permissions
    $(addStateJ     "admin.group.edit" "/edit/:short"         ) -- require special permissions
    $(addStateJ     "admin.user"       "/user"           ) -- require special permissions
    $(addStateJ     "site"             "/site"           ) -- will be per user

    $(addStateJ     "chat"             "/chat"           ) -- will be per user

    setDefaultRoute "/demos/about"

--     addController "LeftCtrl"   $(juliusFile "angular/_lib/Controlers/LeftCtrl.julius")
    addController "LeftCtrl"          $(ncoffeeFile "angular/_lib/Controlers/LeftCtrl.coffee")
    addFilter     "splitChars"        $(juliusFile  "angular/_lib/Filters/splitChars.julius")
    addFilter     "splitChars2"       $(ncoffeeFile "angular/_lib/Filters/splitChars2.coffee")
    addService    "youtubeEmbedUtils" $(juliusFile  "angular/_lib/Service/youtubeEmbedUtils.julius")
    addDirective  "youtubeVideo"      $(juliusFile  "angular/_lib/Directive/youtubeVideo.julius")
--     addDirective  "resize"            $(juliusFile  "angular/_lib/Directive/resize.julius")
  -- ^ empty
    addFactory "ytPlayer" [js| function (){
      var player, curr_vars;

      var methods = { player: player, curr_vars: curr_vars};
      return methods;

    } |]


    addFactory "Group" [js| function($resource) { return $resource("@{SiteGroupR}/:short"); }|]
    addFactory "User"  [js| function($resource) { return $resource("@{UserR}/:email");      }|]

    addFactory "wsLink" [js| function($websocket, $rootScope, $log, maid) {
      // Open a WebSocket connection
      var dataStream = $websocket('@{HomeR}'.replace("http:", "ws:").replace("https:", "wss:"));
      var collection = [];

      dataStream.onMessage(function(message) {
      $log.debug(message);
      var myReader = new FileReader();
        myReader.readAsText(message.data);
        myReader.addEventListener("loadend", function(e)
            {
              $rootScope.$apply( function() {
                var buffer = JSON.parse(e.srcElement.result);
                $log.debug(buffer);
                // if (_.isUndefined (collection[buffer.tag])) collection[buffer.tag] = [];
                collection.unshift({tag:buffer.tag, cont:buffer.contents});
              })
            });
      });

      var methods = { collection: collection
                    , get: function(s) {
                             dataStream.send(JSON.stringify({ action: 'get', value: s }));
                            }
                    , shout: function(s){
                               dataStream.send(JSON.stringify({ tag: 'Shout', contents: [maid, new Date() ,s] }));
                            }
                    };

      return methods;
    }|]

    addFactory "sections" [ncoffee|
() ->
  sections =
    [
      state: "site"
      name: "site"
      visible: false
      pages: []
    ,
      state: "admin"
      name: "admin"
      visible: false
      pages: [ { state:"admin.video", name: "video", icon: "fa video-camera"}
             , { state:"admin.group", name: "group", icon: "fa group font-spin"}
             , { state:"admin.group.add", name: "group add", icon: "fa group font-spin"}
             , { state:"admin.allusers", name: "allusers", icon: "fa users"}
             ]
    ,
      state: "oauth2"
      name:  "oauth2"
      visible : false
      pages: [ { state: "oauth2.channels",     name: "Channels",      icon: "fa list-alt" }]
    ,
      state: "chat"
      name:  "chat"
      visible : false
      pages: []
    ,
      state : "demos"
      name:   "Demos"
      visible : false
      pages: [ { state: "demos.panel",     name: "Pannel",     icon: "fa columns" }
             , { state: "demos.button",    name: "Button",     icon: "fa barcode" }
             , { state: "demos.checkbox",  name: "Checkbox",   icon: "fa barcode" }
             , { state: "demos.content",   name: "Content",    icon: "fa barcode" }
             , { state: "demos.dialog",    name: "Dialog",     icon: "fa barcode" }
             , { state: "demos.slider",    name: "Slider",     icon: "fa barcode" }
             , { state: "demos.textfield", name: "Text Field", icon: "fa barcode" }
             , { state: "demos.youtube",   name: "Youtube",    icon: "fa youtube" }
             , { state: "demos.empty",     name: "Empty",      icon: "fa frown-o" }
             , { state: "demos.about",     name: "About",      icon: "fa info" }
             ]
    ]
  return sections
|]

noop = return ()

chatApp :: CMap MsgBus -> Text  -> WebSocketsT Handler ()
chatApp chans name = do
    now1 <- liftIO getCurrentTime
    sendBinaryData (MsgInfo now1 $ "Welcome to the chat server, please enter your name.")
    sendBinaryData $ MsgInfo now1 $ "Welcome, " <> name

    rChan <- atomically $ do
        adjustFilter (\_ -> [\ k u _ -> if (trace (show k) k) == name then (trace "N" HaveNull) else (trace "M" MissingData) ]) name chans
        broadcastChan (SystemInfo now1 $ name <> " has joined the chat") name chans
        getChan name chans
    race_
        (forever $ atomically (readTChan rChan) >>= sendBinaryData)
        (sourceWS $$ mapM_C (\(msg :: MsgBus)-> do
          now <- upTime' <$> liftIO getCurrentTime
          atomically $ do
            maybe noop (sendSendChan False chans name) (toEcho $ now msg)
            broadcastChan (now msg) name chans
            ))