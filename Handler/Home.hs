{-# LANGUAGE  ScopedTypeVariables#-}
module Handler.Home where

import Import
-- import Text.Hamlet
import Text.Julius
import Text.Naked.Coffee

import Yesod.AngularUI
import Yesod.WebSockets
import Yesod.WebSockets.Extra
-- import Data.Time

import qualified Data.Text as T
import Data.Bool

import Debug.Trace

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.

getLoginCheckR :: Handler Html
getLoginCheckR = do
   maybeAuthId >>= \case
     Just a -> redirect HomeR
     Nothing -> redirect $ AuthR LoginR


getRedirHashR :: [Text] -> Handler Html
getRedirHashR [] = redirect HomeR
--    cJar <- reqCookies <$> getRequest
--    maybe (redirect HomeR) (\f -> redirect ( HomeR :#: f)) $ "hash" `lookup` cJar
getRedirHashR pa = redirect $ HomeR :#: (T.intercalate "/" pa)

handleHomeR :: Handler Html
handleHomeR =  do
           maid <- maybe (permissionDenied "You need to have login") (return . userIdent) =<< runDB . get =<< requireAuthId
           devel <- appDevelopment . appSettings <$> getYesod
           {-- ap :: AuthPerms  <- queryDB sadasd -}
           {- conf <- liftIO getIt -}
           ch <- userChannels <$> getYesod
           mrender <- getMessageRender
           appLangs <- languages
           let
             jsi18n :: SomeMessage App -> RawJavascript
             jsi18n m = rawJS $ mrender $ m

           webSockets ( chatApp ch maid)
--            create websocket

           genAngularBind
               jsi18n   -- ^ javascript convertor for messages
               appLangs -- ^ user languages
               maid devel {- -> ap-> conf -> -} (\y x ->
                 angularUILayout y $ do
                   setTitle "Video Selector" -- "Welcome To Yesod!"
                   -- addStylesheetRemote "//fonts.googleapis.com/css?family=Nothing+You+Could+Do"
                  -- toWidget $(juliusFile "angular/tools.julius")
                   -- toWidget [julius| alert("Hello World!"); |]
                   x
                 )

genAngularBind :: (SomeMessage App -> RawJavascript) -> [Text] -> Text -> Bool -> {- AuthPerms-> Value ->  -} ( Text -> Widget  ->  Handler Html ) -> Handler Html
genAngularBind jsi18n appLangs maid  development {- (AuthPerms{..}) something -} = do
  -- canViewIt <- verifyBool permsViewSomething apSitePerms
  runAngularUI True {- <- maybe change it to debug? to have instant refreh -} (const $ return ()) $ do
--     let angMenu =  $(hamletFile "angular/menu.hamlet")

    addConstant "maid"     [js|#{rawJS $ show maid}|]
    addConstant "appLangs" [js|#{rawJS $ show appLangs}|]

    addConfig "$log"      [js|debugEnabled(#{development})|]
    addConfig "$compile"  [js|debugInfoEnabled(#{development})|]
    addConfig "$http"     [js|useApplyAsync(true)|]
    addConfig "$location" [js|html5Mode({rewriteLinks:true, requireBase:true, enabled: true})|]

    addModules [ "ui.router"
               , "ngSanitize"
               , "ngAnimate"
               , "ngAria"
               , "ngCookies"
               , "ngMaterial"
               , "ngWebSocket"
               , "ngResource"
               , "ngLocale"
               , "angulartics"
               , "angulartics.google.analytics"
               ]

    $(addStateJ     "demos"                "/demos"          ) -- could work without passwords
    $(addStateJ     "demos.empty"          "/empty"          )
    $(addStateJ     "demos.panel"          "/panel"          )
    $(addStateJ     "demos.button"         "/button"         )
    $(addStateJ     "demos.checkbox"       "/checkbox"       )
    $(addStateJ     "demos.content"        "/content"        )
    $(addStateJ     "demos.dialog"         "/dialog"         )
    $(addStateJ     "demos.slider"         "/slider"         )
    $(addStateJ     "demos.textfield"      "/textfield"      )
    $(addStateJ     "demos.youtube"        "/youtube"        )
    $(addStateJ     "demos.about"          "/about"          )

    $(addStateJ     "oauth2"               "/oauth2"         )  -- show only to channel admin who autenticated oauth
    $(addStateV     "oauth2.channels"  "@" "/channels"       )
    $(addStateV     "oauth2.playlists" "@" "/playlists/:cid" )
    $(addStateV     "oauth2.playlist"  "@" "/playlist/:pid"  )
    $(addStateV     "oauth2.video"     "@" "/video/:vid"     )

    $(addStateJ     "admin"                "/admin"          ) -- only channel admin
    $(addStateJ     "admin.video"          "/video"          )
    $(addStateJ     "admin.group"          "/group"          ) -- require special permissions
    $(addStateJ     "admin.group.add"      "/add"            ) -- require special permissions
    $(addStateJ     "admin.group.edit"     "/:short/edit"    ) -- require special permissions
    $(addStateJ     "admin.group.user"     "/:short/user"    ) -- require special permissions
    $(addStateV     "admin.group.user.add" "@" "/add"            )
    $(addStateJ     "admin.user"           "/user"           ) -- require special permissions
    $(addStateV     "admin.user.add"   "@" "/add"            ) -- require special permissions
    $(addStateV     "admin.user.edit"  "@" "/edit/:ident"    ) -- require special permissions

    $(addStateJ     "site"                 "/site"           ) -- will be per user

    $(addStateJ     "chat"                 "/chat"           ) -- will be per user

    $(addStateJ     "logout"               "/auth/logout"    )

    setDefaultRoute "/demos/about"

    addController "LeftCtrl"          $(ncoffeeFile "angular/_lib/Controlers/LeftCtrl.coffee")
    addController "RightCtrl"         $(juliusFile  "angular/_lib/Controlers/RightCtrl.julius")
    addController "AppCtrl"           $(juliusFile  "angular/_lib/Controlers/AppCtrl.julius")
    addFilter     "splitChars"        $(juliusFile  "angular/_lib/Filters/splitChars.julius")
    addFilter     "splitChars2"       $(ncoffeeFile "angular/_lib/Filters/splitChars2.coffee")
    addService    "youtubeEmbedUtils" $(juliusFile  "angular/_lib/Service/youtubeEmbedUtils.julius")
    addDirective  "youtubeVideo"      $(juliusFile  "angular/_lib/Directive/youtubeVideo.julius")
    addFactory "ytPlayer" [js| function (){
      var player, curr_vars;

      var methods = { player: player, curr_vars: curr_vars};
      return methods;

    } |]

    addFactory "User"      [js| function($resource) { return $resource("@{UserR}/:ident"); }|]
    addFactory "Group"     [js| function($resource) { return $resource("@{SiteGroupR}/:short"); }|]
    addFactory "GroupUser" [js| function($resource) { return $resource("@{SiteGroupR}/:short/user/:ident"); }|]

    addFactory "wsLink" [js| function($websocket, $rootScope, $log, maid, $mdToast, $timeout, $interval) {
      // Open a WebSocket connection
      var methods = {};
      var collection = [];
      url = '@{HomeR}'.replace("http:", "ws:").replace("https:", "wss:");
      var dataStream = {};

      function toast(m, d){
         $mdToast.show(
            $mdToast.simple()
               .content(m)
               .position("top left right")
               .hideDelay( d || 3000)
             );
      }

      function open (){
      dataStream = $websocket(url);

      dataStream.onOpen(function(){ toast("Chat server connected"); });
      dataStream.onClose(function(){ toast("Chat server Disconnected"); });
      dataStream.onError(function(){
        toast("Reconnection");
        $timeout ( open, 5000);
        });

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

                if (buffer.tag == 'Shout')
                  toast(buffer.contents[0] + " -> " + buffer.contents[2]);
              })
            });
      })
      };

      try{
         open();
      methods = { collection: collection
                    , get: function(s) {
                             dataStream.send(JSON.stringify({ action: 'get', value: s }));
                            }
                    , shout: function(s){
                               dataStream.send(JSON.stringify({ tag: 'Shout', contents: [maid, new Date() ,s] }));
                            }
                    };
      }  catch(e) {

      toast("error... retry", 1000);
      $timeout ( open, 5000);

         methods = { collection: collection
                   , get: function(s){}
                   , shout: function(s){}
                   }
      }
      return methods;
    }|]
    addFactory "title" [js| function($log, $timeout){
       return { get: ""
              , set: function (nt){ this.get = nt; }
              }
     }
    |]
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
             #, { state:"admin.group.add", name: "group add", icon: "fa group font-spin"}
             , { state:"admin.user", name: "users", icon: "fa users"}
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
      name:   "%{jsi18n (SomeMessage MsgDemos)}"
      visible : false
      pages: [ { state: "demos.panel",     name: "Pannel",     icon: "fa columns" }
             , { state: "demos.button",    name: "Button",     icon: "fa barcode" }
             , { state: "demos.checkbox",  name: "Checkbox",   icon: "fa barcode" }
             , { state: "demos.content",   name: "Content",    icon: "fa barcode" }
             , { state: "demos.dialog",    name: "Dialog",     icon: "fa barcode" }
             , { state: "demos.slider",    name: "Slider",     icon: "fa barcode" }
             , { state: "demos.textfield", name: "Text Field", icon: "fa barcode" }
             , { state: "demos.youtube",   name: "Youtube",    icon: "fa youtube" }
             , { state: "demos.empty",     name: "%{jsi18n (SomeMessage MsgHello)}",      icon: "fa frown-o" }
             , { state: "demos.about",     name: "About",      icon: "fa info" }
             ]
    ]
  return sections
|]

noop = return ()

postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField "lang"
    setLanguage lang
    redirect HomeR

getLangR :: Handler ()
getLangR = languages >>= redirect . work
  where
   work ((T.unpack -> "en"):_) = StaticR angular_i18n_angular_locale_en_js
   work ((T.unpack -> "en-GB"):_) = StaticR angular_i18n_angular_locale_en_gb_js
   work ((T.unpack -> "en-US"):_) = StaticR angular_i18n_angular_locale_en_us_js
   work ((T.unpack -> "pl"):_) = StaticR angular_i18n_angular_locale_pl_js
   work _ = StaticR angular_i18n_angular_locale_en_js
--    work (_:as) = work as

chatApp :: CMap MsgBus -> Text  -> WebSocketsT Handler ()
chatApp chans name = do
    now1 <- liftIO getCurrentTime
    sendBinaryData (MsgInfo now1 $ "Welcome to the chat server, please enter your name.")
    sendBinaryData $ MsgInfo now1 $ "Welcome, " <> name

    (rChan, keepWS) <- atomically $ do
        adjustFilter (\_ -> [ -- \ k u _ -> if k == name then HaveNull else MissingData
                            -- ,\ k u ( Shout _ _ _)  -> if k == u then HaveNull else MissingData
                            \ k u t -> if k == u
                               then
                                 HaveNull
                               else
                                 case t of
                                   _ -> MissingData
                            ]) name chans
        broadcastChan (Enter name now1) name chans
        (,) <$> getChan name chans <*> newEmptyTMVar
    let
      cleanup _ = void $ liftIO $ do
         void $ atomically . tryPutTMVar keepWS $ False

      writeWS = do
       nowTime <- liftIO getCurrentTime
       atomically (readTChan rChan) >>= sendBinaryDataE >>= either cleanup return
       atomically (isEmptyTMVar keepWS) >>= bool
            ((atomically $ broadcastChan (Close name nowTime) name chans) >> sendClose ("closing" :: Text))
            writeWS

      readWS = do
       nowTime <- liftIO getCurrentTime
       let now = upTime' nowTime
       msg <-  either (\a -> cleanup a >> return (Close name nowTime)) (return . now )=<< receiveDataE
       atomically (do
          maybe noop (sendSendChan False chans name) (toEcho msg)
          broadcastChan msg name chans
          isEmptyTMVar keepWS)
       >>= bool
           (sendClose ("closing" :: Text))
           readWS

    concurrently_ readWS writeWS
