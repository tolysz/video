{-# LANGUAGE  ScopedTypeVariables#-}
module Handler.Home where

import Import
import Text.Julius
import Text.Naked.Coffee

import Yesod.AngularUI
import Yesod.WebSockets
import Yesod.WebSockets.Extra

import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Bool
import qualified Network.Wai as Wai (remoteHost, requestHeaders)
import qualified Data.ByteString.Lazy as BSL
import Debug.Trace
import System.IO.Unsafe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Lens as DA
import Data.Aeson.Lens as DA
import qualified Data.Aeson as DA


import Handler.Translate
import Handler.DB
import Permissions
type LangCache = Map LangId Html

anonCache :: MVar LangCache
anonCache = unsafePerformIO $ newMVar Map.empty
{-# NOINLINE anonCache #-}

handleHomeR :: [Text] ->  Handler Html
handleHomeR _ =  do
           req <- waiRequest
           let ip = T.pack . show . Wai.remoteHost $ req
               xip = maybe ip ( ( <> (T.drop 9 ip)) . E.decodeUtf8) $ lookup "X-Real-IP" (Wai.requestHeaders req)
           (maid, loggedIn) <- maybeAuthId >>= \case
               Nothing ->  return  ( "not logged in" , False)
               Just n ->   return . (,True ) . maybe xip usersUuid =<< (runDB . get $ n)
           langI18Ang <- getUserLang
           webSockets ( chatApp (bool xip maid loggedIn))

           perms <- userPerms
           hasCache  <- isJust . join . fmap (Map.lookup langI18Ang) <$> tryReadMVar anonCache

           if ((not hasCache && not loggedIn) || loggedIn)
               then do
                   {-- ap :: AuthPerms  <- queryDB sadasd -}
                   {- conf <- liftIO getIt -}
                   mrender <- getMessageRender
                   let
                     jsi18n :: SomeMessage App -> RawJavascript
                     jsi18n m = rawJS $ mrender $ m
                   TC thm <- getUserThemeR
                   res <- genAngularBind
                       perms
                       jsi18n   -- ^ javascript convertor for messages
                       langI18Ang -- ^ user languages
                       thm -- ^ user theme
                       maid -- loggedIn
                       (compiledAsDevel || (isDebugger perms)){- -> ap-> conf -> -} (\y x ->
                         angularUILayout y $ do
                           setTitle "Video Selector" -- "Welcome To Yesod!"
                           x
                         )

                   when (not hasCache && not loggedIn) $ void $ do
                       m1 <- takeMVar anonCache
                       tryPutMVar anonCache $ Map.insert langI18Ang res m1
                   return res
               else do
                  m1 <- readMVar anonCache
                  maybe (handleHomeR []) return $ Map.lookup langI18Ang m1

genAngularBind :: Permssions -> (SomeMessage App -> RawJavascript) -> LangId -> Theme-> Text -> Bool  ->  ( Text -> Widget  ->  Handler Html ) -> Handler Html
genAngularBind (perm@Permssions{..}) jsi18n appLang thm maid development = do
  runAngularUI $ cached $ do
    addConstant "maid"    [js|#{rawJS $ show maid}|]
    addConstant "appLang" [js|#{toJSON appLang}|]
    addConstant "perms"   [js|#{toJSON perm}|]
    addConstant "appVers"   [js|#{toJSON permVers}|]

    addConstant "thm"   [js|#{toJSON thm}|]
    addConstant "thmColours" [js|['red', 'pink', 'purple', 'deep-purple', 'indigo', 'blue', 'light-blue', 'cyan', 'teal', 'green', 'light-green', 'lime', 'yellow', 'amber', 'orange', 'deep-orange', 'brown', 'grey', 'blue-grey']|]

--     addValue    "debug"   [js|false|]

    addRun [js|function($rootScope,$state, $stateParams){
       $rootScope.appDebug = false;
       $rootScope.setDebug = function (x){$rootScope.appDebug = x;};

       $rootScope.$state = $state;
       $rootScope.$stateParams = $stateParams;
       $rootScope.$on("$stateChangeSuccess",  function(event, toState, toParams, fromState, fromParams) {
                // to be used for back button //won't work when page is reloaded.
                $rootScope.previousState_name = fromState.name;
                $rootScope.previousState_params = fromParams;
        });
            //back button function called from back button's ng-click="back()"
        $rootScope.back = function() {
                $state.go($rootScope.previousState_name,$rootScope.previousState_params);
          };
     }|]

    addConfig "$log"      [js|debugEnabled(#{development})|]
    addConfig "$compile"  [js|debugInfoEnabled(#{development})|]
    addConfig "$http"     [js|useApplyAsync(true)|]
    addConfig "$location" [js|html5Mode({rewriteLinks:true, requireBase:true, enabled: true})|]

    addConfigRaw [js|function($mdThemingProvider, thm, thmColours){
    
    red = function(c){
     if (_.indexOf(thmColours, c) > -1)
        {
           return c;
        }
        else
        {
           return 'red';
        }

    }

    app = function (name){
       try{
           $mdThemingProvider.theme(name)
             .primaryPalette   (red(thm[name]['primary'])   )
             .accentPalette    (red(thm[name]['accent'] )   )
             .warnPalette      (red(thm[name]['warn']   )   )
             .backgroundPalette(red(thm[name]['background']));

           if (thm[name]['dark'])
                  $mdThemingProvider.theme(name).dark();
         } catch(e){};

         };

    _.map ( _.keys(thm), app);

      $mdThemingProvider.setDefaultTheme('default');
      $mdThemingProvider.alwaysWatchTheme(true);
}|]


    addModules [ "ui.router"
               , "ngSanitize"
               , "ngAnimate"
               , "ngAria"
               , "ngCookies"
               , "ngMaterial"
               , "ngResource"
               , "ngLocale"
               , "angulartics"
               , "angulartics.google.analytics"
               ]

    state $(utcFile "/demos"           "demos"               )
    state $(utcFile "/empty"           "demos.empty"         )
    state $(utcFile "/panel"           "demos.panel"         )
    state $(utcFile "/button"          "demos.button"        )
    state $(utcFile "/checkbox"        "demos.checkbox"      )
    state $(utcFile "/content"         "demos.content"       )
    state $(utcFile "/dialog"          "demos.dialog"        )
    state $(utcFile "/slider"          "demos.slider"        )
    state $(utcFile "/textfield"       "demos.textfield"     )
    state $(utcFile "/youtube"         "demos.youtube"       )
    state $(utcFile "/about"           "demos.about"         )

--     state $(utcFile  "/oauth2"         "oauth2"              )  -- show only to channel admin who autenticated oauth
    state (url   "/oauth2"  >> nameA "oauth2")
    state $(utcVFile "/:uuid/channels"       "oauth2.channels"  "@")
    state $(utcVFile "/:uuid/refresh"        "oauth2.refresh"   "@")
--
    state $(utcVFile "/:uuid/playlists/:cid" "oauth2.playlists" "@")
    state $(utcVFile "/:uuid/playlist/:pid"  "oauth2.playlist"  "@")
    state $(utcVFile "/:uuid/video/:vid"     "oauth2.video"     "@")
    state $(utcFile  "/admin"           "admin"               )  -- only channel admin
    state $(utcFile  "/video"           "admin.video"         )
    state $(utcFile  "/group"           "admin.group"         )  -- require special permissions
    state $(utcFile  "/add"             "admin.group.add"     )  -- require special permissions
    state $(utcFile  "/:uuid/edit"      "admin.group.edit"    )  -- require special permissions
    state $(utcFile  "/:uuid/user"      "admin.group.user"    )  -- require special permissions
    state $(utcVFile "/add"             "admin.group.user.add" "@")
    state $(utcVFile "/:uuuid/edit"     "admin.group.user.edit" "@")
    state $(utcFile  "/user"            "admin.user"          )  -- require special permissions
    state $(utcVFile "/add"             "admin.user.add"  "@" )  -- require special permissions
    state $(utcVFile "/edit/:uuid"      "admin.user.edit" "@" )  -- require special permissions
    state $(utcFile  "/site"            "site"                )  -- will be per user
    state $(utcVFile "/:uuid/playlists" "site.playlists" "@" )  -- will be per user
    state $(utcVFile "/:pluuid"         "site.playlists.details" "@" )  -- will be per user
    state $(utcFile  "/auth/logout"     "logout"              )
    state $(utcFile  "/auth/login"      "login"               )

    when (isLogged) $ do
        state $(utcFile  "/chat"       "chat"                )  -- will be per user
        state $(utcFile  "/settings"   "settings"            )
        state $(utcVFile "/theme"   "settings.theme"   "@")
        state $(utcVFile "/me"         "settings.me"      "@")

    setDefaultRoute "site"

    addController "LeftCtrl"          $(ncoffeeFile "angular/_lib/Controlers/LeftCtrl.coffee"      )
    addController "RightCtrl"         $(juliusFile  "angular/_lib/Controlers/RightCtrl.julius"     )
    addController "AppCtrl"           $(juliusFile  "angular/_lib/Controlers/AppCtrl.julius"       )

    addController "ContactChipDemoCtrl" [js|  function ($timeout, $q) {
                             var self = this;

                             self.querySearch = querySearch;
                             self.allContacts = loadContacts();
                             self.contacts = [self.allContacts[0]];
                             self.filterSelected = true;

                             /**
                              * Search for contacts.
                              */
                             function querySearch (query) {
                               var results = query ?
                                   self.allContacts.filter(createFilterFor(query)) : [];
                               return results;
                             }

                             /**
                              * Create filter function for a query string
                              */
                             function createFilterFor(query) {
                               var lowercaseQuery = angular.lowercase(query);

                               return function filterFn(contact) {
                                 return (contact._lowername.indexOf(lowercaseQuery) != -1);;
                               };

                             }

                             function loadContacts() {
                               var contacts = [
                                 'Marina Augustine',
                                 'Oddr Sarno',
                                 'Nick Giannopoulos',
                                 'Narayana Garner',
                                 'Anita Gros',
                                 'Megan Smith',
                                 'Tsvetko Metzger',
                                 'Hector Simek',
                                 'Some-guy withalongalastaname'
                               ];

                               return contacts.map(function (c, index) {
                                 var cParts = c.split(' ');
                                 var contact = {
                                   name: c,
                                   email: cParts[0][0].toLowerCase() + '.' + cParts[1].toLowerCase() + '@example.com',
                                   image: 'http://lorempixel.com/50/50/people?' + index
                                 };
                                 contact._lowername = contact.name.toLowerCase();
                                 return contact;
                               });
                             }
                           }
    |]


    addFilter     "splitChars"        $(juliusFile  "angular/_lib/Filters/splitChars.julius"       )
    addFilter     "splitChars2"       $(ncoffeeFile "angular/_lib/Filters/splitChars2.coffee"      )
    addService    "youtubeEmbedUtils" $(juliusFile  "angular/_lib/Service/youtubeEmbedUtils.julius")
    addDirective  "youtubeVideo"      $(juliusFile  "angular/_lib/Directive/youtubeVideo.julius"   )
    addFactory    "ytPlayer" [js| function (){
      var player, curr_vars;

      var methods = { player: player, curr_vars: curr_vars};
      return methods;

    } |]

    addFactory "User"      [js| function($resource) { return $resource("@{UserR}/:uuid"); }|]
    addFactory "Group"     [js| function($resource) { return $resource("@{SiteGroupR}/:uuid"); }|]
    addFactory "GroupUser" [js| function($resource) { return $resource("@{SiteGroupUser0R}/:uuid/:uuuid"); }|]

    addFactory "wsLink" [js| function($rootScope, $log, maid, $mdToast, $timeout, $interval, $window, appVers, appLang) {
      // Open a WebSocket connection
      var methods = {};
      var collection = [];
      url = '@{HomeR []}'.replace(/^http/, "ws");
      var dataStream = {};

      function toast(m, d){
         $mdToast.show(
            $mdToast.simple()
               .content(m)
               .position("bottom left right")
               .hideDelay( d ? d : 3000 )
             );
      }

      function open (){
      dataStream = new WebSocket(url);

      dataStream.onerror = function(e) {
             $log.debug(e);
             $timeout ( open, 5000);
             $log.debug("reconnecting");
           };

      dataStream.onmessage = function(message) {
          $log.debug(message);
          var myReader = new FileReader();
          myReader.readAsText(message.data);
          myReader.addEventListener("loadend", function(e){
              $rootScope.$apply( function() {
                $log.debug(e);
                var buffer = JSON.parse((e.srcElement ? e.srcElement : e.target).result); // e.srcElement
                $log.debug(buffer);
                collection.unshift({tag:buffer.tag, cont:buffer.contents});

                if (buffer.tag == 'Shout')
                  toast(buffer.contents[0] + " -> " + buffer.contents[2]);
                if (buffer.tag == 'MsgVersion' && buffer.contents[1] !== appVers)
                    $window.location.reload(); // possibly add msg
              })
            });
      }

      };

      try{
         open();
      methods = { collection: collection
                    , get: function(s) {
                             dataStream.send(JSON.stringify({ action: 'get', value: s }));
                            }
                    , shout: function(s){
                               dataStream.send(JSON.stringify({ tag: 'Shout', contents: [maid, new Date(), s, appLang] }));
                            }
                    };
      }  catch(e) {

//      toast("error... retry", 1000);
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
              , getImg: ""
              , setImg: function (nt){ this.getImg = nt; }
              , getLink: ""
              , setLink: function (nt){ this.getLink = nt; }

              }
     }
    |]
    addFactory "sections" [ncoffee|
(perms) ->
  sections =
    [
      state: "site"
      name: "%{jsi18n (SomeMessage MsgMenuSite)}"
      visible: false
      pages: []
    ,
      state: "chat"
      name:  "%{jsi18n (SomeMessage MsgMenuChat)}"
      visible : false
      pages: []
      logged: true
    ,
      state : "demos"
      name:   "%{jsi18n (SomeMessage MsgDemos)}"
      visible : false
      pages: [ { state: "demos.panel",     name: "Pannel",     icon: "fa columns font-menu-icon font-lg" }
             , { state: "demos.button",    name: "Button",     icon: "fa barcode font-menu-icon font-lg" }
             , { state: "demos.checkbox",  name: "Checkbox",   icon: "fa barcode font-menu-icon font-lg" }
             , { state: "demos.content",   name: "Content",    icon: "fa barcode font-menu-icon font-lg" }
             , { state: "demos.dialog",    name: "Dialog",     icon: "fa barcode font-menu-icon font-lg" }
             , { state: "demos.slider",    name: "Slider",     icon: "fa barcode font-menu-icon font-lg" }
             , { state: "demos.textfield", name: "Text Field", icon: "fa barcode font-menu-icon font-lg" }
             , { state: "demos.youtube",   name: "Youtube",    icon: "fa youtube font-menu-icon font-lg" }
             , { state: "demos.empty",     name: "%{jsi18n (SomeMessage MsgHello)}",      icon: "fa frown-o font-menu-icon font-lg" }
             , { state: "demos.about",     name: "About",      icon: "fa info font-menu-icon font-lg" }
             ]
    ,
      state: "settings"
      name:  "%{jsi18n (SomeMessage MsgMenuSettings)}"
      visible : false
      pages: [ { state: "settings.me",     name: "Me",     icon: "fa columns font-menu-icon font-lg" }
             , { state: "settings.theme",  name: "theme",  icon: "fa columns font-menu-icon font-lg" }
             ]
      logged: true
    ,
      state: "admin"
      name: "%{jsi18n (SomeMessage MsgMenuAdmin)}"
      visible: false
      pages: [ { state:"admin.video", name: "%{jsi18n (SomeMessage MsgMenuAdminVideo)}", icon: "fa video-camera  font-menu-icon font-lg"}
             , { state:"admin.group", name: "%{jsi18n (SomeMessage MsgMenuAdminGroup)}", icon: "fa group font-spin font-menu-icon font-lg"}
             #, { state:"admin.group.add", name: "group add", icon: "fa group font-spin  font-menu-icon font-lg"}
             , { state:"admin.user", name: "%{jsi18n (SomeMessage MsgMenuAdminUsers)}", icon: "fa users font-menu-icon font-lg"}
             ]
      admin: true
#    ,
#      state: "oauth2"
#      name:  "OAuth2"
#      visible : false
#      pages: [ { state: "oauth2.channels",     name: "Channels",      icon: "fa list-alt font-menu-icon font-lg" }]
#      admin: true
    ]
  return _.reject(sections,
    (x) -> (x.admin && ! perms.isAdmin) || (x.logged && ! perms.isLogged)
    )
|]

noop = return ()

-- langIdLocale :: LangId -

postLangR :: Handler ()
postLangR = do
    setUltDestReferer
    lang <- runInputPost $ ireq textField "lang"
    setLanguage lang
    redirectUltDest (HomeR [])


acceptOwnLanguageTranslations lid k u ( Transl     _ t l) = if l == lid then HaveData () else HaveNull
acceptOwnLanguageTranslations lid _ _ _ = MissingData

chatApp :: Text  ->  WebSocketsT Handler ()
chatApp name = do
    lid <- lift getUserLang
    chans <- userChannels <$> getYesod
    appVers <- appVersion <$> getYesod
    now1 <- liftIO getCurrentTime
    runInnerHandler <- lift handlerToIO

    sendBinaryData $ MsgVersion now1 appVers
    sendBinaryData $ MsgInfo now1 ("Welcome, " <> name) lid

    (rChan, keepWS) <- atomically $ do
        adjustFilter (\_ -> [\ k u t -> if k == u
                               then
                                 HaveNull
                               else
                                 case t of
                                   _ -> MissingData
                            , acceptOwnLanguageTranslations lid

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
       let ech = (toEcho msg)
       case ech of
         Just (SelfEcho t m l) -> do
            b <- runInnerHandler $ translate lid m
            atomically $ mapM (\(lang, jso) -> do
                case DA.decode jso of
                  Nothing -> return ()
                  Just (v :: Value)  -> do -- {\n \"data\": {\n \"translations\": [\n {\n \"translatedText\": \"Testowanie aplikacji\"\n }\n ]\n }\n}\n
                       let ll = v DA.^? DA.key "data" . DA.key "translations" . DA.nth 0 . DA.key "translatedText" . DA._String
                       case ll of
                         Just m -> broadcastChan (Transl t m lang) name chans
                         _ -> return ()
                ) b
            print $ l `lookup` b
         _ -> return ()

       atomically (do
          maybe noop (sendSendChan False chans name) ech
          broadcastChan msg name chans
          isEmptyTMVar keepWS)
       >>= bool
           (sendClose ("closing" :: Text))
           readWS
    concurrently_ readWS writeWS

