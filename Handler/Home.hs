module Handler.Home where

import Import

import Data.Maybe

import Text.Hamlet
import Text.Julius
-- import Text.Coffee
import Text.Naked.Coffee

import Yesod.AngularUI

--import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
--                              withSmallInput)
--
-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
handleHomeR :: Handler Html
handleHomeR =  do
           maid <- fmap (userIdent . fromJust) . runDB . get =<< requireAuthId
           {-- ap :: AuthPerms  <- queryDB sadasd -}
           {- conf <- liftIO getIt -}
           genAngularBind maid {- -> ap-> conf -> -} (\y x ->
                 angularUILayout y $ do
                   setTitle "Video Selector" -- "Welcome To Yesod!"
                   -- addStylesheetRemote "//fonts.googleapis.com/css?family=Nothing+You+Could+Do"
                  -- toWidget $(juliusFile "angular/tools.julius")
                   -- toWidget [julius| alert("Hello World!"); |]
                   x
                 )


development :: Bool
development = True

genAngularBind :: Text-> {- AuthPerms-> Value ->  -} ( Text -> Widget  ->  Handler Html ) -> Handler Html
genAngularBind maid  {- (AuthPerms{..}) something -} = -- do
  -- canViewIt <- verifyBool permsViewSomething apSitePerms

  runAngularUI True {- <- maybe change it to debug? to have instant refreh -} (const $ return ()) $ do
    let angMenu =  $(hamletFile "angular/menu.hamlet")

    addConstant "maid"    [js|#{rawJS $ show maid}|]

    addConfig "$log"      [js|debugEnabled(#{development})|]
    addConfig "$compile"  [js|debugInfoEnabled(#{development})|]
    addConfig "$http"     [js|useApplyAsync(true)|]
    addConfig "$location" [js|html5Mode({rewriteLinks:false, requireBase:false, enabled: false})|]

    addModules [ "ui.router"
               , "ngSanitize"
               , "ngAnimate"
               , "ngAria"
               , "ngMaterial"
               ]

    $(addStateJ     "demos"           "/demos"     )
    $(addStateJ     "demos.empty"     "/empty"     )
    $(addStateJ     "demos.panel"     "/panel"     )
    $(addStateJ     "demos.button"    "/button"    )
    $(addStateJ     "demos.checkbox"  "/checkbox"  )
    $(addStateJ     "demos.content"   "/content"   )
    $(addStateJ     "demos.dialog"    "/dialog"    )
    $(addStateJ     "demos.slider"    "/slider"    )
    $(addStateJ     "demos.textfield" "/textfield" )
    $(addStateJ     "demos.youtube"   "/youtube"   )
    $(addStateJ     "demos.about"     "/about"     )

    setDefaultRoute "/demos/about"

--     addController "LeftCtrl"   $(juliusFile "angular/Controlers/LeftCtrl.julius")
    addController "LeftCtrl"    $(ncoffeeFile "angular/Controlers/LeftCtrl.coffee")
    addFilter     "splitChars"  $(juliusFile  "angular/Filters/splitChars.julius")
    addFilter     "splitChars2" $(ncoffeeFile "angular/Filters/splitChars2.coffee")
  -- ^ empty

