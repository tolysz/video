module Reflex.Material.MdIcon where

import GHCJS.DOM.Element
-- import GHCJS.DOM.HTMLLinkElement

import Reflex
import Reflex.Dom
import Reflex.Dom.Widget
import qualified Data.Map as Map
import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.Trans


addStylesheet :: MonadWidget t m => String -> m ()
addStylesheet l = emptyElWith "link" $ def & attributes
   .~ mconcat [ "href" =: l
              , "rel" =: "stylesheet"
              ]

addMaterialIcons :: MonadWidget t m => m ()
addMaterialIcons = addStylesheet "https://fonts.googleapis.com/icon?family=Material+Icons"

addIcon t = elClass "i" "material-icons" $ text t
-- <i class="{--}material-icons"></i>
-- <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">

mdButton :: MonadWidget t m => m () -> m (Event t ())
mdButton s = do
  (e, (e1,_)) <- elAttr' "button"
     (Map.fromList[("type","button"),("class","md-raised md-primary md-button")]) $ do
       s
       elDynAttr' "div" (constDyn $ Map.singleton "class" "md-ripple-container") blank
--        & setValue .~ fmap (\_ -> "") tweetButton
  let cc = domEvent Click e
--   performEventAsync $ ffor cc $ \_ -> liftIO $ putStrLn "Button clicked"
  performEvent $ ffor cc $ \_ -> do
         liftIO $ putStrLn "Button clicked"
  return $ cc

{-
    $('.ripple').on('click', function (event) {
      event.preventDefault();

      var $div = $('<div/>'),
          btnOffset = $(this).offset(),
      		xPos = event.pageX - btnOffset.left,
      		yPos = event.pageY - btnOffset.top;
      $div.addClass('ripple-effect');
      var $ripple = $(".ripple-effect");

      $ripple.css("height", $(this).height());
      $ripple.css("width", $(this).height());
      $div
        .css({
          top: yPos - ($ripple.height()/2),
          left: xPos - ($ripple.width()/2),
          background: $(this).data("ripple-color")
        })
        .appendTo($(this));

      window.setTimeout(function(){
        $div.remove();
      }, 2000);
-}
