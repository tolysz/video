{-# LANGUAGE OverloadedStrings
  , ViewPatterns
  , RankNTypes
  , KindSignatures
  , CPP
  , FlexibleContexts
  , JavaScriptFFI
   #-}

module Google.Api.Mirror.TimelineValidator (glasifyHTML, glasifyWeb, glasifyEmbed, glasifyURLs) where

import Prelude
import Text.HTML.KURE
import Language.KURE (contextfreeT , applyT, Transform(..), idR)
import Data.String (IsString(..))

import Data.Monoid
import Data.Maybe


#ifdef __GHCJS__
foreign import javascript unsafe "document.write($1+'<br/>');"
    writeNumber :: Int -> IO ()
foreign import javascript safe   "setTimeout($c, $1);"
    delay       :: Int -> IO ()
#else
import Text.Regex.TDFA

replaceAll :: String -> (String -> String) -> String -> String
replaceAll re f s = start end
  where (_, end, start) = foldl go (0, s, id) $ getAllMatches $ s =~ re
        go (ind,read,write) (off,len) =
          let (skip, start)        = splitAt (off - ind) read 
              (matched, remaining) = splitAt len start
          in (off + len, remaining, write . (skip++) . (f matched ++))
urlsFromStyle v = map (drop 4 . init) $ getAllTextMatches ( v =~ ("url\\((.+)\\)" :: String) :: AllTextMatches [] String )
#endif


-- | Get clean version of HTML
glasifyHTML :: Monad m =>  String -> String -> m HTML
glasifyHTML u h = applyT glasifyHTML' mempty (parseHTML u h)

glasifyHTML' :: Monad m => Transform Context m HTML HTML
glasifyHTML' =  htmlT filterTags nop nop htmlC

filterTags :: Monad m => Transform Context m Element HTML
filterTags = elementT filterAttrs glasifyHTML' go
  where
    go (tagToRemove -> True) _ _ = zero
    go t@(tagToKeep -> True) a i = html $ elementC t a i
    go _ _ i = i

tagToRemove :: (IsString a, Eq a) => a -> Bool
tagToRemove a = a `elem` 
 [ {-- Document headers: --} "head", "title"
 , {-- Embeds:           --} "audio", "embed", "object", "source", "video"
 , {-- Frames:           --} "frame", "frameset"
 , {-- Scripting:        --} "applet", "script"
 ]

tagToKeep :: (IsString a, Eq a) => a -> Bool
tagToKeep a = a `elem`
  [ {-- Headers:    --} "h1", "h2", "h3", "h4", "h5", "h6"
  , {-- Images:     --} "img"
  , {-- Lists:      --} "li", "ol", "ul"
  , {-- HTML5:    : --} "article", "aside", "details", "figure", "figcaption", "footer", "header", "nav", "section", "summary", "time"
  , {-- Structural: --} "blockquote", "br", "div", "hr", "p", "span"
  , {-- Style:      --} "b", "big", "center", "em", "i", "u", "s", "small", "strike", "strong", "style", "sub", "sup"
  , {-- Tables:     --} "table", "tbody", "td", "tfoot", "th" , "thead", "tr"
  ]

nop :: forall a (m :: * -> *) c.(Html a, Monad m) => Transform c m a HTML
nop =  contextfreeT (return . html)

nop' :: forall a (m :: * -> *) c.(Html a, Monad m) => Transform c m a [String]
nop' =  contextfreeT (\_ -> return [])

filterAttrs :: Monad m => Transform Context m Attrs Attrs
filterAttrs = attrsT (attrT go) (attrsC . catMaybes)
  where
    go t@(attrToKeep -> True) v = Just $ attrC t v
    go (attrToRemove -> True) v = Nothing
    go t v = Just $ attrC t v

attrToKeep a = a `elem`
   [ "id", "class", "style", "src", "height", "width"]

attrToRemove a = a `elem`
   [ "onafterprint", "onbeforeprint", "onbeforeunload", "onerror"
   , "onhaschange" , "onload"       , "onmessage"     , "onoffline"
   , "ononline"    , "onpagehide"   , "onpageshow"    , "onpopstate"
   , "onredo"      , "onresize"     , "onstorage"     , "onundo"
   , "onunload"    , "onblur"       , "onchange"      , "oncontextmenu"
   , "onfocus"     , "onformchange" , "onforminput"   , "oninput"
   , "oninvalid"   , "onreset"      , "onselect"      , "onsubmit"
   , "onkeydown"   , "onkeypress"   , "onkeyup"       , "onclick"
   , "ondblclick"  , "ondrag"       , "ondragend"     , "ondragenter"
   , "ondragleave" , "ondragover"   , "ondragstart"   , "ondrop"
   , "onmousedown" , "onmousemove"  , "onmouseout"    , "onmouseover"
   , "onmouseup"   , "onmousewheel" , "onscroll"      , "onabort"
   , "oncanplay"   , "oncuechange"  , "oncanplaythrough", "ondurationchange"
   , "onemptied"   , "onended"      , "onerror"       , "onloadeddata"
   , "onloadedmetadata", "onloadstart", "onpause"     , "onplay"
   , "onplaying"   , "onprogress"   , "onratechange"  , "onseeked"
   , "onseeking"   , "onstalled"    , "onsuspend"     , "ontimeupdate"
   , "onvolumechange", "onwaiting"
   ]

-- extract links
-- href <- should be out

-- src="attachment[id], cid:, glass:, https://, http://
-- style="background-image: url(https://mirror-api-playground.appspot.com/links/washington.jpg)"

-- keep attrs


-- | proxy timelineItemId (AttachmentId, AttachmentUrl) -> baseHTML
glasifyWeb :: String -> String -> [(String, String)] -> HTML -> m HTML
glasifyWeb pxurl tid aids = error "glasifyWeb not implemented"

glasifyURLs :: Monad m => HTML -> m [String]
glasifyURLs = applyT glasifyURLs' mempty

glasifyURLs' :: Monad m => Transform Context m HTML [String]
glasifyURLs' =  htmlT filterTagsUrl nop' nop' mconcat

filterTagsUrl :: Monad m => Transform Context m Element [String]
filterTagsUrl = elementT filterAttrsUrl glasifyURLs' (\_ a i -> a <> i)

filterAttrsUrl :: Monad m => Transform Context m Attrs [String]
filterAttrsUrl = attrsT (attrT go) mconcat
   where
      go :: String -> String -> [String]
      go "src" v = [v]
      go "style" v = urlsFromStyle v
      go _ _ = []

-- | Replace Urls
glasifyEmbed ::  Monad m => (String -> String) -> HTML -> m HTML
glasifyEmbed st = applyT (glasifyEmbed' st) mempty

glasifyEmbed' :: Monad m => (String -> String) -> Transform Context m HTML HTML
glasifyEmbed' st =  htmlT (replaceTagsUrl st) nop nop htmlC

replaceTagsUrl :: Monad m => (String -> String) ->  Transform Context m Element HTML
replaceTagsUrl st = elementT (replaceAttrsUrl st) (glasifyEmbed' st) go
  where
    go t a i = html $ elementC t a i

replaceAttrsUrl :: Monad m => (String -> String) -> Transform Context m Attrs Attrs
replaceAttrsUrl st = attrsT (attrT go) attrsC
   where
      go :: String -> String -> Attr
      go "src" v = attr "src" (st v)
      go "style" v = attr "style" $ replaceAll "url\\((.+)\\)" ((\u -> "url(" ++ u ++ ")") . st) v
       --map (drop 4 . init) $ getAllTextMatches ( v =~ ("url\\((.+)\\)" :: String) :: AllTextMatches [] String )
      go a v = attr a v
