module Handler.Translate where

{-# LANGUAGE PackageImports, FlexibleContexts, LambdaCase #-}

import Import

import qualified Data.Conduit as C
import Network.HTTP.Types.Status (ok200)

-- import Control.Concurrent.Async (mapConcurrently)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS8
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)
import qualified Data.List as DL
import Data.Bool
import qualified Data.Text as T
import Data.String.QM

-- taggedRequest :: Maybe Text -> LangId -> String -> LangId  -> IO (LangId, Request)
-- taggedRequest (Just (T.unpack -> key)) (toGoogleTrans -> slid) (BS8.toString . urlEncode True . BS8.fromString -> cont) li@(toGoogleTrans -> lid) =
--     (li, ) <$> parseUrl [qm|https://www.googleapis.com/language/translate/v2?key=$key&q=$cont&source=$slid&target=$lid|]
-- taggedRequest Nothing (toGoogleTrans -> slid) (BS8.toString . urlEncode False . BS8.fromString -> cont)  li@(toGoogleTrans -> lid) =
--     (li, ) <$> parseUrl [qm|https://www.googleapis.com/language/translate/v2?q=$cont&source=$slid&target=$lid|]
--
-- taggedResult :: (LangId, Request) -> Handler (LangId, Response LBS.ByteString)
-- taggedResult (lid, req) = (lid, ) <$> httpLbs req

toGoogleTrans :: LangId -> String
toGoogleTrans LangEnGB = "en"
toGoogleTrans LangEnUs = "en"
toGoogleTrans LangPl   = "pl"
toGoogleTrans LangRu   = "ru"
toGoogleTrans LangFr   = "fr"
toGoogleTrans LangIt   = "it"
toGoogleTrans LangDe   = "de"
toGoogleTrans _        = "en"

translate :: LangId -> Text -> Handler [(LangId, LBS.ByteString)]
translate _ _ = return []
-- translate fromLang cont = do
--     key <- appGoogleServerKey . appSettings <$> getYesod
--     requests <- liftIO $ mapM (taggedRequest key fromLang (T.unpack cont)) (DL.filter (\a -> toGoogleTrans fromLang /= toGoogleTrans a )  [LangEnGB ..])
--     runInnerHandler <- handlerToIO
--     catMaybes <$> do
--         results <- liftIO  (mapConcurrently (\ a -> runInnerHandler (taggedResult a)) requests)
--         forM results $ \(lang, rs) ->
--              return $ case responseStatus rs of
--                 ok200 -> Just (lang, responseBody rs)
--                 _     -> Nothing

--translate fromLang toLang cont
--   'https://www.googleapis.com/language/translate/v2?key=&q=hello%20world&source=en&target=de' -O -
