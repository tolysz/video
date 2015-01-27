{-# LANGUAGE RankNTypes, ScopedTypeVariables   #-}
module Data.CMap ( CMap (..), Mask, newCMapIO, getChan, broadcastChan, writeFiltered, sendSendChan ) where

import ClassyPrelude
import qualified Data.Map.Strict as Map
import Data.Bool
import Data.Possible
import qualified ListT
import qualified Focus
import qualified STMContainers.Map as STMMap

type Mask a = (Text -> a -> Possible ())
newtype CMap a = CMap (STMMap.Map Text ([Mask a], TChan a))
-- ^   We send 'a' based on user 'Text' filtered by masks

-- | just an empty initialization
newCMapIO :: forall a. IO (CMap a)
newCMapIO = CMap <$> STMMap.newIO

-- | we extract a channel
getChan :: Text -> CMap a -> STM (TChan a)
getChan k (CMap x) =
        dupTChan =<< ( k `STMMap.lookup` x >>= \case
          Nothing -> do
              ch <- newBroadcastTChan
              STMMap.insert ([],ch) k x
              return ch
          Just (_,ch) -> return ch
          )

-- | send the same info to all and then filtrer
broadcastChan :: a -> CMap a  -> STM ()
broadcastChan msg (CMap x) = mapM_ (uncurry (writeFiltered msg)) =<< ListT.toList (STMMap.stream x)

-- | filter messages as we write them to the channels, so maybe we do not need to write anything !!!
writeFiltered msg _ ( []  , c) = writeTChan c msg
writeFiltered msg k ( f:fs, c) = case f k msg of
          HaveData _  -> writeTChan c msg              -- we accept it for sure
          MissingData -> writeFiltered msg k (fs,c)    -- we just do not know
          HaveNull    -> return ()                     -- we know we failed

adjustFilter :: ([Mask a] -> [Mask a]) -> Text -> CMap a -> STM ()
-- ^ modify filters ideally we just set them
adjustFilter f k (CMap x) = do
     ~ch <- newBroadcastTChan
     STMMap.focus (Focus.alterM (return . Just . maybe (f [],ch) (first f))) k x

-- | we send some data to a specific channel, possibly applying filters
sendSendChan :: forall a. CMap a -> Bool -> Text -> a -> STM ()
sendSendChan (CMap x) b k msg =
    STMMap.lookup k x >>= \case
      Nothing -> return ()
      Just v@(_,c) -> bool (writeTChan c msg) (writeFiltered msg k v) b
