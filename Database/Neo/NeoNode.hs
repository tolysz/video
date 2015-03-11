{-# Language RecordWildCards, LambdaCase #-}

module Database.Neo.NeoNode where

import Data.Possible
import Data.Aeson
import Data.Default
import Data.Aeson.Types
import Control.Applicative
import Data.Text (Text)
import Prelude

import Control.Arrow ((***))

import Control.Lens ((^?), (^.))
import Control.Lens.Iso (non)
import Data.Aeson.Lens

data NeoNode a = NeoNode
  { nodeData  :: Possible a
  , nodeUrl   :: Possible String
  , nodeId    :: Possible Integer
  } deriving Show

instance Default (NeoNode a) where
  def = NeoNode MissingData MissingData MissingData

instance FromJSON a => FromJSON (NeoNode a) where
   parseJSON (Object v) =
    (v .:?? "self") >>= \case
      HaveData s -> do
        let (i,u) = ((HaveData . read . reverse) *** (HaveData . reverse . tail)) . break  ('/' ==) . reverse  $ s
        d <- v .:?? "data"
        return $ NeoNode d u i
      _ -> NeoNode
             <$> v .:?? "data"
             <*> v .:?? "url"
             <*> v .:?? "id"
   parseJSON _ = fail "Not a Setting object"

instance (ToJSON a) => ToJSON (NeoNode a) where
  toJSON NeoNode{..} = object
     [ "data" .= toJSON nodeData
     , "url"  .= nodeUrl
     , "id"   .= nodeId
     ]

-- extractNode :: Value -> Node
--extractNode :: Value -> NeoNode a
--extractNode v = 
--   let
--    (i,u) = ((HaveData . read . reverse) *** (HaveData . reverse . tail)) . break  ('/' ==) . reverse  $ (v ^. key "self"  . _String)
--    d = (v ^. key "data"  . _Object)
--   in
--     NeoNode d u i
