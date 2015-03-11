{-# Language OverloadedStrings #-} 

module Database.Neo.Monad where

-- import Database.Neo.Types
import Prelude
import Database.Neo.Cypher
import Database.Neo.Rest
import Database.Neo.Utils

import Control.Monad.Reader
import Control.Monad.State.Strict

import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.Map.Strict as Map

type NeoM a = ReaderT Neo IO a

type CypherM a = State Cypher a


qCyp :: FromJSON a => Neo -> Cypher -> IO (Maybe a)
qCyp n =  postNeo n "cypher"

tl :: Cypher -> CypherM ()
tl = put


qLab :: Neo -> IO String
qLab n = jjshow $ getNeo n "labels" ("" :: String)

qRelType n = jjshow $ getNeo n "relationship/types" ("" :: String)


qNode :: Neo -> IO String
qNode n = jjshow $ postNeo n "node" ("" :: String)

qNeo :: Neo -> IO String
qNeo n = jjshow $ getNeo n "" ("" :: String)

qExt n = jjshow $ getNeo n "extensions_info" ("" :: String)

mCyp :: FromJSON a => Cypher -> NeoM (Maybe a)
mCyp q = ask >>= lift . flip qCyp q

jCyp :: Maybe Value -> NeoM Value
jCyp = return . fromMaybe emptyObject

-- resp = mCyp >>= jCyp . toJSON

mS :: (FromJSON a, Show a) => a -> NeoM ()
mS = lift . print

runCypher = execState

runNeoDef :: (FromJSON a, Show a ) => Neo4jConf -> Cypher -> (Cypher -> NeoM (Maybe a)) -> IO (Maybe a)
runNeoDef cf c nm = do
   n <- newNeo cf
--   let c = execState mc cc
   runReaderT ( nm c) n

runNeo :: (FromJSON a, Show a ) => Neo4jConf -> Cypher -> (Cypher -> NeoM (Maybe a)) -> IO (Maybe a)
runNeo cf c nm = do
   n <- newNeo cf
--   let c = execState mc cc
   runReaderT ( nm c) n

runNeo' :: (FromJSON a, Show a ) => Neo -> Cypher -> (Cypher -> NeoM (Maybe a)) -> IO (Maybe a)
runNeo' n c nm = runReaderT (nm c) n
