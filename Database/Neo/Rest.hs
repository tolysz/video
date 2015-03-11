
{-# Language OverloadedStrings
  , RecordWildCards
  , RankNTypes
  , TupleSections
  , DeriveGeneric
  #-}

module Database.Neo.Rest where

import Network.HTTP.Client
import Network.HTTP.Types.Method
import qualified Data.Aeson as DA
import qualified Data.Aeson.Types as DA
-- import qualified Data.Aeson.Generic as DAG
import Data.Monoid
import Control.Applicative
import Data.Default
import Data.String
import Data.Maybe
import Prelude
import GHC.Generics
import qualified Data.Text.Encoding as DTE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

data Neo = Neo { manager  :: Manager
               , conf     :: Neo4jConf
--                , baseHost :: String -- proto + port + base path/
--                , auth     :: Maybe BS.ByteString
               }

data Neo4jConf = Neo4jConf
  { ncHost :: String
  , ncAuth :: Maybe BS.ByteString
  } deriving (Show, Generic)

instance DA.FromJSON Neo4jConf where
  parseJSON = DA.withObject "NeoConf" $ \o ->
      Neo4jConf <$> o DA..:? "host" DA..!= "http://localhost:7474/db/data/"
                <*> (fmap DTE.encodeUtf8 <$> o DA..:? "auth")
instance DA.ToJSON Neo4jConf where
  toJSON Neo4jConf {..}= DA.object [ "host" DA..= DA.toJSON ncHost
                                   , "auth" DA..= DA.toJSON (DTE.decodeUtf8 <$> ncAuth)
                                   ]

type NEO = (DA.ToJSON a, DA.FromJSON b) => Neo -> String -> a -> IO (Maybe b)

-- newtype ServerURL = ServerURL String
-- instance Default ServerURL where
--   def = "http://localhost:7474/db/data/"

-- instance IsString ServerURL where
--   fromString = ServerURL


-- | Helper to get a new Manager
newNeo :: Neo4jConf -> IO Neo
newNeo c  = do
        m <- newManager defaultManagerSettings
        return $ Neo m c
-- |
endNeo :: Neo -> IO ()
endNeo Neo{..} = closeManager manager


postNeo :: NEO
postNeo = methodNeo "POST"

putNeo :: NEO
putNeo = methodNeo "PUT"

getNeo :: NEO
getNeo = methodNeo "GET"

methodNeo :: Method -> NEO
methodNeo m Neo{..} ep j = do
   initReq <- parseUrl $ ncHost conf <> ep
   let req = initReq 
              { method = m
              , requestHeaders = [ ("Accept","application/json ; charset=UTF-8")
                                 , ("Content-Type", "application/json")
                                 , ("X-Stream", "true")
                                 ] ++ maybe [] (\a -> [( "Authorization", a)]) (ncAuth conf)
              , requestBody = RequestBodyLBS $ DA.encode j
              }
   withResponse req manager $ \rb' -> DA.decode . BSL.fromChunks <$> brConsume (responseBody rb')
{-# inline methodNeo #-}
