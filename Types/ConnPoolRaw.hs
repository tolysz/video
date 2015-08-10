module Types.ConnPoolRaw where

import Prelude
import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as PGS (connect, withTransaction)
import qualified Database.PostgreSQL.Simple.Internal as PGS
import Data.ByteString (ByteString)
-- import
newtype ConnectionPoolRaw = ConnPoolRaw (DP.Pool PGS.Connection)

type ConnectionString = ByteString

createPostgresqlRawPool :: ConnectionString -> IO ConnectionPoolRaw
createPostgresqlRawPool coninf = do
  ConnPoolRaw <$> DP.createPool
    (PGS.connectPostgreSQL  coninf) -- ^ create
    PGS.close            -- ^ destroy
    2                    -- ^ number of stripes
    100                  -- ^ time to keep unused
    10                   -- ^ connections per stripe


withRawDBConn :: ConnectionPoolRaw -> (PGS.Connection -> IO a) -> IO a
withRawDBConn (ConnPoolRaw cp) cc =
 DP.withResource cp cc
