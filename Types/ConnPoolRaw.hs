module Types.ConnPoolRaw where

import qualified Data.Pool as DP
import qualified Database.PostgreSQL.Simple as PGS (connect, withTransaction)
import qualified Database.PostgreSQL.Simple.Internal as PGS
-- import
newtype ConnPoolRaw = ConnPoolRaw (Pool PG.Connection)

createPostgresqlRawPool :: ConnectInfo -> IO ConnPoolRaw
createPostgresqlRawPool coninf = do
  ConnPoolRaw <$> DP.createPool
    (PGS.connect coninf) -- ^ create
    PGS.close            -- ^ destroy
    2                    -- ^ number of stripes
    100                  -- ^ time to keep unused
    10                   -- ^ connections per stripe


withRawDBConn :: ConnPoolRaw -> (PG.Connection -> IO a) -> IO a
withRawDBConn (ConnPoolRaw cp) cc =
 DP.withResource cp cc
