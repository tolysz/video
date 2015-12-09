module TestEr where
-- import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
-- import Network.HTTP.Client.Conduit
import Control.Monad
import           Data.Conduit
import           Data.Aeson as DA
import           Data.Conduit.Attoparsec  (sinkParser)
import           Data.Aeson.Parser        (json')
-- import           Network.HTTP.Conduit

main = do
--   manager <- newManager
  print  =<< simpleHttp "https://accounts.google.com/o/oauth2/token" -- FIXME don't hardcode, use: https://accounts.google.com/.well-known/openid-configuration
  --         value <- withManagerSettings settingsSsl $ \manager -> do
--   manager <- liftM authHttpManager $ lift getYesod
--   print  =<< htt{--}pLbs req
--               responseBody res $$+- sinkParser json'

  print "cool"