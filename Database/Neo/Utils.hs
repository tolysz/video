

module Database.Neo.Utils where

import Prelude
import Control.Applicative
import qualified Data.ByteString.Lazy.UTF8 as BS
import Data.Maybe
import Data.Aeson

jshow :: Value -> String
jshow = BS.toString . encode

jjshow f = fromJust <$> fmap jshow <$> f
