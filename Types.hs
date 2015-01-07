{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances   #-}

module Types where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Char (toLower, isUpper)
import Data.Bool
import Prelude -- (drop, (.), (++), String)
import Data.Typeable
import GHC.Generics
import Data.Aeson.TH
import Data.Aeson
import Yesod.Core.Content
import Data.String

data OAuth2Google = OAuth2Google
  { gaClientId            :: Text
  , gaClientSecret        :: Text
  , gaClientEmail         :: Maybe Text
  , gaClientX509CertUrl   :: Maybe Text
  } deriving (Show, Eq, Typeable, Generic)

instance FromJSON OAuth2Google where
  parseJSON = genericParseJSON
     (defaultOptions
      { fieldLabelModifier = fromCamel 2
      })

fromCamel :: Int -> String -> String
fromCamel n = worker True . drop n
  where
   worker _     []     = []
   worker lastUp (c:cs) =
        (bool
           (bool [c] (['_' , (toLower c)]) (isUpper c))
           [toLower c]
           lastUp
           ) ++ (worker (isUpper c) cs)

data TC a = TC a

instance (ToJSON a) => ToContent (TC a) where
  toContent (TC a) = toContent $ toJSON a

instance (ToJSON a) => ToTypedContent (TC a) where
  toTypedContent a = TypedContent typeJson (toContent a )

