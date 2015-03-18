{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Types.Lang where

import Prelude
import Data.Typeable
import GHC.Generics
import Control.Monad

import qualified Data.Text as T
import qualified Data.Aeson as DA

data LangId
   = LangEnGB
   | LangEnUs
   | LangPl
   | LangRu
   | LangFr
   | LangDe
    deriving (Show, Eq, Ord, Typeable, Generic)
instance DA.FromJSON LangId where
  parseJSON (DA.String a) = return $ readLang [a]
  parseJSON _             = mzero

instance DA.ToJSON LangId where
  toJSON LangEnGB = "en-GB"
  toJSON LangEnUs = "en-US"
  toJSON LangPl   = "pl"
  toJSON LangRu   = "ru"
  toJSON LangFr   = "fr-FR"
  toJSON LangDe   = "de-DE"

readLang :: [T.Text] -> LangId
readLang ((T.unpack . T.toLower -> a):_) = case a of
    "en-gb" -> LangEnGB
    "en-us" -> LangEnUs
    "de-de" -> LangDe
    "fr-fr" -> LangFr
    "en"    -> LangEnGB
    "de"    -> LangDe
    "fr"    -> LangFr
    "pl"    -> LangPl
    "ru"    -> LangRu
    _       -> LangEnGB
readLang  _ = LangEnGB
