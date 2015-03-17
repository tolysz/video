{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Types.Lang where

data LangId
   = LangEnGB
   | LangEnUs
   | LangPl
   | LangRu
   | LangFr
   | LangDe
    deriving (Show, Eq, Typeable, Generic)

readLang :: [Text] -> LangId
readLang ((map toLower . T.unpack -> a):_) = case a of
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


