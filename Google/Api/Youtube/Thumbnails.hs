{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DeriveDataTypeable    #-}

module Google.Api.Youtube.Thumbnails where

 -- https://developers.google.com/youtube/v3/docs/#Thumbnails
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH

{-
{
  "default": {
    "url": string,
    "width": unsigned integer,
    "height": unsigned integer
  },
  "medium": {
    "url": string,
    "width": unsigned integer,
    "height": unsigned integer
  },
  "high": {
    "url": string,
    "width": unsigned integer,
    "height": unsigned integer
  }
}
-}