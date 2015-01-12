{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           , DeriveDataTypeable
           , GeneralizedNewtypeDeriving
           , DeriveGeneric
           , TemplateHaskell
           #-}


module Google.Api.Types.GoogleUser
 (GoogleUser (..))
 where

import Data.Aeson
import Prelude     hiding (id)
import Data.Aeson.Types
import Data.Text          (Text)

import Data.Possible
import Data.Default
import Control.Lens       (makeLenses) -- , set)
import Data.Typeable
import GHC.Generics
import Google.Api.Utils

data GoogleUser = GoogleUser
  { _googleUserId            :: Possible Text
  , _googleUserName          :: Possible Text
  , _googleUserGivenName     :: Possible Text
  , _googleUserFamilyName    :: Possible Text
  , _googleUserLink          :: Possible Text
  , _googleUserPicture       :: Possible Text
  , _googleUserGender        :: Possible Text
  , _googleUserBirthday      :: Possible Text
  , _googleUserLocale        :: Possible Text
  , _googleUserEmail         :: Possible Text
  , _googleUserVerifiedEmail :: Possible Bool
  } deriving  (Show, Typeable, Generic)

opts = defaultOptions { fieldLabelModifier = fromCamel 11 }

instance FromJSON GoogleUser where parseJSON = genericParseJSON opts
instance ToJSON GoogleUser   where toJSON    = genericToJSON    opts

instance Default GoogleUser where
  def = GoogleUser MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData

makeLenses ''GoogleUser

-- instance FromJSON GoogleUser
-- instance ToJSON GoogleUser

{-
instance FromJSON GoogleUser where
 parseJSON (Object v) =
  GoogleUser <$>  v .:?? "id"
             <*>  v .:?? "name"
             <*>  v .:?? "given_name"
             <*>  v .:?? "family_name"
             <*>  v .:?? "link"
             <*>  v .:?? "picture"
             <*>  v .:?? "gender"
             <*>  v .:?? "birthday"
             <*>  v .:?? "locale"
             <*>  v .:?? "email"
             <*>  v .:?? "verified_email"
 parseJSON _ = fail "Not a Setting object"

instance ToJSON GoogleUser where
  toJSON GoogleUser{..} = object
     [ "id"             .= googleUserId
     , "name"           .= googleUserName
     , "given_name"     .= googleUserGivenName
     , "family_name"    .= googleUserFamilyName
     , "link"           .= googleUserLink
     , "picture"        .= googleUserPicture
     , "gender"         .= googleUserGender
     , "birthday"       .= googleUserBirthday
     , "locale"         .= googleUserLocale
     , "email"          .= googleUserEmail
     , "verified_email" .= googleUserVerifiedEmail
     ]

-- $(deriveJSON defaultOptions ''User)

{--
, "id":"234324234"
, "name":"Marcin Tołysz"
, "given_name":"Marcin"
, "family_name":"Tołysz"
, "link":"https://plus.google.com/324324"
, "picture":"https://lh5.googleusercontent.com/-lAddddM/photo.jpg"
, "gender":"" ??
, "birthday":"" ??
, "email":"tolysz@gmail.com"
, "verified_email":true

--}-}
