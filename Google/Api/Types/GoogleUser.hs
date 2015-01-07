{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}



module Google.Api.Types.GoogleUser where

import           Data.Aeson -- .TH                 (defaultOptions, deriveJSON)
import           Data.Text                     (Text)
import           Prelude                       hiding (id)
import qualified Prelude                       as P (id)

import Data.Possible
import Data.Default
import Control.Applicative

data GoogleUser = GoogleUser
  { googleUserId         :: Possible Text
  , googleUserName       :: Possible Text
  , googleUserGivenName  :: Possible Text
  , googleUserFamilyName :: Possible Text
  , googleUserLink       :: Possible Text
  , googleUserPicture    :: Possible Text
  , googleUserGender     :: Possible Text
  , googleUserBirthday   :: Possible Text
  , googleUserLocale     :: Possible Text
  , googleUserEmail      :: Possible Text
  , googleUserVerifiedEmail :: Possible Bool
  } deriving (Show)

instance Default GoogleUser where
  def = GoogleUser MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData MissingData

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
     [ "id"          .= googleUserId
     , "name"        .= googleUserName
     , "given_name"  .= googleUserGivenName
     , "family_name" .= googleUserFamilyName
     , "link"        .= googleUserLink
     , "picture"     .= googleUserPicture
     , "gender"      .= googleUserGender
     , "birthday"    .= googleUserBirthday
     , "locale"      .= googleUserLocale
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

--}