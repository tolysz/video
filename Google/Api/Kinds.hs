
module Google.Api.Kinds where

import Data.Text (Text)
class ApiKind a where
  apiKind :: a -> Text
