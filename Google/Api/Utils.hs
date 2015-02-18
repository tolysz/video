module Google.Api.Utils where

import Prelude
import Data.Char (toLower, isUpper)

import Data.Bool
import Data.Aeson.Types

optsL :: Int -> Options
optsL i = defaultOptions { fieldLabelModifier = dropL i }

fromCamel :: Int -> String -> String
fromCamel n = worker True . drop n
  where
   worker _     []     = []
   worker lastUp (c:cs) =
        bool
           (bool [c] ['_' , toLower c] (isUpper c))
           [toLower c]
           lastUp
            ++ worker (isUpper c) cs

toCamel1 :: Int -> String -> String
toCamel1 n = worker True . drop n
  where
   worker _     []     = []
   worker True  (c:cs) = toLower c : worker False cs
   worker False (c:cs) = c : worker False cs

dropL :: Int -> String -> String
dropL n = worker . drop n
  where
   worker []     = []
   worker (c:cs) = toLower c : cs
