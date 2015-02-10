{-# LANGUAGE  ViewPatterns, ScopedTypeVariables #-}

module Data.Binary.Enum where

import Prelude
import Data.Binary
import Control.Applicative
import qualified Data.List as DL

-- | we can store Enum values inside values we know how to store, so let
newtype BitMap t a = BitMap [a]
  -- ^ Map lets you encode 'flags' compressing distinct eg. packing 8 diferent flags per byte
newtype BitEnc t a = BitEnc a
          -- ^ t is a store type, you will need to make sure thats your enum fits there

instance (Enum a, Enum t, Binary t) => Binary (BitEnc t a) where
  put (BitEnc a) = put (toEnum . fromEnum $ a :: t)
  get = BitEnc . toEnum . fromEnum <$> (get :: Get t)

instance (Enum a, Eq a, Eq t, Num t, Integral t, Binary t) => Binary (BitMap t a) where
  put (BitMap a) = put ( foldr (\(fromEnum -> x) y -> 2 ^ x + y ) 0 (DL.nub a) :: t )
  get = BitMap . process 0 <$> (get :: Get t)
    where
      process _ 0    = []
      process k (flip divMod 2 -> (d, m))
        | m == 1     = toEnum k : process (k + 1) d
        | otherwise  =            process (k + 1) d
