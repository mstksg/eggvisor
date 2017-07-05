{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Finite.Util (
  ) where

import           Data.Aeson
import           Data.Finite
import           Data.Proxy
import           GHC.TypeLits
import           Text.Printf

instance ToJSON (Finite n) where
    toJSON     = toJSON . getFinite
    toEncoding = toEncoding . getFinite
instance KnownNat n => FromJSON (Finite n) where
    parseJSON v = do
      i <- parseJSON v
      case packFinite i of
        Nothing -> fail $ printf "Integer out of range (%d not <= %d)" i (natVal (Proxy @n))
        Just f  -> return f

instance ToJSONKey (Finite n)
instance KnownNat n => FromJSONKey (Finite n)

