{-# LANGUAGE DataKinds         #-}

module Data.Vector.Sized.Util (
  ) where

import           Data.Aeson
import           GHC.TypeLits
import qualified Data.Vector.Generic.Sized as VG
import qualified Data.Vector.Generic as UVG

instance (FromJSON (v a), UVG.Vector v a, KnownNat n) => FromJSON (VG.Vector v n a) where
    parseJSON o = do
      v <- parseJSON o
      case VG.toSized v of
        Nothing -> fail "Wrong number of items in array"
        Just v' -> return v'



