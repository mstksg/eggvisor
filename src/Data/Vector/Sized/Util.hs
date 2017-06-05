{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Vector.Sized.Util (
    ixSV
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Finite
import           Data.Finite.Internal
import           GHC.TypeLits
import qualified Data.Vector.Generic       as UVG
import qualified Data.Vector.Generic.Sized as VG

instance (FromJSON (v a), UVG.Vector v a, KnownNat n) => FromJSON (VG.Vector v n a) where
    parseJSON o = do
      v <- parseJSON o
      case VG.toSized v of
        Nothing -> fail "Wrong number of items in array"
        Just v' -> return v'

instance (ToJSON (v a), UVG.Vector v a, KnownNat n) => ToJSON (VG.Vector v n a) where
    toJSON     = toJSON     . VG.fromSized
    toEncoding = toEncoding . VG.fromSized

ixSV
    :: (KnownNat n, UVG.Vector v a)
    => Finite n -> Lens' (VG.Vector v n a) a
ixSV ix f v = (\x -> v VG.// [(fromIntegral ix, x)]) <$> f (VG.index v ix)

type instance Index   (VG.Vector v n a) = Finite n
type instance IxValue (VG.Vector v n a) = a

instance (KnownNat n, UVG.Vector v a) => Ixed (VG.Vector v n a) where
    ix = ixSV
