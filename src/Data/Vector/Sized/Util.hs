{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Data.Vector.Sized.Util (
    ixSV
  ) where

-- import           Data.Finite.Internal
import           Control.Lens
import           Data.Aeson
import           Data.Finite
import           Data.Foldable
import           GHC.TypeLits
import qualified Data.Vector               as V
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
ixSV i f v = (\x -> v VG.// [(fromIntegral i, x)]) <$> f (VG.index v i)

type instance Index   (VG.Vector v n a) = Finite n
type instance IxValue (VG.Vector v n a) = a

instance (KnownNat n, UVG.Vector v a) => Ixed (VG.Vector v n a) where
    ix = ixSV

instance FunctorWithIndex (Finite n) (VG.Vector V.Vector n) where
    imap = VG.imap
instance FoldableWithIndex (Finite n) (VG.Vector V.Vector n) where
    ifoldMap f = fold . VG.imap f
-- instance TraversableWithIndex (Finite n) (VG.Vector V.Vector n) where
--     itraverse = VG.imapM

-- instance (FunctorWithIndex i f, Functor f) => FunctorWithIndex (Fin n, i) (VecT n f) where
--     imap f = TCV.imap $ \i -> L.imap (\j x -> f (i, j) x)
-- instance FoldableWithIndex i f => FoldableWithIndex (Fin n, i) (VecT n f) where
--     ifoldMap f = TCV.ifoldMap $ \i -> L.ifoldMap (\j x -> f (i, j) x)
-- instance TraversableWithIndex i f => TraversableWithIndex (Fin n, i) (VecT n f) where
--     itraverse f = TCV.itraverse $ \i -> L.itraverse (\j x -> f (i, j) x)
