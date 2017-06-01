{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}

module Data.Type.Combinator.Util (
    HasLen(..)
  , withProd
  , unzipP
  , zipP
  , allWit
  , allWit'
  , tagSum
  , sumIso
  , sumSome
  , someSum
  , ixProd
  , _Flip
  , sumProd
  ) where

import           Control.Lens hiding ((:<), Index)
import           Data.Kind
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Nat
import           Data.Type.Product
import           Data.Type.Sum
import           Data.Type.Vector
import           Type.Class.Higher
import           Type.Class.Witness
import           Type.Family.List
import           Type.Family.Nat

data HasLen :: N -> [k] -> Type where
    HLZ :: HasLen 'Z '[]
    HLS :: HasLen n as -> HasLen ('S n) (a ': as)

withProd
    :: forall n f g a. ()
    => (f a -> Some g)
    -> VecT n f a
    -> Some (HasLen n :&: Prod g)
withProd f = go
  where
    go :: VecT m f a -> Some (HasLen m :&: Prod g)
    go = \case
      ØV      -> Some (HLZ :&: Ø)
      x :* xs -> some (f x)   $ \y ->
                 some (go xs) $ \(hl :&: ys) ->
        Some (HLS hl :&: (y :< ys))

unzipP
    :: Prod (f :&: g) as
    -> (Prod f :&: Prod g) as
unzipP = \case
    Ø               -> Ø :&: Ø
    (x :&: y) :< zs -> case unzipP zs of
      xs :&: ys -> (x :< xs) :&: (y :< ys)

zipP
    :: (Prod f :&: Prod g) as
    -> Prod (f :&: g) as
zipP = \case
    Ø         :&: Ø         -> Ø
    (x :< xs) :&: (y :< ys) -> (x :&: y) :< zipP (xs :&: ys)

allWit'
    :: Prod (Wit :.: c) as
    -> Wit (ListC (c <$> as))
allWit' = \case
    Ø           -> Wit
    Comp Wit :< wits -> case allWit' wits of
      Wit -> Wit

allWit
    :: forall f c as. ()
    => (forall a. f a -> Wit (c a))
    -> Prod f as
    -> Wit (ListC (c <$> as))
allWit f = allWit' . map1 (Comp . f)

tagSum
    :: Prod f as
    -> Sum g as
    -> Sum (f :&: g) as
tagSum = \case
    Ø       -> \case
    x :< xs -> \case
      InL y  -> InL (x :&: y)
      InR ys -> InR (tagSum xs ys)

sumIso :: Iso' (Sum f as) (Some (Index as :&: f))
sumIso = iso sumSome someSum

sumSome :: Sum f as -> Some (Index as :&: f)
sumSome = \case
    InL x  -> Some (IZ :&: x)
    InR xs -> some (sumSome xs) $ \(ix :&: y) ->
                Some (IS ix :&: y)

someSum :: forall f as. Some (Index as :&: f) -> Sum f as
someSum = withSome (uncurryFan go)
  where
    go :: forall bs a. Index bs a -> f a -> Sum f bs
    go = \case
      IZ   -> InL
      IS i -> \x -> InR (go i x)

ixProd :: Index as a -> Lens' (Prod f as) (f a)
ixProd = \case
    IZ -> \f -> \case
      x :< xs -> (:< xs) <$> f x
    IS i -> \f -> \case
      x :< xs -> (x :<) <$> ixProd i f xs

_Flip :: Lens (Flip f a b) (Flip f c d) (f b a) (f d c)
_Flip f (Flip x) = Flip <$> f x

sumProd
    :: Functor h
    => (forall a. (f :&: g) a -> h ((f :&: g) a))
    -> (Sum f :&: Prod g) as
    -> h ((Sum f :&: Prod g) as)
sumProd f = uncurryFan $ \case
    InL x  -> \case
      y :< ys -> (InL .&. (:< ys)) <$> f (x :&: y)
    InR xs -> \case
      y :< ys -> (InR .&. (y  :<)) <$> sumProd f (xs :&: ys)

instance Field1 ((f :&: g) a) ((h :&: g) a) (f a) (h a) where
    _1 f (x :&: y) = (:&: y) <$> f x

instance Field2 ((f :&: g) a) ((f :&: h) a) (g a) (h a) where
    _2 f (x :&: y) = (x :&:) <$> f y

-- sumProd = \case
--     InL x -> \f -> \case
--       y :< ys -> (:< ys) . fanSnd <$> f (x :&: y)
--     InR xs -> \f -> \case
--       y :< ys -> (y :<) <$> sumProd xs f ys

-- sumProd :: (Sum f :&: Prod g) as -> Sum (f :&: g) as

-- sumProd :: Sum f as -> Lens' (Prod g as) (Sum (f :&: g) as)
-- sumProd = \case
--     InL x -> \f -> \case
--       y :< ys -> f (InL (x :&: y)) <&> _

-- sumProd :: Sum f as -> Lens' (Prod g as) (Some (f :&: g))
-- sumProd = \case
--     InL x -> \f -> \case
--       y :< ys -> f (Some (x :&: y)) <&> \case Some (x' :&: y') -> y' :< ys
