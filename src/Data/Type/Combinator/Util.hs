{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

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
  , ixVecT
  , ixV
  , _Flip
  , _C
  , sumProd
  , for1
  , Replicate
  , vecToProd
  , vecToAnyProd
  , addFinCap
  , strengthen
  , natFin
  , natFinCap
  , someNat
  , unzipV
  , fins'
  , dsumSome
  , someDSum
  , liftTraversal
  ) where

import           Control.Lens as L hiding ((:<), Index, Traversable1(..))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Dependent.Sum
import           Data.Foldable            as F
import           Data.Kind
import           Data.Maybe
import           Data.Type.Combinator
import           Data.Type.Conjunction
import           Data.Type.Fin
import           Data.Type.Index
import           Data.Type.Length
import           Data.Type.Nat
import           Data.Type.Product        as TCP
import           Data.Type.Sum
import           Data.Type.Vector         as TCV
import           GHC.Generics             (Generic)
import           Numeric.Natural
import           Text.Printf
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import           Type.Family.Constraint
import           Type.Family.List
import           Type.Family.Nat
import qualified GHC.TypeLits             as TL

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
    InR xs -> some (sumSome xs) $ \(i :&: y) ->
                Some (IS i :&: y)

someSum :: forall f as. Some (Index as :&: f) -> Sum f as
someSum = withSome (uncurryFan go)
  where
    go :: forall bs a. Index bs a -> f a -> Sum f bs
    go = \case
      IZ   -> InL
      IS i -> InR . go i

ixProd :: Index as a -> Lens' (Prod f as) (f a)
ixProd = \case
    IZ -> \f -> \case
      x :< xs -> (:< xs) <$> f x
    IS i -> \f -> \case
      x :< xs -> (x :<) <$> ixProd i f xs

ixVecT :: Fin n -> Lens' (VecT n f a) (f a)
ixVecT = \case
    FZ   -> \f -> \case
      x :* xs -> (:* xs) <$> f x
    FS n -> \f -> \case
      x :* xs -> (x :*) <$> ixVecT n f xs

ixV :: Fin n -> Lens' (Vec n a) a
ixV = \case
    FZ   -> \f -> \case
      I x :* xs -> (:+ xs) <$> f x
    FS n -> \f -> \case
      x   :* xs -> (x :*) <$> ixV n f xs

makePrisms ''Flip
makeWrapped ''Flip

makePrisms ''C
makeWrapped ''C

-- _Flip :: Iso (Flip f a b) (Flip f c d) (f b a) (f d c)
-- _Flip = iso

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

instance (Known Nat n, FromJSON a) => FromJSON (Vec n a) where
    parseJSON o = do
      xs <- parseJSON o
      case listVec known xs of
        Nothing -> fail $ printf "Bad number of items in list. (Expected %d, got %d)"
                            (natVal (known @_ @Nat @n)) (length xs)
        Just ys -> return ys
instance ToJSON a => ToJSON (Vec n a) where
    toJSON     = toJSON . F.toList
    toEncoding = toEncoding . F.toList

instance (Every (Comp FromJSON f) as, Known Length as) => FromJSON (Prod f as) where
    parseJSON = withArray "Prod f as" (go TCP.indices . F.toList)
      where
        go  :: forall bs. ()
            => Prod (Index as) bs
            -> [Value]
            -> Parser (Prod f bs)
        go = \case
          Ø -> \case
            []  -> return Ø
            _:_ -> fail "Too many items in array"
          i :< is -> \case
            []   -> fail "Too few items in array"
            x:xs -> (:<) <$> (parseJSON x \\ every @_ @(Comp FromJSON f) i)
                         <*> go is xs
instance Every (Comp ToJSON f) as => ToJSON (Prod f as) where
    toJSON = toJSON . ifoldMap1 (\i x -> [toJSON x] \\ every @_ @(Comp ToJSON f) i)
    toEncoding = toEncoding . toJSON @(Prod f as)

deriving instance Generic (Flip a b c)

instance ToJSON (f b a) => ToJSON (Flip f a b)
instance FromJSON (f b a) => FromJSON (Flip f a b)

listVec :: Nat n -> [a] -> Maybe (Vec n a)
listVec = \case
    Z_ -> \case
      [] -> Just ØV
      _  -> Nothing
    S_ n -> \case
      [] -> Nothing
      x:xs -> do
        ys <- listVec n xs
        return (x :+ ys)

for1
    :: (Applicative h, Traversable1 t)
    => t f b
    -> (forall a. f a -> h (g a))
    -> h (t g b)
for1 x f = traverse1 f x

type family Replicate (n :: N) (a :: k) = (as :: [k]) | as -> n where
    Replicate 'Z     a = '[]
    Replicate ('S n) a = a ': Replicate n a

vecToProd
    :: VecT n f a
    -> Prod f (Replicate n a)
vecToProd = \case
    ØV      -> Ø
    x :* xs -> x :< vecToProd xs

vecToAnyProd
    :: Vec n a
    -> HasLen n as
    -> Prod (C a) as
vecToAnyProd = \case
    ØV        -> \case
      HLZ -> Ø
    I x :* xs -> \case
      HLS hl -> C x :< vecToAnyProd xs hl

instance Known (HasLen 'Z) '[] where
    known = HLZ
instance Known (HasLen n) as => Known (HasLen ('S n)) (a ': as) where
    known = HLS known

addFinCap :: forall n. Known Nat n => Fin n -> Fin n -> Fin n
addFinCap FZ     m = m
addFinCap (FS n) m = natFinCap . finNat . FS $ addFinCap (weaken n :: Fin n) m

strengthenN :: Nat n -> Fin m -> Fin (n + m)
strengthenN = \case
    Z_   -> id
    S_ n -> FS . strengthenN n

strengthen :: Known Nat n => Fin ('S n) -> Maybe (Fin n)
strengthen = natFin . finNat

natFin :: forall n. Known Nat n => Some Nat -> Maybe (Fin n)
natFin = withSome (go known)
  where
    go :: forall m o. Nat m -> Nat o -> Maybe (Fin m)
    go = \case
      Z_   -> \_ -> Nothing
      S_ m -> \case
        Z_   -> Just FZ
        S_ o -> FS <$> go m o

natFinCap :: forall n. Known Nat n => Some Nat -> Fin ('S n)
natFinCap = withSome (go known)
  where
    go :: forall m o. Nat ('S m) -> Nat o -> Fin ('S m)
    go = \case
      S_ Z_ -> \_ -> FZ
      S_ (S_ m) -> \case
        Z_   -> FZ
        S_ o -> FS $ go (S_ m) o

someNat :: Natural -> Some Nat
someNat x | x <= 0    = Some Z_
          | otherwise = some (someNat (x - 1)) (Some . S_)

unzipV :: Functor f => VecT n f (a, b) -> (VecT n f a, VecT n f b)
unzipV = \case
    ØV       -> (ØV, ØV)
    xy :* zs -> case unzipV zs of
      (xs, ys) -> (fmap fst xy :* xs, fmap snd xy :* ys)

fins' :: Nat n -> Vec n (Fin n)
fins' = \case
    Z_   -> ØV
    S_ n -> FZ :+ (FS <$> fins' n)

zipFins :: forall n f a. Functor f => VecT n f a -> VecT n f (Fin n, a)
zipFins = \case
    ØV      -> ØV
    x :* xs -> ((FZ,) <$> x) :* (first FS <$> zipFins xs)

instance (FunctorWithIndex i f, Functor f) => FunctorWithIndex (Fin n, i) (VecT n f) where
    imap f = TCV.imap $ \i -> L.imap (\j x -> f (i, j) x)
instance FoldableWithIndex i f => FoldableWithIndex (Fin n, i) (VecT n f) where
    ifoldMap f = TCV.ifoldMap $ \i -> L.ifoldMap (\j x -> f (i, j) x)
instance TraversableWithIndex i f => TraversableWithIndex (Fin n, i) (VecT n f) where
    itraverse f = TCV.itraverse $ \i -> L.itraverse (\j x -> f (i, j) x)

instance FunctorWithIndex () I where
    imap f = fmap (f ())
instance FoldableWithIndex () I where
    ifoldMap f = foldMap (f ())
instance TraversableWithIndex () I where
    itraverse f = traverse (f ())

dsumSome :: DSum f g -> Some (f :&: g)
dsumSome (t :=> x) = Some (t :&: x)

someDSum :: Some (f :&: g) -> DSum f g
someDSum = withSome $ \(t :&: x) -> t :=> x

liftTraversal
    :: (Traversable1 t, Applicative f)
    => (forall a. LensLike' f (g a) b)
    -> LensLike' f (t g as) b
liftTraversal t f = traverse1 (t f)
