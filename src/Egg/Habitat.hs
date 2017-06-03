{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Egg.Habitat (
    Hab(..), habName, habBaseCapacity, habCosts
  , HabData(..), _HabData
  , SomeHabData
  , HabStatus(..), _HabStatus, hsSlots, hsContents
  , initHabStatus
  , habAt
  , baseCapacity
  , baseCapacities
  , totalCapacity
  , habCapacity
  , fullHabs
  , availableSpace
  , capacities
  , habHistory
  , habPrice
  , upgradeHab
  , internalHatcheryRate
  , fillNext
  ) where


import           Control.Applicative
import           Control.Lens hiding        ((.=))
import           Control.Monad
import           Control.Monad.Trans.Writer
import           Data.Aeson.Types
import           Data.Bool
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Maybe
import           Data.Monoid
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Tuple
import           Data.Type.Combinator.Util
import           Data.Type.Fin
import           Data.Type.Vector           as TCV
import           Data.Vector.Sized.Util
import           Data.Yaml
import           Egg.Research
import           GHC.Generics               (Generic)
import           Numeric.Lens
import           Numeric.Natural
import           Type.Family.Nat
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Vector.Sized          as SV

data Hab = Hab { _habName         :: T.Text
               , _habBaseCapacity :: Natural
               , _habCosts        :: Vec N4 Double
               }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''Hab

newtype HabData habs = HabData { _hdHabs :: SV.Vector habs Hab }

makePrisms ''HabData
makeWrapped ''HabData

type SomeHabData = DSum Sing HabData

data HabStatus habs
    = HabStatus { _hsSlots    :: Vec N4 (S.Set (Finite habs))
                , _hsContents :: Vec N4 Natural
                }

makeLenses ''HabStatus
-- makePrisms ''HabStatus
-- makeWrapped ''HabStatus

_HabStatus :: Iso' (HabStatus habs) (Vec N4 (S.Set (Finite habs), Natural))
_HabStatus = iso (\hs -> liftA2 (,) (_hsSlots hs) (_hsContents hs))
                 (uncurry HabStatus . unzipV)

habParseOptions :: Options
habParseOptions = defaultOptions
    { fieldLabelModifier = camelTo2 '-' . drop 3
    }

instance FromJSON Hab where
    parseJSON  = genericParseJSON  habParseOptions
instance ToJSON Hab where
    toJSON     = genericToJSON     habParseOptions
    toEncoding = genericToEncoding habParseOptions

instance FromJSON SomeHabData where
    parseJSON = withObject "HabData" $ \v -> do
      habs <- v .: "habitats"
      SV.withSized habs $ \habsV ->
        return $ SNat :=> HabData habsV
instance ToJSON SomeHabData where
    toJSON = \case
        _ :=> r -> toJSON r
    toEncoding = \case
        _ :=> r -> toEncoding r
instance KnownNat habs => FromJSON (HabData habs) where
    parseJSON = withObject "HabData" $ \v -> do
      habs <- v .: "habitats"
      case SV.toSized habs of
        Nothing    -> fail "Bad number of items in list."
        Just habsV -> return $ HabData habsV
instance ToJSON (HabData habs) where
    toJSON HabData{..} = object
        [ "habitats" .= SV.fromSized _hdHabs
        ]
    toEncoding HabData{..} = pairs . mconcat $
        [ "habitats" .= SV.fromSized _hdHabs
        ]

-- | Initial 'HabStatus' to start off the game.
initHabStatus :: KnownNat habs => HabStatus habs
initHabStatus = HabStatus (S.singleton 0 :+ pure S.empty) (pure 0)

-- | Base capacity of each slot.
baseCapacities
    :: forall habs. KnownNat habs
    => HabData habs
    -> HabStatus habs
    -> Vec N4 Natural
baseCapacities hd = fmap go . _hsSlots
  where
    go = maybe 0 (\h -> hd ^. _HabData . ixSV h . habBaseCapacity)
       . lookupMax

-- | Total base capacity of all slots.
baseCapacity
    :: forall habs. KnownNat habs
    => HabData habs
    -> HabStatus habs
    -> Natural
baseCapacity HabData{..} = sumOf $ hsSlots
                                 . folded
                                 . folding lookupMax
                                 . to (SV.index _hdHabs)
                                 . habBaseCapacity

-- | Total capacity of all hatcheries, factoring in bonuses.
totalCapacity
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Natural
totalCapacity HabData{..} bs =
    sumOf $ hsSlots
          . folded
          . folding lookupMax
          . to (SV.index _hdHabs)
          . habBaseCapacity
          . to fromIntegral
          . bonusingFor bs BTHabCapacity
          . to round

-- | Capacities of all slots, factoring in bonuses.
capacities
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Vec N4 Natural
capacities hd bs = fmap ( round
                        . bonusEffectFor bs BTHabCapacity
                        . fromIntegral
                        )
                 . baseCapacities hd

-- | Hab at the given slot
habAt :: KnownNat habs => HabData habs -> HabStatus habs -> Fin N4 -> Maybe Hab
habAt HabData{..} hs i = hs ^? hsSlots . ixV i . to lookupMax . folded . to (SV.index _hdHabs)

-- | How many of each hab has been purchased so far.  If key is not found,
-- zero purchases is implied.
habHistory :: HabStatus habs -> M.Map (Finite habs) (Finite 4)
habHistory = M.fromListWith (+) . toListOf (hsSlots . folded . folded . to (, 1))

-- | Get the price of a given hab, if a purchase were to be made.  Does not
-- check if purchase is legal (see 'upgradeHab').
habPrice :: KnownNat habs => HabData habs -> HabStatus habs -> Finite habs -> Double
habPrice hd hs hab = priceOf
                   . maybe FZ (fromJust . natFin . someNat . fromIntegral)
                   . M.lookup hab
                   . habHistory
                   $ hs
  where
    priceOf :: Fin N4 -> Double
    priceOf i = hd ^. _HabData . ixSV hab . habCosts . ixV i

-- | Get the actual capacity for a given hab, with given bonuses.
habCapacity
    :: Bonuses
    -> Hab
    -> Natural
habCapacity bs = view $ habBaseCapacity
                      . to fromIntegral
                      . bonusingFor bs BTHabCapacity
                      . to round

-- | Purchase a hab upgrade.  Returns (base) cost and new hab status, if
-- purchase is valid.
--
-- Purchase is invalid if purchasing a hab in a slot where a greater hab
-- is already purchased.
upgradeHab
    :: KnownNat habs
    => HabData habs
    -> Fin N4
    -> Finite habs
    -> HabStatus habs
    -> Maybe (Double, HabStatus habs)
upgradeHab hd slot hab hs0 =
    fmap swap . runWriterT . flip (hsSlots . ixV slot) hs0 $ \s0 -> WriterT $
      let valid = case lookupMax s0 of
                    Nothing -> True
                    Just h  -> h < hab
          price = habPrice hd hs0 hab
          s1    | valid     = S.insert hab s0
                | otherwise = s0
      in  (s1, price) <$ guard valid

-- | Obsolete with containers-0.5.9, with GHC 8.2
lookupMax :: S.Set a -> Maybe a
lookupMax = fmap fst . S.maxView

-- | Which habs are full?
fullHabs
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Vec N4 Bool
fullHabs hd bs = fmap (uncurry isFull) . view _HabStatus
  where
    isFull :: S.Set (Finite habs) -> Natural -> Bool
    isFull s c = case lookupMax s of
      Nothing -> True
      Just m  -> c >= (hd ^. _HabData . ixSV m . to (habCapacity bs))

availableSpace
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Vec N4 (Maybe Natural, Natural)
availableSpace hd bs hs = checkAvail <$> caps <*> (hs ^. hsContents)
  where
    caps :: Vec N4 Natural
    caps   = capacities hd bs hs
    checkAvail :: Natural -> Natural -> (Maybe Natural, Natural)
    checkAvail cap cont | cont >= cap = (Nothing, cap)
                        | otherwise   = (Just (cap - cont), cap)

-- | Calculate the time until the next hab is full, and return the updated
-- habs after that time.  Returns Nothing if all habs are full.
--
-- Also returns the current total fill rate per hatchery, taking into
-- account all bonuses.
fillNext
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> (Maybe ((Fin N4, Double), HabStatus habs), Double)
fillNext hd bs hs = ((\m@(_,t) -> (m, fillIt t)) <$> nextFill, totalRate)
  where
    avails :: Vec N4 (Maybe Natural, Natural)
    avails = availableSpace hd bs hs
    internalRate :: Double
    internalRate = internalHatcheryRate bs
    numFull      :: Natural
    numFull      = sumOf (folded . to (maybe 1 (const 0) . fst)) avails
    totalRate    = internalRate * (1 + 0.1 * fromIntegral numFull)
    fillAtRate   :: Vec N4 (Maybe Double)
    fillAtRate = (fst <$> avails) & mapped . mapped %~ (/ totalRate) . fromIntegral
    nextFill :: Maybe (Fin N4, Double)
    nextFill = ifoldr (go . fst) Nothing fillAtRate
      where
        go :: Fin N4 -> Maybe Double -> Maybe (Fin N4, Double) -> Maybe (Fin N4, Double)
        go i (Just t) m@(Just (_, mt)) | t < mt    = Just (i, t)
                                       | otherwise = m
        go i mt      Nothing                       = (i,) <$> mt
        go _ Nothing m@(Just _)                    = m
    fillIt :: Double -> HabStatus habs
    fillIt t = hs & hsContents %~
      liftA2 (\(_, cap) cont -> min (round (fromIntegral cont + totalRate * t)) cap)
        avails

fillHabs
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> Double
    -> HabStatus habs
    -> HabStatus habs
fillHabs hd bs = go
  where
    go :: Double -> HabStatus habs -> HabStatus habs
    go dt hs0 = case fillNext hd bs hs0 of
      (Nothing               , _ ) -> hs0
      (Just ((_, tFill), hs1), rt)
        | dt < tFill -> hs0 & hsContents . mapped +~ round (rt * dt)
        | otherwise  -> go (dt - tFill) hs1

-- | Compute the base internal hatchery rate (chickens per second per
-- hatchery) from bonuses.
internalHatcheryRate :: Bonuses -> Double
internalHatcheryRate bs = 0 ^. bonusingFor bs BTInternalHatchery . dividing 60
