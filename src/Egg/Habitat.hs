{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
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
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Egg.Habitat (
    Hab(..), habName, habBaseCapacity, habCosts
  , HabData(..), _HabData
  , SomeHabData
  , HabStatus(..), _HabStatus, hsSlots, hsPop
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
  , WaitError(..), _WENoInternalHatcheries, _WEMaxHabCapacity
  , waitTilNextFilled
  , stepHabs
  , WaitTilRes(WaitTilSuccess, MaxHabPopIn, NoWait, NoInternalHatcheries)
  , _WaitTilSuccess, _MaxHabPopIn, _NoWait, _NoInternalHatcheries
  , wtrRes, wtrStatus
  , waitTilPop
  , fillTimes
  ) where

import           Control.Applicative
import           Control.Lens hiding        ((.=))
import           Control.Monad
import           Control.Monad.Trans.Writer
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Tuple
import           Data.Type.Combinator.Util
import           Data.Type.Fin
import           Data.Type.Vector           as TCV
import           Data.Vector.Sized.Util
import           Egg.Research
import           GHC.Generics               (Generic)
import           Numeric.Lens
import           Numeric.Natural
import           Text.Printf
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
  deriving (Show, Eq, Ord, Generic)

makePrisms ''HabData
makeWrapped ''HabData

type SomeHabData = DSum Sing HabData

data HabStatus habs
    = HabStatus { _hsSlots :: Vec N4 (S.Set (Finite habs))
                , _hsPop   :: Vec N4 Double
                }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''HabStatus
-- makePrisms ''HabStatus
-- makeWrapped ''HabStatus

_HabStatus :: Iso' (HabStatus habs) (Vec N4 (S.Set (Finite habs), Double))
_HabStatus = iso (\hs -> liftA2 (,) (_hsSlots hs) (_hsPop hs))
                 (uncurry HabStatus . unzipV)

habParseOptions :: Options
habParseOptions = defaultOptions
    { fieldLabelModifier = camelTo2 '-' . drop 4
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
        Nothing    -> fail $ printf "Bad number of items in list. (Expected %d, got %d)"
                         (fromSing (SNat @habs)) (length habs)
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

-- | Compute the base internal hatchery rate (chickens per second per
-- hatchery) from bonuses.
internalHatcheryRate :: Bonuses -> Double
internalHatcheryRate bs = 0 ^. bonusingFor bs BTInternalHatchery . dividing 60

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
    isFull :: S.Set (Finite habs) -> Double -> Bool
    isFull s c = case lookupMax s of
      Nothing -> True
      Just m  ->
        let totCap = hd ^. _HabData . ixSV m . to (habCapacity bs)
        in  c >= fromIntegral totCap

-- | Gives total avaiable space of each hab (Nothing if hab full), and also
-- the capacity of each hab.
availableSpace
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Vec N4 (Maybe Double, Natural)
availableSpace hd bs hs = checkAvail <$> caps <*> (hs ^. hsPop)
  where
    caps :: Vec N4 Natural
    caps   = capacities hd bs hs
    checkAvail :: Natural -> Double -> (Maybe Double, Natural)
    checkAvail cap pop
        | pop >= cap' = (Nothing, cap)
        | otherwise   = (Just (cap' - pop), cap)
      where
        cap' = fromIntegral cap

-- -- | Calculate the time until the next hab is full, and return the updated
-- -- habs after that time.  Returns Nothing if all habs are full.
-- waitTilNextFilled
--     :: forall habs. KnownNat habs
--     => HabData habs
--     -> Bonuses
--     -> HabStatus habs
--     -> WaitResult habs (Fin N4, Double)
-- waitTilNextFilled hd bs = fst . waitTilNextFilled' hd bs

data WaitError = WENoInternalHatcheries
               | WEMaxHabCapacity

makePrisms ''WaitError

-- | Calculate the time until the next hab is full, and return the updated
-- habs after that time.  Returns Nothing if all habs are full.
--
-- Also returns the current total fill rate per hatchery, taking into
-- account all bonuses.
--
-- Conclusions:
--
-- 1. No internal hatcheries
-- 2. All habs are already full.
-- 3. Hab n filled after a given amount of time.
waitTilNextFilled
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Either WaitError ((Fin N4, Double), (HabStatus habs, Double))
waitTilNextFilled hd bs hs
    | internalRate <= 0 = Left WENoInternalHatcheries
    | otherwise         = case nextFill of
        Nothing      -> Left WEMaxHabCapacity
        Just r@(_,t) -> Right (r, (fillIt t, totalRate))
  where
    avails :: Vec N4 (Maybe Double, Natural)
    avails = availableSpace hd bs hs
    internalRate :: Double
    internalRate = internalHatcheryRate bs
    numFull      :: Natural
    numFull      = sumOf (folded . to (maybe 1 (const 0) . fst)) avails
    sharingRate  :: Double
    sharingRate  = 0 ^. bonusingFor bs BTInternalHatcherySharing
    totalRate    :: Double
    totalRate    = internalRate * (1 + sharingRate * fromIntegral numFull)
    fillAtRate   :: Vec N4 (Maybe Double)
    fillAtRate = (fst <$> avails) & mapped . mapped %~ (/ totalRate)
    nextFill :: Maybe (Fin N4, Double)
    nextFill = ifoldr (go . fst) Nothing fillAtRate
      where
        go  :: Fin N4
            -> Maybe Double
            -> Maybe (Fin N4, Double)
            -> Maybe (Fin N4, Double)
        go i (Just t) m@(Just (_, mt)) | t < mt    = Just (i, t)
                                       | otherwise = m
        go i mt      Nothing                       = (i,) <$> mt
        go _ Nothing m@(Just _)                    = m
    fillIt :: Double -> HabStatus habs
    fillIt t = hs & hsPop %~
      liftA2 (\(a, _) -> maybe id (const (+ totalRate * t)) a) avails

-- | Times for each hab to be filled.
fillTimes
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Either WaitError (Vec N4 Double)
fillTimes hd bs hs0 = go space0 hs0
  where
    space0 = maybe (Just 0) (const Nothing ) . fst <$>
               availableSpace hd bs hs0
    go  :: Vec N4 (Maybe Double)
        -> HabStatus habs
        -> Either WaitError (Vec N4 Double)
    go ts0 hs1 = do
      ((n, t), (hs2, _)) <- waitTilNextFilled hd bs hs1
      let ts1 = ts0 & ixV n .~ Just t
      case sequence ts1 of
        Just ts -> return ts
        Nothing -> liftA2 (maybe id (+)) ts1 <$> go ts1 hs2

-- | Steps the 'HabStatus' over the given time interval.
stepHabs
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> Double           -- ^ time interval to step
    -> HabStatus habs   -- ^ initial status
    -> HabStatus habs
stepHabs hd bs = go
  where
    go :: Double -> HabStatus habs -> HabStatus habs
    go dt hs0 = case waitTilNextFilled hd bs hs0 of
        Left  _                       -> hs0
        Right ((_, tFill), (hs1, rt))
          | dt < tFill -> hs0 & hsPop %~
                liftA2 (\(a, _) -> maybe id (const (+ rt * dt)) a) avails
          | otherwise  ->
                go (dt - tFill) hs1
      where
        avails = availableSpace hd bs hs0

data WaitTilRes habs
    = WaitTilSuccess { _wtrRes :: Double, _wtrStatus :: HabStatus habs }
    | MaxHabPopIn    { _wtrRes :: Double }
    | NoWait
    | NoInternalHatcheries
  deriving (Show, Eq, Ord)

makePrisms ''WaitTilRes
makeLenses ''WaitTilRes

-- | Time until a given population is reached.
waitTilPop
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> Natural
    -> HabStatus habs
    -> WaitTilRes habs
waitTilPop hd bs goal hs0
    | pop0 > goal' = NoWait
    | otherwise    = go pop0 hs0
  where
    goal' = fromIntegral goal
    pop0 :: Double
    pop0 = sumOf (hsPop . traverse) hs0
    go :: Double -> HabStatus habs -> WaitTilRes habs
    go currPop hs1 = case waitTilNextFilled hd bs hs1 of
      Left WENoInternalHatcheries -> NoInternalHatcheries
      Left WEMaxHabCapacity       -> MaxHabPopIn 0
      Right ((_, dt), (hs2, rt))    ->
        let newPop = sumOf (hsPop . traversed) hs2
            avails = availableSpace hd bs hs1
        in  if newPop >= goal'
              then let dt'    = (goal' - currPop) / rt
                       filled = hs2 & hsPop %~
                         liftA2 (\(a, _) -> maybe id (const (+ rt * dt')) a) avails
                   in  WaitTilSuccess dt' filled
              else go newPop hs2 & wtrRes +~ dt
