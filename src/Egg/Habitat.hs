{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Egg.Habitat (
    Hab(..), habName, habBaseCapacity, habCosts
  , HabData(..), _HabData
  , SomeHabData
  , HabStatus(..), _HabStatus, hsSlots, hsPop
  , IsCalm(..), _NotCalm, _Calm
  , initHabStatus
  , habAt
  , totalChickens
  , baseHabCapacity
  , baseHabCapacities
  , totalHabCapacity
  , habCapacity
  , fullHabs
  , slotAvailability
  , availableSpace
  , availableSpaces
  , maxOutHabs
  , addChickens
  , habCapacities
  , habHistory
  , habPrice
  , upgradeHab
  , habUpgrades
  , internalHatcheryRate
  , totalGrowthRate
  , HWaitError(..), _HWENoInternalHatcheries, _HWEMaxHabCapacity
  , waitTilNextFilled
  , stepHabs
  , stepHabsDT
  , WaitTilRes(WaitTilSuccess, NoWait, NonStarter)
  , _WaitTilSuccess, _NoWait, _NonStarter
  , wtrTime, wtrRes
  , waitTilPop
  , fillTimes
  ) where

import           Control.Applicative
import           Control.Lens hiding        ((.=))
import           Control.Monad
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Type.Combinator.Util  as TC
import           Data.Type.Fin
import           Data.Type.Vector           as TCV
import           Data.Vector.Sized.Util
import           Egg.Commodity
import           Egg.Research
import           GHC.Generics               (Generic)
import           GHC.TypeLits               as TL
import           Numeric.Lens
import           Numeric.Natural
import           Text.Printf
import           Type.Family.Nat
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Vector.Sized          as SV

data Hab = Hab { _habName         :: T.Text
               , _habBaseCapacity :: Natural
               , _habCosts        :: Vec N4 Bock
               }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''Hab

newtype HabData habs = HabData { _hdHabs :: SV.Vector habs Hab }
  deriving (Show, Eq, Ord, Generic)

makePrisms ''HabData
makeWrapped ''HabData

type SomeHabData = DSum Sing HabData

data HabStatus habs
    = HabStatus { _hsSlots :: Vec N4 (Maybe (Finite habs))
                , _hsPop   :: Vec N4 Double
                }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''HabStatus

_HabStatus :: Iso' (HabStatus habs) (Vec N4 (Maybe (Finite habs), Double))
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
    parseJSON o = do
      habs <- parseJSON o
      SV.withSized habs $ \habsV ->
        return $ SNat :=> HabData habsV
instance ToJSON SomeHabData where
    toJSON = \case
        _ :=> r -> toJSON r
    toEncoding = \case
        _ :=> r -> toEncoding r
instance KnownNat habs => FromJSON (HabData habs) where
    parseJSON o = do
      habs <- parseJSON o
      case SV.toSized habs of
        Nothing    -> fail $ printf "Bad number of items in list. (Expected %d, got %d)"
                         (fromSing (SNat @habs)) (length habs)
        Just habsV -> return $ HabData habsV
instance ToJSON (HabData habs) where
    toJSON     = toJSON     . SV.fromSized . _hdHabs
    toEncoding = toEncoding . SV.fromSized . _hdHabs

data IsCalm = NotCalm | Calm
    deriving (Show, Eq, Ord, Enum)

makePrisms ''IsCalm

data WaitTilRes f a
    = WaitTilSuccess { _wtrTime :: Double, _wtrRes :: f a }
    -- | MaxPopIn       { _wtrTime :: Double }
    | NoWait
    | NonStarter
  deriving (Show, Eq, Ord, Functor)

makePrisms ''WaitTilRes
makeLenses ''WaitTilRes

data HWaitError = HWENoInternalHatcheries
                | HWEMaxHabCapacity

makePrisms ''HWaitError

-- | Initial 'HabStatus' to start off the game.
initHabStatus :: (KnownNat habs, 1 TL.<= habs) => HabStatus habs
initHabStatus = HabStatus (Just 0 :+ pure Nothing) (pure 0)

-- | Base capacity of each slot.
baseHabCapacities
    :: forall habs. KnownNat habs
    => HabData habs
    -> HabStatus habs
    -> Vec N4 Natural
baseHabCapacities hd = fmap go . _hsSlots
  where
    go = maybe 0 (\h -> hd ^. _HabData . ixSV h . habBaseCapacity)

-- | Total base capacity of all slots.
baseHabCapacity
    :: forall habs. KnownNat habs
    => HabData habs
    -> HabStatus habs
    -> Natural
baseHabCapacity HabData{..} =
    sumOf $ hsSlots
          . folded
          . folded
          . to (SV.index _hdHabs)
          . habBaseCapacity

-- | Total capacity of all hatcheries, factoring in bonuses.
totalHabCapacity
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Double
totalHabCapacity hd bs =
    sumOf $ to (baseHabCapacity hd)
          . to fromIntegral
          . bonusingFor bs BTHabCapacity

-- | Capacities of all slots, factoring in bonuses.
habCapacities
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Vec N4 Double
habCapacities hd bs = fmap ( bonusEffectFor bs BTHabCapacity
                           . fromIntegral
                           )
                    . baseHabCapacities hd

-- | Hab at the given slot
habAt :: KnownNat habs => HabData habs -> HabStatus habs -> Fin N4 -> Maybe Hab
habAt HabData{..} hs i = hs ^? hsSlots . ixV i . folded . to (SV.index _hdHabs)

-- | Total number of chickens
totalChickens
    :: HabData habs
    -> HabStatus habs
    -> Double
totalChickens HabData{..} = sumOf $ hsPop . folded

-- | How many of each hab is currently owned.  If key is not found, zero
-- purchases is implied.
habHistory :: HabStatus habs -> M.Map (Finite habs) (Fin N5)
habHistory = M.mapMaybe (natFin . someNat)
           . M.fromListWith (+)
           . toListOf (hsSlots . folded . folded . to (, 1))

-- | Get the BASE price of a given hab, if a purchase were to be made.
-- Does not check if purchase is legal (see 'upgradeHab').
--
-- Returns Nothing if four copies of this hab are already currently owned.
habPrice :: KnownNat habs => HabData habs -> HabStatus habs -> Finite habs -> Maybe Bock
habPrice hd hs hab = fmap priceOf
                   . maybe (Just FZ) TC.strengthen
                   . M.lookup hab
                   . habHistory
                   $ hs
  where
    priceOf :: Fin N4 -> Bock
    priceOf i = hd ^. _HabData . ixSV hab . habCosts . ixV i

-- | Get the actual capacity for a given hab, with given bonuses.
habCapacity
    :: Bonuses
    -> Hab
    -> Double
habCapacity bs = view $ habBaseCapacity
                      . to fromIntegral
                      . bonusingFor bs BTHabCapacity

-- | Compute the base internal hatchery rate (chickens per second per
-- hatchery) from bonuses.
internalHatcheryRate :: Bonuses -> IsCalm -> Double
internalHatcheryRate bs cm =
    0 ^. bonusingFor bs BTInternalHatchery . dividing 60 . multiplying calmBonus
  where
    calmBonus = case cm of
        NotCalm -> 1
        Calm    -> 1 ^. bonusingFor bs BTInternalHatcheryCalm

-- | Total hab growth rate, in chickens per second.
totalGrowthRate
    :: KnownNat habs
    => HabData habs
    -> Bonuses
    -> IsCalm
    -> HabStatus habs
    -> Double
totalGrowthRate hd bs cm hs =
    internalHatcheryRate bs cm
        ^. multiplying (1 + sharingRate * fromIntegral numFull)
         . multiplying (4 - fromIntegral numFull)
  where
    numFull      :: Finite 5
    numFull      = sumOf (availableSpace hd bs . to (maybe 1 (const 0))) hs
    sharingRate  :: Double
    sharingRate  = 0 ^. bonusingFor bs BTInternalHatcherySharing

-- | Purchase a hab upgrade.  Returns cost and new hab status, if
-- purchase is valid.
--
-- Purchase is invalid if purchasing a hab in a slot where a greater hab
-- is already purchased.
upgradeHab
    :: KnownNat habs
    => HabData habs
    -> Bonuses
    -> Fin N4
    -> Finite habs
    -> HabStatus habs
    -> Either (Finite habs) (Bock, HabStatus habs)
upgradeHab hd bs slot hab hs0 =
    getComp . flip (hsSlots . ixV slot) hs0 $ \s0 -> Comp $ do
      case s0 of
        Just h  | h >= hab -> Left h
        _                  -> Right ()
      -- should always be valid of the previous condition is true
      let price = fromJust $ habPrice hd hs0 hab
      return (price ^. bonusingFor bs BTBuildCosts, Just hab)

-- | Get all possible Hab upgrades
habUpgrades
    :: forall habs. (KnownNat habs)
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> VecT N4 (SV.Vector habs) (Either (Finite habs) Bock)
habUpgrades hd bs hs = hs ^. hsSlots
                           & vmap (go . getI)
  where
    hist :: M.Map (Finite habs) (Fin N5)
    hist = habHistory hs
    go :: Maybe (Finite habs) -> SV.Vector habs (Either (Finite habs) Bock)
    go i = hd ^. _HabData
               & SV.imap
                   (\j h -> do
                       case i of
                         Just i' | i' >= j -> Left i'
                         _                 -> Right ()
                       let n = fromJust . maybe (Just FZ) TC.strengthen $ M.lookup j hist
                       return $ h ^. habCosts . ixV n . bonusingFor bs BTBuildCosts
                   )

-- | Which habs are full?
fullHabs
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Vec N4 Bool
fullHabs hd bs = fmap (uncurry isFull) . view _HabStatus
  where
    isFull :: Maybe (Finite habs) -> Double -> Bool
    isFull s c = case s of
      Nothing -> True
      Just m  ->
        let totCap = hd ^. _HabData . ixSV m . to (habCapacity bs)
        in  c >= totCap

-- | Gives total avaiable space of each hab (Nothing if hab full), and also
-- the capacity of each hab.
slotAvailability
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> Vec N4 (Maybe Double, Double)
slotAvailability hd bs hs = checkAvail <$> caps <*> (hs ^. hsPop)
  where
    caps :: Vec N4 Double
    caps = habCapacities hd bs hs
    checkAvail :: Double -> Double -> (Maybe Double, Double)
    checkAvail cap pop
        | pop >= cap = (Nothing, cap)
        | otherwise  = (Just (cap - pop), cap)

-- | Traverse over the available space of each hab slot.
--
-- 'Nothing' means slot is full, @'Just' a@ means there is @a@ room left.
--
-- Will truncate the set @a@ if it's too big, making it an improper
-- traversal if @a@ is set to be too large.
--
-- Will truncate the set @a@ if it's negative (to zero), making it an
-- imporper traversal if @a@ is set to be negative.
availableSpace
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> Traversal' (HabStatus habs) (Maybe Double)
availableSpace hd bs f0 hs = (_HabStatus . traverse) (uncurry (go f0)) hs
  where
    go  :: Applicative f
        => (Maybe Double -> f (Maybe Double))
        -> Maybe (Finite habs)
        -> Double
        -> f (Maybe (Finite habs), Double)
    go f h p = f avail <&> \case
        Nothing -> (h, fromMaybe 0 cap)
        Just a' -> (h, case cap of
                         Nothing -> 0
                         Just c
                           | a' <= 0 -> c
                           | otherwise -> max 0 (c - a')
                   )
      where
        avail = case cap of
                  Nothing -> Nothing
                  Just c  | p < c     -> Just (c - p)
                          | otherwise -> Nothing
        cap = h <&> \i -> hd ^. _HabData
                              . ixSV i
                              . habBaseCapacity
                              . to  fromIntegral
                              . bonusingFor bs BTHabCapacity

-- | Traverse over the available space of each hab slot.
--
-- 'Nothing' means slot is full, @'Just' a@ means there is @a@ room left.
--
-- Will truncate the set @a@ if it's too big, making it an improper
-- lens if @a@ is set to be too large.
--
-- Will truncate the set @a@ if it's negative (to zero), making it an
-- improper lens if @a@ is set to be negative.
availableSpaces
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> Lens' (HabStatus habs) (Vec N4 (Maybe Double))
availableSpaces hd bs f0 hs = (_HabStatus) (go f0) hs
  where
    go  :: Functor f
        => (Vec N4 (Maybe Double) -> f (Vec N4 (Maybe Double)))
        -> Vec N4 (Maybe (Finite habs), Double)
        -> f (Vec N4 (Maybe (Finite habs), Double))
    go f hv = liftA3 restore (fst <$> hv) caps <$> f avails
      where
        restore h cap = \case
          Nothing -> (h, fromMaybe 0 cap)
          Just a' -> (h, case cap of
                           Nothing -> 0
                           Just c
                             | a' <= 0 -> c
                             | otherwise -> max 0 (c - a')
                     )
        (avails, caps) = unzipV $ uncurry availCap <$> hv
    availCap :: Maybe (Finite habs) -> Double -> (Maybe Double, Maybe Double)
    availCap h p = (subtract p <$> mfilter (> p) cap, cap)
      where
        cap = h <&> \i -> hd ^. _HabData
                              . ixSV i
                              . habBaseCapacity
                              . to  fromIntegral
                              . bonusingFor bs BTHabCapacity


-- | Fill up with max chickens
maxOutHabs
    :: KnownNat habs
    => HabData habs
    -> Bonuses
    -> HabStatus habs
    -> HabStatus habs
maxOutHabs hd bs = set (availableSpace hd bs) Nothing

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
    -> IsCalm
    -> HabStatus habs
    -> Either HWaitError (((Fin N4, Double), Double), HabStatus habs)
waitTilNextFilled hd bs cm hs
    | internalRate <= 0 = Left HWENoInternalHatcheries
    | otherwise         = case nextFill of
        Nothing      -> Left HWEMaxHabCapacity
        Just r@(_,t) -> Right ((r, totalRate), fillIt t)
  where
    avails :: Vec N4 (Maybe Double, Double)
    avails = slotAvailability hd bs hs
    internalRate :: Double
    internalRate = internalHatcheryRate bs cm
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
    fillIt t = hs & availableSpace hd bs . mapped -~ totalRate * t

-- | Times for each hab to be filled.
fillTimes
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> IsCalm
    -> HabStatus habs
    -> Either HWaitError (Vec N4 Double)
fillTimes hd bs cm hs0 = go space0 hs0
  where
    space0 = maybe (Just 0) (const Nothing ) . fst <$>
               slotAvailability hd bs hs0
    go  :: Vec N4 (Maybe Double)
        -> HabStatus habs
        -> Either HWaitError (Vec N4 Double)
    go ts0 hs1 = do
      (((n, t), _), hs2) <- waitTilNextFilled hd bs cm hs1
      let ts1 = ts0 & ixV n .~ Just t
      case sequence ts1 of
        Just ts -> return ts
        Nothing -> liftA2 (maybe id (+)) ts1 <$> go ts1 hs2

-- | Steps the 'HabStatus' over the given time interval.
stepHabs
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> IsCalm
    -> Double           -- ^ time interval to step
    -> HabStatus habs   -- ^ initial status
    -> HabStatus habs
stepHabs hd bs cm = go
  where
    go :: Double -> HabStatus habs -> HabStatus habs
    go dt hs0 = case waitTilNextFilled hd bs cm hs0 of
        Left  _                       -> hs0
        Right (((_, tFill), rt), hs1)
          | dt < tFill -> hs0 & availableSpace hd bs . mapped -~ rt * dt
          | otherwise  ->
                go (dt - tFill) hs1

-- | Step the 'HabStatus' over a sufficiently SMALL (infinitessimal) time
-- interval @dt@.
--
-- Assumes that a hab will not become newly filled during this time
-- interval.
--
-- Mostly useful for "continuous" simulations.  For larger time periods
-- where habs may fill up over the period, use 'stepHabs'.
stepHabsDT
    :: KnownNat habs
    => HabData habs
    -> Bonuses
    -> IsCalm
    -> Double           -- ^ small (infinitessimal) time interval to step
    -> HabStatus habs   -- ^ initial status
    -> HabStatus habs
stepHabsDT hd bs cm dt hs = hs & availableSpace hd bs . mapped -~ totalRate * dt
  where
    internalRate :: Double
    internalRate = internalHatcheryRate bs cm
    numFull      :: Natural
    numFull      = sumOf (availableSpace hd bs . to (maybe 1 (const 0))) hs
    sharingRate  :: Double
    sharingRate  = 0 ^. bonusingFor bs BTInternalHatcherySharing
    totalRate    :: Double
    totalRate    = internalRate * (1 + sharingRate * fromIntegral numFull)

-- | Time until a given population is reached.
--
-- If wait and no result, it means the result is the time until habs are
-- full.
waitTilPop
    :: forall habs. KnownNat habs
    => HabData habs
    -> Bonuses
    -> IsCalm
    -> Natural
    -> HabStatus habs
    -> WaitTilRes Maybe (HabStatus habs)
waitTilPop hd bs cm goal hs0
    | pop0 > goal' = NoWait
    | otherwise    = go pop0 hs0
  where
    goal' = fromIntegral goal
    pop0 :: Double
    pop0 = sumOf (hsPop . folded) hs0
    go :: Double -> HabStatus habs -> WaitTilRes Maybe (HabStatus habs)
    go currPop hs1 = case waitTilNextFilled hd bs cm hs1 of
      Left HWENoInternalHatcheries -> NonStarter
      Left HWEMaxHabCapacity       -> WaitTilSuccess 0 Nothing
      Right (((_, dt), rt), hs2)  ->
        let newPop = sumOf (hsPop . folded) hs2
        in  if newPop >= goal'
              then let dt'    = (goal' - currPop) / rt
                       filled = hs2 & availableSpace hd bs . mapped -~ rt * dt'
                   in  WaitTilSuccess dt' (Just filled)
              else go newPop hs2 & wtrTime +~ dt

-- | Add chickens to habs, distributing properly.  Returns any leftovers.
addChickens
    :: KnownNat habs
    => HabData habs
    -> Bonuses
    -> Double
    -> HabStatus habs
    -> (Maybe Double, HabStatus habs)
addChickens hd bs c = availableSpaces hd bs $ \avails ->
    let totAvail     = sumOf (folded . folded) avails
        newAvailPerc = 1 - c / totAvail
    in  if c >= totAvail
          then (Just (c - totAvail), pure Nothing                            )
          else (Nothing            , avails & mapped . mapped *~ newAvailPerc)
