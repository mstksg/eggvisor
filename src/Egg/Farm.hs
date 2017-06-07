{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Egg.Farm (
    FarmStatus(..)
  , fsEgg, fsResearch, fsHabs, fsDepot, fsBocks, fsGoldenEggs, fsSoulEggs
  , initFarmStatus
  , farmEggValue
  , farmLayingRate
  , farmLayingRateAvailability
  , farmIncome
  , farmIncomeDT
  , farmSoulEggBonus
  , farmBonuses
  -- , waitTilDepotFull
  , stepFarm
  , stepFarmDT
  ) where

import           Control.Lens
import           Control.Monad.Trans.Writer
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Kind
import           Data.Singletons.TypeLits
import           Data.Tuple
import           Data.Type.Combinator
import           Data.Type.Fin
import           Data.Vector.Sized.Util
import           Egg.Commodity
import           Egg.Egg
import           Egg.GameData
import           Egg.Habitat
import           Egg.Research
import           Egg.Vehicle
import           Numeric.Lens
import           Numeric.Natural
import           Type.Class.Known
import           Type.Family.List
import           Type.Family.Nat
import qualified Data.Vector.Sized            as SV
import qualified GHC.TypeLits                 as TL

data FarmStatus :: Nat -> ([Nat], Nat) -> Nat -> Nat -> Type where
    FarmStatus :: { _fsEgg        :: Finite eggs
                  , _fsResearch   :: ResearchStatus tiers epic
                  , _fsHabs       :: HabStatus habs
                  , _fsDepot      :: SomeDepotStatus vehicles
                  , _fsBocks      :: Bock
                  , _fsGoldenEggs :: GoldenEgg
                  , _fsSoulEggs   :: SoulEgg
                  }
               -> FarmStatus eggs '(tiers, epic) habs vehicles

deriving instance ListC (Show <$> (Flip SV.Vector Natural <$> tiers))
    => Show (FarmStatus eggs '(tiers, epic) habs vehicles)
deriving instance ListC (Eq <$> (Flip SV.Vector Natural <$> tiers))
    => Eq (FarmStatus egg '(tiers, epic) habs vehicles)
deriving instance (ListC (Eq <$> (Flip SV.Vector Natural <$> tiers)), ListC (Ord <$> (Flip SV.Vector Natural <$> tiers)))
    => Ord (FarmStatus egg '(tiers, epic) habs vehicles)

fsEgg :: Lens (FarmStatus e1 '(t, g) h v) (FarmStatus e2 '(t, g) h v) (Finite e1) (Finite e2)
fsEgg f fs = (\e -> fs { _fsEgg = e }) <$> f (_fsEgg fs)
fsResearch :: Lens (FarmStatus e '(t1, g1) h v) (FarmStatus e '(t2, g2) h v) (ResearchStatus t1 g1) (ResearchStatus t2 g2)
fsResearch f fs = f (_fsResearch fs) <&> \r ->
    (FarmStatus <$> _fsEgg
                <*> pure r
                <*> _fsHabs
                <*> _fsDepot
                <*> _fsBocks
                <*> _fsGoldenEggs
                <*> _fsSoulEggs
    ) fs
fsHabs :: Lens (FarmStatus e '(t, g) h1 v) (FarmStatus e '(t, g) h2 v) (HabStatus h1) (HabStatus h2)
fsHabs f fs = (\h -> fs { _fsHabs = h }) <$> f (_fsHabs fs)
fsDepot :: Lens (FarmStatus e '(t, g) h v1) (FarmStatus e '(t, g) h v2) (SomeDepotStatus v1) (SomeDepotStatus v2)
fsDepot f fs = (\d -> fs { _fsDepot = d }) <$> f (_fsDepot fs)
fsBocks :: Lens' (FarmStatus e '(t, g) h v) Bock
fsBocks f fs = (\b -> fs { _fsBocks = b }) <$> f (_fsBocks fs)
fsGoldenEggs :: Lens' (FarmStatus e '(t, g) h v) GoldenEgg
fsGoldenEggs f fs = (\e -> fs { _fsGoldenEggs = e }) <$> f (_fsGoldenEggs fs)
fsSoulEggs :: Lens' (FarmStatus e '(t, g) h v) SoulEgg
fsSoulEggs f fs = (\s -> fs { _fsSoulEggs = s }) <$> f (_fsSoulEggs fs)

initFarmStatus
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles, 1 TL.<= eggs, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
initFarmStatus (GameData _ rd _ _ _) =
    FarmStatus 0
               (emptyResearchStatus rd)
               initHabStatus
               (known :=> initDepotStatus)
               0
               0
               0

-- | Egg value
farmEggValue
    :: KnownNat eggs
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmEggValue gd fs = gd ^. gdEggData
                         . _EggData
                         . ixSV (fs ^. fsEgg)
                         . eggValue
                         . bonusingFor (farmBonuses gd fs) BTEggValue

-- | Egg rate in eggs per second.
--
-- Multiplies 'farmLayingRatePerChicken' by the number of chickens, and
-- caps it based on the depot capacity.
--
-- Also gives remaining capacity of depots, with 'Nothing' if depots are
-- at full capacity.
farmLayingRate
    :: KnownNat vehicles
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmLayingRate gd fs = fst $ farmLayingRateAvailability gd fs

-- | Egg rate in eggs per second.
--
-- Multiplies 'farmLayingRatePerChicken' by the number of chickens, and
-- caps it based on the depot capacity.
--
-- Also gives remaining capacity of depots, with 'Nothing' if depots are
-- at full capacity.
farmLayingRateAvailability
    :: KnownNat vehicles
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> (Double, Maybe Double)
farmLayingRateAvailability gd fs =
    fs ^. to (farmLayingRatePerChicken gd)
        . to (* chickens)
        . to (\r -> if r > cap
                      then (cap, Nothing       )
                      else (r  , Just (cap - r))
             )
  where
    chickens :: Double
    chickens = fs ^. fsHabs . to (totalChickens (gd ^. gdHabData))
    cap :: Double
    cap = case fs ^. fsDepot of
      _ :=> d -> totalDepotCapacity (gd ^. gdVehicleData)
                                    (farmBonuses gd fs)
                                    d

-- | How many eggs are laid per second, per chicken.
farmLayingRatePerChicken
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmLayingRatePerChicken gd fs =
    gd ^. gdConstants
        . gcBaseLayingRate
        . dividing 60
        . bonusingFor (farmBonuses gd fs) BTLayingRate

-- -- | Total income (bocks per second), per chicken.
-- farmIncomePerChicken
--     :: (KnownNat eggs, KnownNat vehicles)
--     => GameData   eggs '(tiers, epic) habs vehicles
--     -> FarmStatus eggs '(tiers, epic) habs vehicles
--     -> Double
-- farmIncomePerChicken gd fs = fs ^. to (farmLayingRatePerChicken gd)
--                                  . to (* farmEggValue gd fs)
--                                  . to (* farmSoulEggBonus gd fs)

-- | Total income (bocks per second)
farmIncome
    :: (KnownNat eggs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmIncome gd fs = fs ^. to (farmLayingRate gd)
                       . to (* farmEggValue gd fs)
                       . to (* farmSoulEggBonus gd fs)

-- | Total bock income over a given small-ish period of time, assuming
-- no habs get filled up or depots get filled up in this time interval.
--
-- Time step should be small (dt^3 = 0) enough to avoid discontinuities,
-- but this function still takes into account quadratic growth due to dt^2
-- terms.
--
-- habSize(t) = initHabSize + growthRate * t
-- incomerate(t) = capped (habSize(t) * layingRate) * eggValue
--
-- Assuming capped:
--
-- incomerate(t) = (initHabSize + growthRate * t) * layingRate * eggValue
-- totalincome = [initHabSize * dt + 1/2 growthRate * dt^2] * layingRate * eggValue
--
-- Assuming uncapped:
--
-- incomerate(t) = capped (initHabSize * layingRate) * eggValue
-- totalincome = dt * capped (initHabSize * layingRate) * eggValue
--
farmIncomeDT
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double     -- ^ small time step (dt^3 = 0)
    -> Double
farmIncomeDT gd ic fs dt
    | initLaying >= cap = dt * cap * eggVal
    | otherwise         = (initHabSize * dt + growthRate * dt * dt / 2)
                            * layingRate * eggVal
  where
    bs          = farmBonuses gd fs
    layingRate  = farmLayingRatePerChicken gd fs
    eggVal      = farmEggValue   gd fs
    initHabSize = fs ^. fsHabs . to (totalChickens (gd ^. gdHabData))
    growthRate  = fs ^. fsHabs . to (totalGrowthRate (gd ^. gdHabData) bs ic)
    initLaying  = layingRate * initHabSize
    cap         = case fs ^. fsDepot of
      _ :=> d -> totalDepotCapacity (gd ^. gdVehicleData)
                                    (farmBonuses gd fs)
                                    d

-- | Soul egg bonus
farmSoulEggBonus
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmSoulEggBonus gd fs =
    gd ^. gdConstants
        . gcBaseSoulEggBonus
        . to (* (fs ^. fsSoulEggs . to fromIntegral))
        . bonusingFor (farmBonuses gd fs) BTSoulEggBonus

-- -- | Time until depots are full
-- --
-- -- TODO: increase bocks
-- waitTilDepotFull
--     :: (KnownNat habs, KnownNat vehicles)
--     => GameData   eggs '(tiers, epic) habs vehicles
--     -> IsCalm
--     -> FarmStatus eggs '(tiers, epic) habs vehicles
--     -> WaitTilRes (FarmStatus eggs '(tiers, epic) habs vehicles)
-- waitTilDepotFull gd ic fs =
--     fs &  fsHabs %%~ waitTilPop (gd ^. gdHabData) bs ic (floor chickensAtCap)
--       <&> fsBocks %~ _
--        -- & bocks
--   where
--     bs :: Bonuses
--     bs = farmBonuses gd fs
--     depotCap :: Double
--     depotCap = case fs ^. fsDepot of
--       _ :=> d -> totalDepotCapacity (gd ^. gdVehicleData) bs d
--     chickensAtCap :: Double
--     chickensAtCap = depotCap / farmLayingRatePerChicken gd fs

-- | Bonuses from a given 'GameData' and 'FarmStatus'
farmBonuses
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Bonuses
farmBonuses gd fs = totalBonuses (gd ^. gdResearchData) (fs ^. fsResearch)

-- | Step the farm over a given time interval
--
-- Should be += integral (R c(t)) dt
--   where c'(t) = rt
--
-- TODO: take into account depots filling up
stepFarm
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> Double       -- ^ time interval to step
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
stepFarm gd ic dt0 fs0 = go dt0 fs0
  where
    traverseWait
        :: HabStatus habs
        -> WriterT ((Fin N4, Double), Double) (Either HWaitError) (HabStatus habs)
    traverseWait = WriterT
                 . fmap swap
                 . waitTilNextFilled (gd ^. gdHabData) bs ic
    bs :: Bonuses
    bs = farmBonuses gd fs0
    go  :: Double
        -> FarmStatus eggs '(tiers, epic) habs vehicles
        -> FarmStatus eggs '(tiers, epic) habs vehicles
    go dt fs1 = case waitTilNextHabOrDepotFilled gd ic fs1 of
      -- growth rate constant, and depots won't change status
      NonStarter -> fs1 & fsBocks +~ (farmIncome gd fs1 * dt)
      -- growth rate constant, and depots won't change status
      NoWait     -> fs1 & fsBocks +~ (farmIncome gd fs1 * dt)
      WaitTilSuccess tFill (_, fs2)
        -- habs are growing slowly enough that nothing something changes
        | dt < tFill -> stepFarmDT gd ic dt fs1
        -- habs are growing quickly enough that something changes
        | otherwise  -> go (dt - tFill) fs2

-- | Step farm over a SMALL (infinitessimal) time step.  Assumes that
-- nothing discontinuous changes during the time inveral, including things
-- like:
--
-- 1. habs filling up
-- 2. depots filling up
--
-- Useful for stepping continuous simulations.
stepFarmDT
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> Double       -- ^ small (infinitessimal) time interval to step
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
stepFarmDT gd ic dt fs = fs
    & fsBocks +~ farmIncomeDT gd ic fs dt
    & fsHabs  %~ stepHabs (gd ^. gdHabData) (farmBonuses gd fs) ic dt

-- | Wait until bocks.
waitUntilBocks
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> Bock       -- ^ goal
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> WaitTilRes I (FarmStatus eggs '(tiers, epic) habs vehicles)
waitUntilBocks gd ic goal0 fs0 = go goal0 fs0
  where
    bs :: Bonuses
    bs = farmBonuses gd fs0
    go  :: Bock
        -> FarmStatus eggs '(tiers, epic) habs vehicles
        -> WaitTilRes I (FarmStatus eggs '(tiers, epic) habs vehicles)
    go goal fs1 = undefined

-- | Results:
--
-- 1. Hab N fills in t
-- 2. Depot fills in t
-- 3. Hab is full, depot empty
-- 4. Both hab and depot are full     -- right now 3 and 4 are the same
-- 5. No internal hatcheries => non starter
--
-- No report if depot starts as full...only if it starts out as non-full
-- and then turns full.
--
-- Tag is Nothing if depot fill event, or Just s if hab fill event with
-- slot.
waitTilNextHabOrDepotFilled
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> WaitTilRes ((,) (Maybe (Fin N4))) (FarmStatus eggs '(tiers, epic) habs vehicles)
waitTilNextHabOrDepotFilled gd ic fs0 = case runWriterT $ fsHabs traverseWait fs0 of
    Left e -> case e of
      HWENoInternalHatcheries
        | initAllFull    -> NonStarter
        | initMaxedDepot -> NonStarter
      _                  -> NoWait
    Right (fs1, ((s, tFill), rt))
      ->  case (depotAvail0, snd (farmLayingRateAvailability gd fs1)) of
            -- depot status changes
            (Just avail0, Nothing) ->
              let timeToFill = avail0 / rt
                  fs2 = fs0 & fsBocks +~ farmIncomeDT gd ic fs0 timeToFill
                            & fsHabs  . availableSpace (gd ^. gdHabData) bs . mapped -~ rt * timeToFill
              in  WaitTilSuccess timeToFill (Nothing, fs2)
            -- depot status doesn't change
            _                      -> 
              WaitTilSuccess tFill (Just s, fs1)
  where
    initAllFull    = andOf (fsHabs . to (fullHabs (gd ^. gdHabData) bs) . traverse) fs0
    initMaxedDepot = has _Nothing depotAvail0
    bs :: Bonuses
    bs = farmBonuses gd fs0
    depotAvail0 :: Maybe Double
    depotAvail0 = snd (farmLayingRateAvailability gd fs0)
    traverseWait
        :: HabStatus habs
        -> WriterT ((Fin N4, Double), Double) (Either HWaitError) (HabStatus habs)
    traverseWait = WriterT
                 . fmap swap
                 . waitTilNextFilled (gd ^. gdHabData) bs ic
