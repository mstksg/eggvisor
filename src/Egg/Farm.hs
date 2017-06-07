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
  , farmIncome
  , farmSoulEggBonus
  , farmBonuses
  , waitTilDepotFull
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
farmLayingRate
    :: KnownNat vehicles
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmLayingRate gd fs =
    fs ^. to (farmLayingRatePerChicken gd)
        . to (* chickens)
        . to (max cap)
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

-- | Time until depots are full
--
-- TODO: increase bocks
waitTilDepotFull
    :: (KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> WaitTilRes (FarmStatus eggs '(tiers, epic) habs vehicles)
waitTilDepotFull gd ic fs =
    fsHabs (waitTilPop (gd ^. gdHabData) bs ic (round chickensAtCap)) fs
  where
    bs :: Bonuses
    bs = farmBonuses gd fs
    depotCap :: Double
    depotCap = case fs ^. fsDepot of
      _ :=> d -> totalDepotCapacity (gd ^. gdVehicleData) bs d
    chickensAtCap :: Double
    chickensAtCap = depotCap / farmLayingRatePerChicken gd fs

-- | Bonuses from a given 'GameData' and 'FarmStatus'
farmBonuses
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Bonuses
farmBonuses gd fs = totalBonuses (gd ^. gdResearchData) (fs ^. fsResearch)

-- | Step the farm over a given time interval
--
-- TODO: properly manage quadratic bock increase
--
-- Should be += integral (R c(t)) dt
--   where c'(t) = rt
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
    go dt fs1 = case runWriterT $ fsHabs traverseWait fs1 of
      -- habs aren't growing
      Left  _                       ->
            fs1 & fsBocks +~ (farmIncome gd fs1 * dt)
      Right (fs2, ((_, tFill), rt))
        -- habs are growing but slow enough to not change in the interval
        | dt < tFill ->
            fs1 & fsHabs . availableSpace (gd ^. gdHabData) bs . mapped -~ rt * dt
                & fsBocks +~ farmIncomeDT gd ic fs1 dt
        -- habs are growing quickly enough to change in the interval
        | otherwise  -> go (dt - tFill) $
            fs2 & fsBocks +~ farmIncomeDT gd ic fs1 dt

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
    & fsHabs  %~ stepHabs (gd ^. gdHabData) (farmBonuses gd fs) ic dt
    & fsBocks +~ farmIncomeDT gd ic fs dt

-- waitUntilBocks
--     ::

--     => GameData   eggs '(tiers, epic) habs vehicles
--     -> IsCalm
--     -> FarmStatus eggs '(tiers, epic) habs vehicles
--     -> WaitTilRes (FarmStatus eggs '(tiers, epic) habs vehicles)
