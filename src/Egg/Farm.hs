{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Egg.Farm (
    FarmStatus(..)
  , fsEgg, fsResearch, fsHabs, fsDepot, fsBocks, fsGoldenEggs, fsSoulEggs
  , fsVideoBonus, fsPrestEarnings
  , IncomeChange(..), _ICHabFill, _ICDepotFill, _ICVideoDoublerExpire
  , UpgradeError(..), _UELocked, _UERegression
  , initFarmStatus
  , farmEggValue
  , farmLayingRate
  , farmLayingRatePerChicken
  , farmLayingRateAvailability
  , farmIncome
  , farmIncomeDT
  , farmIncomeDTInv
  , farmDepotCapacity
  , farmValue
  , bonusingSoulEggs
  , videoDoubling
  , farmBonuses
  , stepFarm
  , stepFarmDT
  , waitUntilBocks
  , waitTilFarmPop
  , waitTilDepotFull
  , upgradeEgg
  , eggUpgrades
  , eggsVisible
  , prestigeFarm
  , watchVideo
  ) where

import           Control.Lens
import           Control.Monad
import           Data.Finite
import           Data.Functor
import           Data.Kind
import           Data.Semigroup
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Type.Combinator.Util
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
import           Type.Family.List
import           Type.Family.Nat
import qualified Data.Vector.Sized          as SV
import qualified GHC.TypeLits               as TL

data FarmStatus :: Nat -> ([Nat], Nat) -> Nat -> Nat -> Type where
    FarmStatus :: { _fsEgg          :: Finite eggs
                  , _fsResearch     :: ResearchStatus tiers epic
                  , _fsHabs         :: HabStatus habs
                  , _fsDepot        :: SomeDepotStatus vehicles
                  , _fsBocks        :: Bock
                  , _fsGoldenEggs   :: GoldenEgg
                  , _fsSoulEggs     :: SoulEgg
                  , _fsVideoBonus   :: Maybe Double   -- ^ in seconds
                  , _fsPrestEarnings :: Double
                  }
               -> FarmStatus eggs '(tiers, epic) habs vehicles

deriving instance ListC (Show <$> (Flip SV.Vector Natural <$> tiers))
    => Show (FarmStatus eggs '(tiers, epic) habs vehicles)
deriving instance ListC (Eq <$> (Flip SV.Vector Natural <$> tiers))
    => Eq (FarmStatus egg '(tiers, epic) habs vehicles)
deriving instance (ListC (Eq <$> (Flip SV.Vector Natural <$> tiers)), ListC (Ord <$> (Flip SV.Vector Natural <$> tiers)))
    => Ord (FarmStatus egg '(tiers, epic) habs vehicles)

data IncomeChange
       = ICHabFill (Fin N4)
       | ICDepotFill
       | ICVideoDoublerExpire
  deriving (Show, Eq, Ord)

makePrisms ''IncomeChange

data UpgradeError = UELocked
                  | UERegression
  deriving (Show, Eq, Ord)

makePrisms ''UpgradeError

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
                <*> _fsVideoBonus
                <*> _fsPrestEarnings
    ) fs
fsHabs :: Lens (FarmStatus e '(t, g) h1 v) (FarmStatus e '(t, g) h2 v) (HabStatus h1) (HabStatus h2)
fsHabs f fs = (\h -> fs { _fsHabs = h }) <$> f (_fsHabs fs)
fsDepot :: Lens (FarmStatus e '(t, g) h v1) (FarmStatus e '(t, g) h v2) (SomeDepotStatus v1) (SomeDepotStatus v2)
fsDepot f fs = (\d -> fs { _fsDepot = d }) <$> f (_fsDepot fs)
fsBocks :: Lens' (FarmStatus e '(t, g) h v) Bock
fsBocks f fs = (\b -> fs { _fsBocks = max 0 b }) <$> f (_fsBocks fs)
fsGoldenEggs :: Lens' (FarmStatus e '(t, g) h v) GoldenEgg
fsGoldenEggs f fs = (\e -> fs { _fsGoldenEggs = max 0 e }) <$> f (_fsGoldenEggs fs)
fsSoulEggs :: Lens' (FarmStatus e '(t, g) h v) SoulEgg
fsSoulEggs f fs = (\s -> fs { _fsSoulEggs = max 0 s }) <$> f (_fsSoulEggs fs)
fsPrestEarnings :: Lens' (FarmStatus e '(t, g) h v) Double
fsPrestEarnings f fs = (\pe -> fs { _fsPrestEarnings = max 0 pe }) <$> f (_fsPrestEarnings fs)

-- | Only for incrementing
fsBocksAndPrest :: Traversal' (FarmStatus e '(t, g) h v) Double
fsBocksAndPrest f fs = (\b p -> fs { _fsBocks = max 0 b, _fsPrestEarnings = max 0 p })
    <$> f (_fsBocks fs)
    <*> f (_fsPrestEarnings fs)

-- | Enforces only postive Just
fsVideoBonus :: Lens' (FarmStatus e '(t, g) h v) (Maybe Double)
fsVideoBonus f fs = (\vb -> fs { _fsVideoBonus = mfilter (> 0) vb }) <$> f (_fsVideoBonus fs)



initFarmStatus
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles, 1 TL.<= eggs, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
initFarmStatus (GameData _ rd _ _ _) =
    FarmStatus 0
               (emptyResearchStatus rd)
               initHabStatus
               initSomeDepotStatus
               0
               0
               0
               Nothing
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
    bs = farmBonuses gd fs
    chickens :: Double
    chickens = fs ^. fsHabs . to (totalChickens (gd ^. gdHabData))
    cap :: Double
    cap = withSomeDepotStatus bs (fs ^. fsDepot) $
            totalDepotCapacity (gd ^. gdVehicleData) bs

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

-- | Apply video doubling bonus
videoDoubling
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Iso' Double Double
videoDoubling gd fs = case fs ^. fsVideoBonus of
    Just _  -> multiplying $ gd ^. gdConstants . gcVideoBonus
    Nothing -> id

-- | Total income (bocks per second)
farmIncome
    :: (KnownNat eggs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmIncome gd fs = fs ^. to (farmLayingRate gd)
                       . to (* farmEggValue gd fs)
                       . bonusingSoulEggs gd fs
                       . videoDoubling gd fs

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
-- incomerate(t) = (initHabSize + growthRate * t) * layingRate * eggValue * bonuses
-- totalincome = [initHabSize * dt + 1/2 growthRate * dt^2] * layingRate * eggValue * bonuses
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
    | initLaying >= cap = (dt * cap * eggVal) ^. bonusingSoulEggs gd fs
                                               . videoDoubling gd fs
    | otherwise         = (initHabSize * dt + growthRate * dt * dt / 2)
                            ^. to (* (layingRate * eggVal))
                             . bonusingSoulEggs gd fs
                             . videoDoubling gd fs

  where
    layingRate  = farmLayingRatePerChicken gd fs
    eggVal      = farmEggValue gd fs
    initHabSize = fs ^. fsHabs
                      . to (totalChickens (gd ^. gdHabData))
    growthRate  = fs ^. fsHabs
                      . to (totalGrowthRate (gd ^. gdHabData) (farmBonuses gd fs) ic)
    initLaying  = layingRate * initHabSize
    cap         = fs ^. to (farmDepotCapacity gd)

-- | Total depot capacity of farm, in eggs per second
farmDepotCapacity
    :: KnownNat vehicles
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmDepotCapacity gd fs = withSomeDepotStatus bs (fs ^. fsDepot) $
    totalDepotCapacity (gd ^. gdVehicleData) bs
  where
    bs = farmBonuses gd fs


-- | Find the time to the given goal, assuming constant growth rate and no
-- discontinuities.
--
-- Assuming uncapped:
--
-- totalincome = [initHabSize * dt + 1/2 growthRate * dt^2] * layingRate * eggValue * bonuses
--
-- so we hav:
--
-- 1/2 growthRate dt^2 + initHabSize dt - totalincome / (layingRate * eggValue * bonuses) = 0
--
-- Assuming capped:
--
-- totalincome = capped (initHabSize dt * layingRate) * evvGalue * bonuses
--
-- so:
--
-- dt = totalincome / capped rate
--
-- Assumes egg value is not zero.
--
farmIncomeDTInv
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Bock     -- ^ bock goal
    -> Maybe Double
farmIncomeDTInv gd ic fs goal
    | initLaying <= 0   = Nothing
    | initLaying >= cap = Just $
        dBock / (cap * eggVal ^. bonusingSoulEggs gd fs
                               . videoDoubling gd fs
                )
    | otherwise         =
        let c = dBock ^. dividing (layingRate * eggVal)
                       . from (bonusingSoulEggs gd fs)
                       . from (videoDoubling gd fs)
                       . negated
        in  quadratic (growthRate / 2) initHabSize c
  where
    curr        = fs ^. fsBocks
    dBock       = goal - curr
    layingRate  = farmLayingRatePerChicken gd fs
    eggVal      = farmEggValue gd fs
    initHabSize = fs ^. fsHabs . to (totalChickens (gd ^. gdHabData))
    growthRate  = fs ^. fsHabs
                      . to (totalGrowthRate (gd ^. gdHabData) (farmBonuses gd fs) ic)
    initLaying  = layingRate * initHabSize
    cap         = fs ^. to (farmDepotCapacity gd)

-- | Soul egg bonus
bonusingSoulEggs
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Iso' Double Double
bonusingSoulEggs gd fs = multiplying $
    1 + (bonus * fromIntegral (fs ^. fsSoulEggs)) / 100
  where
    bonus = gd ^. gdConstants
                . gcBaseSoulEggBonus
                . bonusingFor (farmBonuses gd fs) BTSoulEggBonus

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
    go  :: Double
        -> FarmStatus eggs '(tiers, epic) habs vehicles
        -> FarmStatus eggs '(tiers, epic) habs vehicles
    go dt fs1 = case waitTilNextIncomeChange gd ic fs1 of
      WaitTilSuccess tFill (_, fs2)
        -- habs are growing slowly enough that nothing something changes
        | dt < tFill -> stepFarmDT gd ic dt fs1
        -- habs are growing quickly enough that something changes
        | otherwise  -> go (dt - tFill) fs2
      -- growth rate constant, and depots won't change status
      _ -> stepFarmDT gd ic dt fs1

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
    & fsBocksAndPrest       +~ farmIncomeDT gd ic fs dt
    & fsHabs                %~ stepHabs (gd ^. gdHabData) (farmBonuses gd fs) ic dt
    & fsVideoBonus . mapped -~ dt

-- | Wait until bocks.
waitUntilBocks
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> Bock       -- ^ goal
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> WaitTilRes I (FarmStatus eggs '(tiers, epic) habs vehicles)
waitUntilBocks gd ic goal fs0
    | fs0 ^. fsBocks >= goal = NoWait
    | otherwise              = go fs0
  where
    go  :: FarmStatus eggs '(tiers, epic) habs vehicles
        -> WaitTilRes I (FarmStatus eggs '(tiers, epic) habs vehicles)
    go fs1 = case waitTilNextIncomeChange gd ic fs1 of
        WaitTilSuccess tFill (_, fs2)
          -- change, and goal is still out of reach
          | fs2 ^. fsBocks > goal -> go fs2 & wtrTime +~ tFill
        _ -- change or no change, and goal happens before then
          | otherwise       ->
              let dt = farmIncomeDTInv gd ic fs1 goal
              in  case dt of
                    Nothing -> NonStarter
                    Just t  -> WaitTilSuccess t (I (stepFarmDT gd ic t fs1))

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
waitTilNextIncomeChange
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> WaitTilRes ((,) IncomeChange) (FarmStatus eggs '(tiers, epic) habs vehicles)
waitTilNextIncomeChange gd ic fs0 = case getComp $ fsHabs traverseWait fs0 of
    Left e -> case e of
      HWENoInternalHatcheries
        | initAllFull    -> NonStarter
        | initMaxedDepot -> NonStarter
      _                  -> NoWait
    Right (((s, tFill), rt), fs1)
      -> let depotStatChangeIn = do
               avail0 <- depotAvail0
               case snd (farmLayingRateAvailability gd fs1) of
                 Nothing -> return . Min $ Arg (avail0 / rt) ICDepotFill
                 Just _  -> Nothing
             videoDoublerChangeIn =
               fs0 ^. fsVideoBonus
                    & mapped %~ \x -> Min (Arg x ICVideoDoublerExpire)
         in  case depotStatChangeIn <> videoDoublerChangeIn of
               Just (Min (Arg tChange ich)) ->
                 let fs2 = fs0 & fsBocksAndPrest       +~ farmIncomeDT gd ic fs0 tChange
                               & fsHabs  . availableSpace (gd ^. gdHabData) bs . mapped -~ rt * tChange
                               & fsVideoBonus . mapped -~ tChange
                 in  WaitTilSuccess tChange (ich, fs2)
               Nothing                      ->
                 let fs2 = fs0 & fsBocksAndPrest       +~ farmIncomeDT gd ic fs0 tFill
                               & fsHabs                .~ (fs1 ^. fsHabs)
                               & fsVideoBonus . mapped -~ tFill
                 in  WaitTilSuccess tFill (ICHabFill s, fs2)
  where
    initAllFull    = andOf (fsHabs . to (fullHabs (gd ^. gdHabData) bs) . folded) fs0
    initMaxedDepot = has _Nothing depotAvail0
    bs :: Bonuses
    bs = farmBonuses gd fs0
    depotAvail0 :: Maybe Double
    depotAvail0 = snd (farmLayingRateAvailability gd fs0)
    traverseWait
        :: HabStatus habs
        -> (Either HWaitError :.: (,) ((Fin N4, Double), Double)) (HabStatus habs)
    traverseWait = Comp . waitTilNextFilled (gd ^. gdHabData) bs ic

-- | Wait until population reaches a given point.
--
-- If Left returned, means that hab maxed out.
waitTilFarmPop
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> Natural    -- ^ goal
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> WaitTilRes (Either (FarmStatus eggs '(tiers, epic) habs vehicles))
                  (FarmStatus eggs '(tiers, epic) habs vehicles)
waitTilFarmPop gd ic goal fs0 =
    case waitTilPop (gd ^. gdHabData) (farmBonuses gd fs0) ic goal (fs0 ^. fsHabs) of
      NoWait             -> NoWait
      NonStarter         -> NonStarter
      WaitTilSuccess t h ->
        let fs1 = stepFarm gd ic t fs0
        in  case h of
              Nothing -> WaitTilSuccess t (Left  fs1)
              Just _  -> WaitTilSuccess t (Right fs1)

-- | Time until depots are full
--
-- If Left returned, means that hab maxed out.
waitTilDepotFull
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> IsCalm
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> WaitTilRes (Either (FarmStatus eggs '(tiers, epic) habs vehicles))
                  (FarmStatus eggs '(tiers, epic) habs vehicles)
waitTilDepotFull gd ic fs0 = waitTilFarmPop gd ic (ceiling chickensAtCap) fs0
  where
    depotCap :: Double
    depotCap = fs0 ^. to (farmDepotCapacity gd)
    chickensAtCap :: Double
    chickensAtCap = depotCap / farmLayingRatePerChicken gd fs0

-- | Upgrade your egg!
--
upgradeEgg
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData  eggs '(tiers, epic) habs vehicles
    -> Finite eggs
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Either UpgradeError (FarmStatus eggs '(tiers, epic) habs vehicles)
upgradeEgg gd e fs
    | e <= fs ^. fsEgg         = Left UERegression
    | farmValue gd fs < unlock = Left UELocked
    | otherwise                = Right $
        fs & fsEgg           .~ e
           & fsResearch . rsCommon . liftTraversal (_Flip . mapped) .~ 0
           & fsHabs          .~ initHabStatus
           & fsDepot         .~ initSomeDepotStatus
           & fsBocks         .~ 0
           & fsGoldenEggs    %~ id
           & fsSoulEggs      %~ id
           & fsVideoBonus    %~ id
           & fsPrestEarnings %~ id
  where
    unlock = gd ^. gdEggData . _EggData . ixSV e . eggUnlock

-- | Vector of possible upgrades
eggUpgrades
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData  eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> (SV.Vector eggs :.: Either UpgradeError) (Finite eggs)
eggUpgrades gd fs = Comp . SV.generate $ \e ->
    let unlock = gd ^. gdEggData . _EggData . ixSV e . eggUnlock
    in  if | e < fs ^. fsEgg          -> Left UERegression
           | farmValue gd fs < unlock -> Left UELocked
           | otherwise                -> Right e

-- | Vector of visible eggs
eggsVisible
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData  eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> (SV.Vector eggs :.: Maybe) (Finite eggs)
eggsVisible gd fs = Comp . SV.generate $ \e ->
    let unlock = gd ^. gdEggData . _EggData . ixSV e . eggUnlock
    in  guard (farmValue gd fs >= unlock) $> e

-- | Farm value
--
-- Formula is current hardcoded.
--
-- Credit /u/patashu:
--
-- <https://www.reddit.com/r/EggsInc/comments/63pfye/patashus_egg_inc_guide_xpost_from_regginc/>
--
-- TODO: figure out if income should be capped by depot or not
farmValue
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData  eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Bock
farmValue gd fs = (x + y + z) ^. bonusingFor bs BTFarmValue
                               . multiplying eggBonus
  where
    eggBonus = gd ^. gdEggData . _EggData . ixSV (fs ^. fsEgg) . eggMultiplier
    bs       = farmBonuses gd fs
    hd       = gd ^. gdHabData
    income   = farmIncome gd fs
    chickens = fs ^. fsHabs . to (totalChickens hd)
    incomepc = income / chickens
    capacity = fs ^. fsHabs . to (totalHabCapacity hd bs)
    hatchery = internalHatcheryRate bs NotCalm * 60
    x = income * 54000
    y = incomepc * capacity * 6000
    z = incomepc * hatchery * 7200000

-- | Prestige!  Resets farm, increments soul eggs.
prestigeFarm
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData  eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
prestigeFarm gd fs =
    fs & fsEgg           .~ 0
       & fsResearch . rsCommon . liftTraversal (_Flip . mapped) .~ 0
       & fsHabs          .~ initHabStatus
       & fsDepot         .~ initSomeDepotStatus
       & fsBocks         .~ 0
       & fsGoldenEggs    %~ id
       & fsSoulEggs      +~ round pBonus
       & fsVideoBonus    %~ id
       & fsPrestEarnings .~ 0
  where
    pBonus = fs ^. fsPrestEarnings
                 . dividing (gd ^. gdConstants . gcPrestigeFactor)
                 . exponentiating 0.15
                 . bonusingFor (farmBonuses gd fs) BTPrestigeEggs

-- | Watch Video Doubler video.
--
-- Currently no limit to how often you can do this.
watchVideo
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
watchVideo gd fs = fs & fsVideoBonus %~ maybe (Just time) (Just . (+ time))
  where
    time = gd ^. gdConstants
               . gcBaseVideoDoublerTime
               . bonusingFor (farmBonuses gd fs) BTVideoDoublerTime
               . multiplying 60

-- | Largest root to a x^2 + b x + c, if any
quadratic :: Double -> Double -> Double -> Maybe Double
quadratic 0 b c = Just (- c / b)
quadratic a b c = case compare discr 0 of
    LT -> Nothing
    EQ -> Just (- b / (2 * a))
    GT -> Just ((- b + sqrt discr) / (2 * a))
  where
    discr = b * b - 4 * a * c
