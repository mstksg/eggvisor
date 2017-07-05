{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Egg.Farm (
    FarmStatus(..)
  , fsEgg, fsResearch, fsHabs, fsHatchery, fsDepot, fsBocks, fsGoldenEggs, fsSoulEggs
  , fsVideoBonus, fsPrestEarnings
  , IncomeChange(..), _ICHabFill, _ICDepotFill, _ICVideoDoublerExpire
  , UpgradeError(..), _UELocked, _UERegression
  , HatchError(..), heHabitats, heHatchery
  , Drone(..), _NormalDrone, _EliteDrone
  , initFarmStatus
  , farmEggValue
  , farmLayingRate
  , farmLayingRatePerChicken
  , farmLayingRateAvailability
  , hatcheryRefillRate
  , hatcheryCapacity
  , refillHatcheries
  , farmIncomeNoBonus
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
  , hatchChickens
  , upgradeEgg
  , eggUpgrades
  , eggsVisible
  , prestigeFarm
  , watchVideo
  , popDrone
  ) where

import           Control.Lens hiding       ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Finite
import           Data.Functor
import           Data.Kind
import           Data.Semigroup
import           Data.Singletons
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
import qualified Data.Vector.Sized         as SV
import qualified GHC.TypeLits              as TL

data FarmStatus :: Nat -> ([Nat], Nat) -> Nat -> Nat -> Type where
    FarmStatus :: { _fsEgg           :: Finite eggs
                  , _fsResearch      :: ResearchStatus tiers epic
                  , _fsHabs          :: HabStatus habs
                  , _fsDepot         :: SomeDepotStatus vehicles
                  , _fsHatchery      :: Double
                  , _fsBocks         :: Bock
                  , _fsGoldenEggs    :: GoldenEgg
                  , _fsSoulEggs      :: SoulEgg
                  , _fsVideoBonus    :: Maybe Double   -- ^ in seconds
                  , _fsPrestEarnings :: Bock
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

data HatchError = HEHatcheryLow { _heHatchery :: Double }
                | HEHabsFull    { _heHabitats :: Double }
  deriving (Show, Eq, Ord)

makePrisms ''HatchError
makeLenses ''HatchError

data Drone = NormalDrone (Fin N3)
           | EliteDrone
  deriving (Show, Eq, Ord)

makePrisms ''Drone

fsEgg :: Lens (FarmStatus e1 '(t, g) h v) (FarmStatus e2 '(t, g) h v) (Finite e1) (Finite e2)
fsEgg f fs = (\e -> fs { _fsEgg = e }) <$> f (_fsEgg fs)
fsResearch :: Lens (FarmStatus e '(t1, g1) h v) (FarmStatus e '(t2, g2) h v) (ResearchStatus t1 g1) (ResearchStatus t2 g2)
fsResearch f fs = f (_fsResearch fs) <&> \r ->
    (FarmStatus <$> _fsEgg <*> pure r <*> _fsHabs <*> _fsDepot <*> _fsHatchery <*> _fsBocks
                <*> _fsGoldenEggs <*> _fsSoulEggs <*> _fsVideoBonus <*> _fsPrestEarnings
    ) fs
fsHabs :: Lens (FarmStatus e '(t, g) h1 v) (FarmStatus e '(t, g) h2 v) (HabStatus h1) (HabStatus h2)
fsHabs f fs = (\h -> fs { _fsHabs = h }) <$> f (_fsHabs fs)
fsDepot :: Lens (FarmStatus e '(t, g) h v1) (FarmStatus e '(t, g) h v2) (SomeDepotStatus v1) (SomeDepotStatus v2)
fsDepot f fs = (\d -> fs { _fsDepot = d }) <$> f (_fsDepot fs)
fsHatchery :: Lens' (FarmStatus e '(t, g) h v) Double
fsHatchery f fs = (\h -> fs { _fsHatchery = h }) <$> f (_fsHatchery fs)
fsBocks :: Lens' (FarmStatus e '(t, g) h v) Bock
fsBocks f fs = (\b -> fs { _fsBocks = max 0 b }) <$> f (_fsBocks fs)
fsGoldenEggs :: Lens' (FarmStatus e '(t, g) h v) GoldenEgg
fsGoldenEggs f fs = (\e -> fs { _fsGoldenEggs = max 0 e }) <$> f (_fsGoldenEggs fs)
fsSoulEggs :: Lens' (FarmStatus e '(t, g) h v) SoulEgg
fsSoulEggs f fs = (\s -> fs { _fsSoulEggs = max 0 s }) <$> f (_fsSoulEggs fs)
fsPrestEarnings :: Lens' (FarmStatus e '(t, g) h v) Bock
fsPrestEarnings f fs = (\pe -> fs { _fsPrestEarnings = max 0 pe }) <$> f (_fsPrestEarnings fs)

-- | Only for incrementing
fsBocksAndPrest :: Traversal' (FarmStatus e '(t, g) h v) Bock
fsBocksAndPrest f fs = (\b p -> fs { _fsBocks = max 0 b, _fsPrestEarnings = max 0 p })
    <$> f (_fsBocks fs)
    <*> f (_fsPrestEarnings fs)

-- | Enforces only postive Just
fsVideoBonus :: Lens' (FarmStatus e '(t, g) h v) (Maybe Double)
fsVideoBonus f fs = (\vb -> fs { _fsVideoBonus = mfilter (> 0) vb }) <$> f (_fsVideoBonus fs)

instance HasHabStatus (FarmStatus e '(t, g) h v) h where
    habStatus = fsHabs

instance (KnownNat egg, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
      => FromJSON (FarmStatus egg '(tiers, epic) habs vehicles) where
    parseJSON  = withObject "FarmStatus" $ \v ->
        FarmStatus <$> v .: "eggs"
                   <*> v .: "research"
                   <*> v .: "habs"
                   <*> v .: "depot"
                   <*> v .: "hatchery"
                   <*> v .: "bocks"
                   <*> v .: "golden-eggs"
                   <*> v .: "soul-eggs"
                   <*> v .: "video-bonus"
                   <*> v .: "prest-earnings"

instance ToJSON (FarmStatus egg research habs vehicles) where
    toJSON FarmStatus{..} = object
        [ "egg"            .= _fsEgg
        , "research"       .= _fsResearch
        , "habs"           .= _fsHabs
        , "depot"          .= _fsDepot
        , "hatchery"       .= _fsHatchery
        , "bocks"          .= _fsBocks
        , "golden-eggs"    .= _fsGoldenEggs
        , "soul-eggs"      .= _fsSoulEggs
        , "video-bonus"    .= _fsVideoBonus
        , "prest-earnings" .= _fsPrestEarnings
        ]
    toEncoding FarmStatus{..} = pairs . mconcat $
        [ "egg"            .= _fsEgg
        , "research"       .= _fsResearch
        , "habs"           .= _fsHabs
        , "depot"          .= _fsDepot
        , "hatchery"       .= _fsHatchery
        , "bocks"          .= _fsBocks
        , "golden-eggs"    .= _fsGoldenEggs
        , "soul-eggs"      .= _fsSoulEggs
        , "video-bonus"    .= _fsVideoBonus
        , "prest-earnings" .= _fsPrestEarnings
        ]

initFarmStatus
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles, 1 TL.<= eggs, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
initFarmStatus gd@(GameData _ rd _ _ _) =
    FarmStatus 0
               (emptyResearchStatus rd)
               initHabStatus
               initSomeDepotStatus
               (gd ^. gcBaseHatcheryCapacity)
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
    -> Bock
farmEggValue gd fs = gd ^. edEggs
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
    chickens = totalChickens fs
    cap :: Double
    cap = withSomeDepotStatus bs (fs ^. fsDepot) $
            totalDepotCapacity (gd ^. gdVehicleData) bs

-- | How many eggs are laid per second, per chicken.
farmLayingRatePerChicken
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmLayingRatePerChicken gd fs =
    gd ^. gcBaseLayingRate
        . dividing 60
        . bonusingFor (farmBonuses gd fs) BTLayingRate

-- | Apply video doubling bonus
videoDoubling
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Iso' Bock Bock
videoDoubling gd fs = case fs ^. fsVideoBonus of
    Just _  -> multiplying $ gd ^. gcVideoBonus . to realToFrac
    Nothing -> id

-- | Total income (bocks per second), no bonuses
farmIncomeNoBonus
    :: (KnownNat eggs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Bock
farmIncomeNoBonus gd fs
    = fs ^. to (realToFrac . farmLayingRate gd)
          . to (* farmEggValue gd fs)
          . bonusingSoulEggs gd fs

-- | Total income (bocks per second), with bonuses
farmIncome
    :: (KnownNat eggs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Bock
farmIncome gd fs = fs ^. to (farmIncomeNoBonus gd)
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
    -> Bock
farmIncomeDT gd ic fs dt
    | initLaying >= cap = (realToFrac (dt * cap) * eggVal)
                                 ^. bonusingSoulEggs gd fs
                                  . videoDoubling gd fs
    | otherwise         = (initHabSize * dt + growthRate * dt * dt / 2)
                            ^. to (* layingRate)
                             . re bock
                             . to (* eggVal)
                             . bonusingSoulEggs gd fs
                             . videoDoubling gd fs

  where
    layingRate  = farmLayingRatePerChicken gd fs
    eggVal      = farmEggValue gd fs
    initHabSize = totalChickens fs
    growthRate  = totalGrowthRate gd (farmBonuses gd fs) ic fs
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

-- | Chickens per second
hatcheryRefillRate
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
hatcheryRefillRate gd fs =
        gd ^. gcBaseHatcheryRefillRate
            . dividing 60
            . bonusingFor (farmBonuses gd fs) BTHatcheryRate

hatcheryCapacity
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
hatcheryCapacity gd fs = gd ^. gcBaseHatcheryCapacity
                             . bonusingFor (farmBonuses gd fs) BTHatcheryCapacity

refillHatcheries
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> Double               -- ^ dt
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
refillHatcheries gd dt fs0 =
        fs0 & fsHatchery +~ hatcheryRefillRate gd fs0 * dt
            & fsHatchery %~ min (hatcheryCapacity gd fs0)

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
    | initLaying >= cap = Just . realToFrac $
        dBock / (realToFrac cap * eggVal
                   ^. bonusingSoulEggs gd fs
                    . videoDoubling gd fs
                )
    | otherwise         =
        let c = dBock ^. dividing (realToFrac layingRate * eggVal)
                       . from (bonusingSoulEggs gd fs)
                       . from (videoDoubling gd fs)
                       . negated
        in  quadratic (growthRate / 2) initHabSize (realToFrac c)
  where
    curr        = fs ^. fsBocks
    dBock       = goal - curr
    layingRate  = farmLayingRatePerChicken gd fs
    eggVal      = farmEggValue gd fs
    initHabSize = totalChickens fs
    growthRate  = totalGrowthRate gd (farmBonuses gd fs) ic fs
    initLaying  = layingRate * initHabSize
    cap         = fs ^. to (farmDepotCapacity gd)

-- | Soul egg bonus
bonusingSoulEggs
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Iso' Bock Bock
bonusingSoulEggs gd fs = multiplying $
    1 + (realToFrac bonus * fromIntegral (fs ^. fsSoulEggs)) / 100
  where
    bonus = gd ^. gcBaseSoulEggBonus
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
    & stepHabs gd (farmBonuses gd fs) ic dt
    & fsVideoBonus . mapped -~ dt
    & refillHatcheries gd dt

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
waitTilNextIncomeChange gd ic fs0 = case waitTilNextFilled gd bs ic fs0 of
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
                               & availableSpace gd bs . mapped -~ rt * tChange
                               & fsVideoBonus . mapped -~ tChange
                               & refillHatcheries gd tChange
                 in  WaitTilSuccess tChange (ich, fs2)
               Nothing                      ->
                 let fs2 = fs0 & fsBocksAndPrest       +~ farmIncomeDT gd ic fs0 tFill
                               & fsHabs                .~ (fs1 ^. fsHabs)
                               & fsVideoBonus . mapped -~ tFill
                               & refillHatcheries gd tFill
                 in  WaitTilSuccess tFill (ICHabFill s, fs2)
  where
    initAllFull    = andOf (to (fullHabs gd bs) . folded) fs0
    initMaxedDepot = has _Nothing depotAvail0
    bs :: Bonuses
    bs = farmBonuses gd fs0
    depotAvail0 :: Maybe Double
    depotAvail0 = snd (farmLayingRateAvailability gd fs0)

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
    case waitTilPop gd (farmBonuses gd fs0) ic goal fs0 of
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

hatchChickens
    :: KnownNat habs
    => GameData   eggs '(tiers, epic) habs vehicles
    -> Natural           -- ^ number of chickens
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Either HatchError (FarmStatus eggs '(tiers, epic) habs vehicles)
hatchChickens gd (fromIntegral->n) fs = do
    if fs ^. fsHatchery >= n
      then Right ()
      else Left $ HEHatcheryLow (fs ^. fsHatchery)
    fs & fsHatchery -~ n & fsHabs %%~ \h ->
      case addChickens gd bs n h of
        (Just _ , _ ) -> Left $ HEHabsFull (totalHabCapacity gd bs h)
        (Nothing, h') -> Right h'
  where
    bs = farmBonuses gd fs

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
           & (\f -> f & fsHatchery .~ hatcheryCapacity gd f)
           & fsGoldenEggs    %~ id
           & fsSoulEggs      %~ id
           & fsVideoBonus    %~ id
           & fsPrestEarnings %~ id
  where
    unlock = gd ^. edEggs . ixSV e . eggUnlock

-- | Vector of possible upgrades
eggUpgrades
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData  eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> (SV.Vector eggs :.: Either UpgradeError) (Finite eggs)
eggUpgrades gd fs = Comp . SV.generate $ \e ->
    let unlock = gd ^. edEggs . ixSV e . eggUnlock
    in  if | e <= fs ^. fsEgg         -> Left UERegression
           | farmValue gd fs < unlock -> Left UELocked
           | otherwise                -> Right e

-- | Vector of visible eggs
eggsVisible
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData  eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> (SV.Vector eggs :.: Maybe) (Finite eggs)
eggsVisible gd fs = Comp . SV.generate $ \e ->
    let unlock = gd ^. edEggs . ixSV e . eggUnlock
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
                               . multiplying (realToFrac eggBonus)
  where
    eggBonus = gd ^. edEggs . ixSV (fs ^. fsEgg) . eggMultiplier
    bs       = farmBonuses gd fs
    income   = farmIncomeNoBonus gd fs
    chickens = totalChickens fs
    incomepc
      | chickens > 0 = income / realToFrac chickens
      | otherwise    = 0
    capacity = totalHabCapacity gd bs fs
    hatchery = internalHatcheryRate bs NotCalm * 60
    x = income * 54000
    y = incomepc * realToFrac capacity * 6000
    z = incomepc * realToFrac hatchery * 7200000

-- | Prestige!  Resets farm, increments soul eggs.
prestigeFarm
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData  eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Maybe (FarmStatus eggs '(tiers, epic) habs vehicles)
prestigeFarm gd fs = guard (round @_ @Int pBonus > 0) $>
    (fs & fsEgg           .~ 0
        & fsResearch . rsCommon . liftTraversal (_Flip . mapped) .~ 0
        & fsHabs          .~ initHabStatus
        & fsDepot         .~ initSomeDepotStatus
        & (\f -> f & fsHatchery .~ hatcheryCapacity gd f)
        & fsBocks         .~ 0
        & fsGoldenEggs    %~ id
        & fsSoulEggs      +~ round pBonus
        & fsVideoBonus    %~ id
        & fsPrestEarnings .~ 0
    )
  where
    pBonus = fs ^. fsPrestEarnings
                 . dividing (realToFrac (gd ^. gcPrestigeFactor))
                 . exponentiating 0.15
                 . bonusingFor (farmBonuses gd fs) BTPrestigeEggs

-- | Watch Video Doubler video.
--
-- Returns 'Left' if video doubler refresh limit has not yet been reached,
-- with time left.
watchVideo
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Either Double (FarmStatus eggs '(tiers, epic) habs vehicles)
watchVideo gd fs = case fs ^. fsVideoBonus of
    Just t | t > lim -> Left (t - lim)
    _                ->
        Right $ fs & fsVideoBonus %~ maybe (Just time) (Just . (+ time))
  where
    lim  = gd ^. gcVideoBonusRefresh . multiplying 60
    time = gd ^. gcBaseVideoDoublerTime
               . bonusingFor (farmBonuses gd fs) BTVideoDoublerTime
               . multiplying 60

popDrone
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> Drone
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
popDrone gd d fs = fs & fsBocksAndPrest +~ (mult * farmValue gd fs)
  where
    mult = case d of
      NormalDrone i -> case i of
        FZ         -> 0.0004
        FS FZ      -> 0.003
        FS (FS FZ) -> 0.01
        _          -> error "unreachable"
      EliteDrone -> 0.4


-- | Largest root to a x^2 + b x + c, if any
quadratic :: Double -> Double -> Double -> Maybe Double
quadratic 0 b c = Just (- c / b)
quadratic a b c = case compare discr 0 of
    LT -> Nothing
    EQ -> Just (- b / (2 * a))
    GT -> Just ((- b + sqrt discr) / (2 * a))
  where
    discr = b * b - 4 * a * c
