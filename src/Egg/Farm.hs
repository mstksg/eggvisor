{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
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
  ) where

-- import           Data.Singletons
-- import           Data.Type.Combinator.Util
-- import           Data.Type.Conjunction
-- import           GHC.Generics              (Generic)
import           Control.Lens
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Kind
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
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

-- | Egg rate in eggs per second
farmLayingRate
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmLayingRate gd fs =
    gd ^. gdConstants
        . gcBaseLayingRate
        . dividing 60
        . multiplying chickens
        . bonusingFor (farmBonuses gd fs) BTLayingRate
  where
    chickens :: Double
    chickens = fs ^. fsHabs . to (totalChickens (gd ^. gdHabData))

-- | Total income (bocks per second)
farmIncome
    :: KnownNat eggs
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmIncome gd fs = fs ^. to (farmLayingRate gd)
                       . multiplying (farmEggValue gd fs)
                       . multiplying (farmSoulEggBonus gd fs)

-- | Soul egg bonus
farmSoulEggBonus
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Double
farmSoulEggBonus gd fs =
    gd ^. gdConstants
        . gcBaseSoulEggBonus
        . multiplying (fs ^. fsSoulEggs . to fromIntegral)
        . bonusingFor (farmBonuses gd fs) BTSoulEggBonus

-- | Bonuses from a given 'GameData' and 'FarmStatus'
farmBonuses
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Bonuses
farmBonuses gd fs = totalBonuses (gd ^. gdResearchData) (fs ^. fsResearch)
