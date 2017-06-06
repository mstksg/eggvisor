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
    FarmStatus(..), fsEgg, fsResearch, fsHabs, fsDepot
  , initFarmStatus
  ) where

-- import           Data.Singletons
-- import           Data.Type.Combinator.Util
-- import           Data.Type.Conjunction
-- import           Egg.Egg
-- import           GHC.Generics              (Generic)
import           Control.Lens
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Kind
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Egg.GameData
import           Egg.Habitat
import           Egg.Research
import           Egg.Vehicle
import           Numeric.Natural
import           Type.Class.Known
import           Type.Family.List
import qualified Data.Vector.Sized            as SV
import qualified GHC.TypeLits                 as TL

data FarmStatus :: Nat -> ([Nat], Nat) -> Nat -> Nat -> Type where
    FarmStatus :: { _fsEgg      :: Finite eggs
                  , _fsResearch :: ResearchStatus tiers epic
                  , _fsHabs     :: HabStatus habs
                  , _fsDepot   :: SomeDepotStatus vehicles
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
                        FarmStatus (_fsEgg fs) r (_fsHabs fs) (_fsDepot fs)
fsHabs :: Lens (FarmStatus e '(t, g) h1 v) (FarmStatus e '(t, g) h2 v) (HabStatus h1) (HabStatus h2)
fsHabs f fs = (\h -> fs { _fsHabs = h }) <$> f (_fsHabs fs)
fsDepot :: Lens (FarmStatus e '(t, g) h v1) (FarmStatus e '(t, g) h v2) (SomeDepotStatus v1) (SomeDepotStatus v2)
fsDepot f fs = (\d -> fs { _fsDepot = d }) <$> f (_fsDepot fs)

initFarmStatus
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles, 1 TL.<= eggs, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
initFarmStatus (GameData _ rd _ _ _) =
    FarmStatus 0 (emptyResearchStatus rd) initHabStatus (known :=> initDepotStatus)
