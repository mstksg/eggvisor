{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}

module Egg.Action (
  ) where

import           Control.Lens
import           Data.Finite
import           Data.Kind
import           Data.Singletons.TypeLits
import           Data.Type.Fin
import           Egg.Research
import           Type.Family.Nat

data Action :: Nat -> ([Nat], Nat) -> Nat -> Nat -> Type where
    AResearch
        :: ResearchIx tiers epic a
        -> Action eggs '(tiers, epic) habs vehicles
    AHab
        :: Fin N4
        -> Finite habs
        -> Action eggs '(tiers, epic) habs vehicles
    AVehicle
        :: Integer
        -> Finite vehicles
        -> Action eggs '(tiers, epic) habs vehicles
    AWatchVideo
        :: Action eggs '(tiers, epic) habs vehicles
    AEggUpgrade
        :: Finite eggs
        -> Action eggs '(tiers, epic) habs vehicles
    APrestige
        :: Action eggs '(tiers, epic) habs vehicles

