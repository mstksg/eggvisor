{-# LANGUAGE TemplateHaskell #-}

module Egg.Commodity (
    Bock
  , GoldenEgg
  , SoulEgg
  , MultiCost(..), mcBocks, mcGoldenEggs
  , mkMCBocks, mkMCGoldenEggs
  , remcBocks, remcGoldenEggs
  ) where

import           Control.Lens

type Bock      = Double
type GoldenEgg = Integer
type SoulEgg   = Integer

data MultiCost = MC { _mcBocks :: Bock, _mcGoldenEggs :: GoldenEgg }
  deriving (Show, Eq, Ord)

makeLenses ''MultiCost

mkMCBocks :: Bock -> MultiCost
mkMCBocks = (`MC` 0)

mkMCGoldenEggs :: GoldenEgg -> MultiCost
mkMCGoldenEggs = MC 0

remcBocks :: Review MultiCost Bock
remcBocks = unto mkMCBocks

remcGoldenEggs :: Review MultiCost GoldenEgg
remcGoldenEggs = unto mkMCGoldenEggs
