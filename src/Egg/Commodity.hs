{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Egg.Commodity (
    Bock(..), bock
  , GoldenEgg
  , SoulEgg
  , MultiCost(..), mcBocks, mcGoldenEggs
  , mkMCBocks, mkMCGoldenEggs
  , remcBocks, remcGoldenEggs
  ) where

import           Control.Lens
import           Data.Aeson

newtype Bock = Bock { _bock :: Double }
  deriving (Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, ToJSON, FromJSON)

instance Show Bock

makeLenses ''Bock

-- type Bock      = Double
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
