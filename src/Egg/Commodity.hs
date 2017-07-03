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
import           Data.Maybe
import           Numeric.Lens
import           Text.Printf
import qualified Data.Vector  as V

newtype Bock = Bock { _bock :: Double }
  deriving (Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, ToJSON, FromJSON)

makeLenses ''Bock

instance Show Bock where
  show = showBock

showBock :: Bock -> String
showBock b
    | digs <= 2 = printf "%.2f" (b ^. bock)
    | digs <= 6 = printf "%.0f" (b ^. bock)
    | otherwise = printf "%.3f%s" (b ^. bock . dividing (10 ^ (pow3 * 3))) powpre
  where
    digs = floor $ logBase 10 b
    pow3 = digs `div` 3
    powpre = fromMaybe (printf "e%d" (pow3*3)) $ powpres V.!? pow3

powpres :: V.Vector String
powpres = V.fromList [ ""  , ""  , "M" , "B"
                     , "T" , "q" , "Q" , "s"
                     , "S" , "o" , "N" , "d"
                     , "U" , "D" , "Td", "qd"
                     , "Qd", "sd", "Sd"
                     ]

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
