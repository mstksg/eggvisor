{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Egg where
-- module Egg (
--   ) where

-- import           Data.Type.Fin
-- import           Data.Type.Nat            as TCN
-- import qualified Type.Family.Nat
import           Control.Monad.Trans.Maybe
import           Data.Dependent.Map
import           Data.Finite
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude.Num
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           GHC.TypeLits
import           Numeric.Natural
import qualified Data.Vector.Sized           as V

$(promote [d|
  numUpgrades :: (Eq a, Num a) => a -> a
  numUpgrades i = case i of
    0  -> 4
    1  -> 5
    2  -> 5
    3  -> 5
    4  -> 5
    5  -> 4
    6  -> 4
    7  -> 4
    8  -> 4
    9  -> 4
    10 -> 3
    _  -> 0
  |])

-- type family NumUpgrades n where
--     NumUpgrades 0  = 4
--     NumUpgrades 1  = 5
--     NumUpgrades 2  = 5
--     NumUpgrades 3  = 5
--     NumUpgrades 4  = 5
--     NumUpgrades 5  = 4
--     NumUpgrades 6  = 4
--     NumUpgrades 7  = 4
--     NumUpgrades 8  = 4
--     NumUpgrades 9  = 4
--     NumUpgrades 10 = 3
--     NumUpgrades n  = 0

newtype (:.~:) :: (l -> Type) -> (k ~> l) -> k -> Type where
    CompTyFun :: f (Apply g a) -> (f :.~: g) a

-- newtype Upgrade :: Nat -> Type where
--     Upgr :: Finite (NumUpgrades n) -> Upgrade n

-- newtype Upgrades :: Nat -> Type where
--     Upgrs :: V.Vector (NumUpgrades n) Natural -> Upgrades n

type HabSlot = Finite 4
type Hab = Finite 18

type VehicleSlot = Finite 14
type Vehicle = Finite 11

data Move :: Type where
    MResearch
        :: (DSum Sing (Finite :.~: NumUpgradesSym0))
        -> Move
    MHabUpgrade
        :: HabSlot -> Hab -> Move
    MVehicleUpgrade
        :: VehicleSlot -> Vehicle -> Move
    MFleetUpgrade
        :: Move
    MWaitChickens
        :: Natural -> Move
    MWaitBocks
        :: Double -> Move

-- data FarmShape :: Type where
--     FS :: { fsFleetSize :: Nat
--           }
--        -> FarmShape

data Farm :: Type where
    Farm :: { fBocks     :: Double
            , fChickens  :: Natural
            , fUpgrades  :: DMap Sing (Flip V.Vector Natural :.~: NumUpgradesSym0)
            , fHabitats  :: V.Vector 4 Hab
            , fVehicles  :: MaybeT (V.Vector 14) Hab
            , fSoulEnggs :: Natural
            }
         -> Farm

data Bonus :: Type where
    Bonus :: { bEggValue   :: Double
             , bFarmValue  :: Double
             , bLayingRate :: Double
             , bHabSize    :: Double
             }
          -> Bonus

