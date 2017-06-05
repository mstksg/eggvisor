{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Egg.Vehicle (
    Vehicle(..), vName, vBaseCapacity, vCosts
  , VehicleData(..), _VehicleData, SomeVehicleData
  , DepotStatus(..), _DepotStatus
  , initDepotStatus
  , upgradeDepo
  , baseCapacity
  , totalCapacity
  ) where

import           Control.Lens hiding       ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Type.Combinator.Util
import           Data.Type.Nat             as TCN
import           Data.Type.Vector
import           Egg.Research
import           GHC.Generics              (Generic)
import           Numeric.Lens
import           Numeric.Natural
import           Text.Printf
import           Type.Class.Known
import           Type.Class.Witness
import           Type.Family.Nat           as TCN
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import qualified Data.Vector.Sized         as SV

data Vehicle = Vehicle
        { _vName         :: T.Text
        , _vBaseCapacity :: Natural         -- ^ eggs per minute
        , _vCosts        :: [Double]
        }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''Vehicle

newtype VehicleData vs = VehicleData { _vdVehicles :: SV.Vector vs Vehicle }
  deriving (Show, Eq, Ord, Generic)

makePrisms ''VehicleData
makeWrapped ''VehicleData

type SomeVehicleData = DSum Sing VehicleData

data DepotStatus vs slots
    = DepotStatus { _dsSlots :: Vec slots (S.Set (Finite vs)) }
  deriving (Show, Eq, Ord, Generic)

makePrisms ''DepotStatus
makeWrapped ''DepotStatus

vehicleParseOptions :: Options
vehicleParseOptions = defaultOptions
    { fieldLabelModifier = camelTo2 '-' . drop 2
    }

instance FromJSON Vehicle where
    parseJSON  = genericParseJSON  vehicleParseOptions
instance ToJSON Vehicle where
    toJSON     = genericToJSON     vehicleParseOptions
    toEncoding = genericToEncoding vehicleParseOptions

instance FromJSON SomeVehicleData where
    parseJSON = withObject "VehicleData" $ \v -> do
      vs <- v .: "vehicles"
      SV.withSized vs $ \vsV ->
        return $ SNat :=> VehicleData vsV
instance ToJSON SomeVehicleData where
    toJSON = \case
        _ :=> r -> toJSON r
    toEncoding = \case
        _ :=> r -> toEncoding r
instance KnownNat vs => FromJSON (VehicleData vs) where
    parseJSON = withObject "VehicleData" $ \v -> do
      vs <- v .: "vehicles"
      case SV.toSized vs of
        Nothing  -> fail $ printf "Bad number of items in list. (Expected %d, got %d)"
                         (fromSing (SNat @vs)) (length vs)
        Just vsV -> return $ VehicleData vsV
instance ToJSON (VehicleData habs) where
    toJSON VehicleData{..} = object
        [ "vehicles" .= SV.fromSized _vdVehicles
        ]
    toEncoding VehicleData{..} = pairs . mconcat $
        [ "vehicles" .= SV.fromSized _vdVehicles
        ]

-- | Initial 'DepotStatus' to start off the game.
initDepotStatus :: KnownNat vs => DepotStatus vs N4
initDepotStatus = DepotStatus $ S.singleton 0 :+ pure S.empty

-- | Add another empty depot slot
upgradeDepo :: forall vs n. Known TCN.Nat n => DepotStatus vs n -> DepotStatus vs ('S n)
upgradeDepo = over _DepotStatus (.++ (S.empty :+ ØV))
                \\ addCommute (known :: TCN.Nat n) (S_ Z_)

-- | Total base capacity of all slots, in eggs per second.
baseCapacity
    :: forall vs slots. KnownNat vs
    => VehicleData vs
    -> DepotStatus vs slots
    -> Double
baseCapacity VehicleData{..} = sumOf $ _DepotStatus
                                     . folded
                                     . folding lookupMax
                                     . to (SV.index _vdVehicles)
                                     . vBaseCapacity
                                     . to fromIntegral
                                     . dividing 60

-- | Total capacity of all hatcheries, factoring in bonuses.
totalCapacity
    :: forall vs slots. KnownNat vs
    => VehicleData vs
    -> Bonuses
    -> DepotStatus vs slots
    -> Double
totalCapacity vd@VehicleData{..} bs =
    sumOf $ to (baseCapacity vd)
          . bonusingFor bs BTVehicleCapacity
          . bonusingFor bs BTVehicleSpeed

-- | Obsolete with containers-0.5.9, with GHC 8.2
lookupMax :: S.Set a -> Maybe a
lookupMax = fmap fst . S.maxView

