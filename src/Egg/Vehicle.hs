{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Egg.Vehicle (
    Vehicle(..), vName, vBaseCapacity, vCosts
  , VehicleData(..), _VehicleData, SomeVehicleData
  , DepotStatus(..), _DepotStatus, SomeDepotStatus
  , initDepotStatus
  , initSomeDepotStatus
  , upgradeDepot
  , baseDepotCapacity
  , totalDepotCapacity
  , vehicleHistory
  , vehiclePrice
  , upgradeVehicle
  , vehicleUpgrades
  , someVehicleUpgrades
  ) where

import           Control.Lens hiding        ((.=))
import           Control.Monad
import           Control.Monad.Trans.Writer
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Tuple
import           Data.Type.Combinator
import           Data.Type.Combinator.Util  as TC
import           Data.Type.Fin
import           Data.Type.Nat              as TCN
import           Data.Type.Vector           as TCV
import           Data.Vector.Sized.Util
import           Egg.Commodity
import           Egg.Research
import           GHC.Generics               (Generic)
import           Numeric.Lens
import           Numeric.Natural
import           Text.Printf
import           Type.Class.Higher
import           Type.Class.Known
import           Type.Class.Witness
import           Type.Family.Nat            as TCN
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import qualified Data.Vector.Sized          as SV
import qualified GHC.TypeLits               as TL

data Vehicle = Vehicle
        { _vName         :: T.Text
        , _vBaseCapacity :: Natural         -- ^ eggs per minute
        , _vCosts        :: V.Vector Bock
        }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''Vehicle

newtype VehicleData vs = VehicleData { _vdVehicles :: SV.Vector vs Vehicle }
  deriving (Show, Eq, Ord, Generic)

makePrisms ''VehicleData
makeWrapped ''VehicleData

type SomeVehicleData = DSum Sing VehicleData

data DepotStatus vs slots
    = DepotStatus { _dsSlots :: Vec slots (Maybe (Finite vs)) }
  deriving (Show, Eq, Ord, Generic)

makePrisms ''DepotStatus
makeWrapped ''DepotStatus

type SomeDepotStatus vs = DSum TCN.Nat (DepotStatus vs)

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
    parseJSON o = do
      vs <- parseJSON o
      SV.withSized vs $ \vsV ->
        return $ SNat :=> VehicleData vsV
instance ToJSON SomeVehicleData where
    toJSON = \case
        _ :=> r -> toJSON r
    toEncoding = \case
        _ :=> r -> toEncoding r
instance KnownNat vs => FromJSON (VehicleData vs) where
    parseJSON o = do
      vs <- parseJSON o
      case SV.toSized vs of
        Nothing  -> fail $ printf "Bad number of items in list. (Expected %d, got %d)"
                         (fromSing (SNat @vs)) (length vs)
        Just vsV -> return $ VehicleData vsV
instance ToJSON (VehicleData habs) where
    toJSON     = toJSON     . SV.fromSized . _vdVehicles
    toEncoding = toEncoding . SV.fromSized . _vdVehicles

-- | Initial 'DepotStatus' to start off the game.
initDepotStatus :: (KnownNat vs, 1 TL.<= vs) => DepotStatus vs N4
initDepotStatus = DepotStatus $ Just 0 :+ pure Nothing

-- | Initial 'SomeDepotStatus' to start off the game.
initSomeDepotStatus :: (KnownNat vs, 1 TL.<= vs) => SomeDepotStatus vs
initSomeDepotStatus = known :=> initDepotStatus


-- | Add another empty depot slot
upgradeDepot :: forall vs n. Known TCN.Nat n => DepotStatus vs n -> DepotStatus vs ('S n)
upgradeDepot = over _DepotStatus (.++ (Nothing :+ ØV))
                 \\ addCommute (known :: TCN.Nat n) (S_ Z_)

-- | Total base capacity of all slots, in eggs per second.
baseDepotCapacity
    :: forall vs slots. KnownNat vs
    => VehicleData vs
    -> DepotStatus vs slots
    -> Double
baseDepotCapacity VehicleData{..} =
    sumOf $ _DepotStatus
          . folded
          . folded
          . to (SV.index _vdVehicles)
          . vBaseCapacity
          . to fromIntegral
          . dividing 60

-- | Total capacity of all vehicles, factoring in bonuses.
totalDepotCapacity
    :: forall vs slots. KnownNat vs
    => VehicleData vs
    -> Bonuses
    -> DepotStatus vs slots
    -> Double
totalDepotCapacity vd@VehicleData{..} bs =
    sumOf $ to (baseDepotCapacity vd)
          . bonusingFor bs BTVehicleCapacity
          . bonusingFor bs BTVehicleSpeed

-- | How many of each vehicle has been purchased so far.  If key is not found,
-- zero purchases is implied.
vehicleHistory
    :: Known TCN.Nat slots
    => DepotStatus vs slots
    -> M.Map (Finite vs) (Fin ('S slots))
vehicleHistory = M.mapMaybe (natFin . someNat)
               . M.fromListWith (+)
               . toListOf (_DepotStatus . folded . folded . to (, 1))

-- | Get the BASE price of a given vehicle, if a purchase were to be made.
-- Does not check if purchase is legal (see 'upgradeVehicle').
vehiclePrice
    :: forall vs slots. (KnownNat vs, Known TCN.Nat slots)
    => VehicleData vs
    -> DepotStatus vs slots
    -> Finite vs
    -> Maybe Bock
vehiclePrice vd ds v = fmap priceOf
                     . TC.strengthen
                     . fromMaybe FZ
                     . M.lookup v
                     . vehicleHistory
                     $ ds
  where
    priceOf :: Fin slots -> Bock
    priceOf i = fromMaybe 0 $
                  vd ^? _VehicleData . ixSV v . vCosts . ix (fin i)

-- | Purchase a vehicle upgrade.  Returns cost and new depot status,
-- if purchase is valid.
--
-- Purchase is invalid if purchasing a vehicle in a slot where a greater
-- vehicle is already purchased.
upgradeVehicle
    :: (KnownNat vs, Known TCN.Nat slots)
    => VehicleData vs
    -> Bonuses
    -> Fin slots
    -> Finite vs
    -> DepotStatus vs slots
    -> Maybe (Bock, DepotStatus vs slots)
upgradeVehicle vd bs slot v ds0 =
    fmap swap . runWriterT . flip (_DepotStatus . ixV slot) ds0 $ \s0 -> WriterT $ do
      guard $ case s0 of
                Nothing -> True
                Just h  -> h < v
      -- should always be valid of the previous condition is true
      price <- vehiclePrice vd ds0 v
      return (Just v, price ^. bonusingFor bs BTVehicleCosts)

-- | List all possible vehicle upgrades.
vehicleUpgrades
    :: forall vs slots. Known TCN.Nat slots
    => VehicleData vs
    -> Bonuses
    -> DepotStatus vs slots
    -> VecT slots (SV.Vector vs) (Maybe Bock)
vehicleUpgrades vd bs ds = ds ^. _DepotStatus
                               & vmap (go . getI)
  where
    hist :: M.Map (Finite vs) (Fin ('S slots))
    hist = vehicleHistory ds
    go :: Maybe (Finite vs) -> SV.Vector vs (Maybe Bock)
    go i = vd ^. _VehicleData
               & SV.imap
                   (\j h -> do
                       guard (Just j > i)
                       let n = M.findWithDefault FZ j hist
                       h ^? vCosts . ix (fin n) . bonusingFor bs BTVehicleCosts
                   )

-- | List all possible vehicle upgrades for a 'SomeDepotStatus'.
someVehicleUpgrades
    :: forall vs. ()
    => VehicleData vs
    -> Bonuses
    -> SomeDepotStatus vs
    -> ([] :.: (,) (Some TCN.Nat) :.: SV.Vector vs) (Maybe Bock)
someVehicleUpgrades vd bs = \case
    (n :: TCN.Nat slots) :=> (ds :: DepotStatus vs slots) -> (n //) $
      Comp . Comp . TCV.ifoldMap (\i x -> [(finNat i, x)]) $ vehicleUpgrades vd bs ds
