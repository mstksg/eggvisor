{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
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
  , DepotStatus(..), _DepotStatus, SomeDepotStatus, _SomeDepotStatus
  , SomeVehicleUpgradeError(..), _SVUERegression, _SVUENoSlot
  , withSomeDepotStatus
  , initDepotStatus
  , initSomeDepotStatus
  , baseDepotCapacity
  , totalDepotCapacity
  , vehicleHistory
  , vehiclePrice
  , upgradeVehicle
  , upgradeSomeVehicle
  , vehicleUpgrades
  , someVehicleUpgrades
  ) where

import           Control.Lens hiding           ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Finite.Internal
import           Data.Maybe
import           Data.Singletons
import           Data.Singletons.Prelude.Num
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Vector.Sized.Util
import           Egg.Commodity
import           Egg.Research
import           GHC.Generics                  (Generic)
import           Numeric.Lens
import           Numeric.Natural
import           Text.Printf
import qualified Data.Map                      as M
import qualified Data.Text                     as T
import qualified Data.Vector                   as V
import qualified Data.Vector.Sized             as SV
import qualified GHC.TypeLits                  as TL

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
    = DepotStatus { _dsSlots :: M.Map (Finite slots) (Finite vs) }
  deriving (Show, Eq, Ord, Generic)

makePrisms ''DepotStatus
makeWrapped ''DepotStatus

data SomeVehicleUpgradeError vs
        = SVUERegression { _svuePrevious :: Finite vs }
        | SVUENoSlot
  deriving (Show, Eq, Ord)

makePrisms ''SomeVehicleUpgradeError

data SomeDepotStatus vs
    = SomeDepotStatus { _sdsSlots :: M.Map Integer (Finite vs) }
  deriving (Show, Eq, Ord, Generic)

_SomeDepotStatus
    :: Functor f
    => Bonuses
    -> (forall slots. KnownNat slots => LensLike' f (DepotStatus vs slots) b)
    -> LensLike' f (SomeDepotStatus vs) b
_SomeDepotStatus bs f g = traverseSomeDepotStatus bs (f g)

withSomeDepotStatus
    :: Bonuses
    -> SomeDepotStatus vs
    -> (forall slots. KnownNat slots => DepotStatus vs slots -> r)
    -> r
withSomeDepotStatus bs sd f = sd ^. _SomeDepotStatus bs (to f)

traverseSomeDepotStatus
    :: Functor f
    => Bonuses
    -> (forall slots. KnownNat slots => DepotStatus vs slots -> f (DepotStatus vs slots))
    -> SomeDepotStatus vs
    -> f (SomeDepotStatus vs)
traverseSomeDepotStatus bs f sds0 = withSomeSing fleetSize $ \(SNat :: Sing slots) ->
    let ds0 = DepotStatus
            . M.mapKeysMonotonic Finite
            . M.filterWithKey (\k _ -> k < fleetSize)
            $ _sdsSlots sds0
    in  SomeDepotStatus . M.mapKeysMonotonic getFinite . _dsSlots
          <$> f @slots ds0
  where
    fleetSize :: Integer
    fleetSize = 4 ^. bonusingFor @Double bs BTFleetSize . to round

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
initDepotStatus :: (KnownNat vs, KnownNat slots, 1 TL.<= vs, 1 TL.<= slots) => DepotStatus vs slots
initDepotStatus = DepotStatus $ M.singleton 0 0

-- | Initial 'SomeDepotStatus' to start off the game.
initSomeDepotStatus :: (KnownNat vs, 1 TL.<= vs) => SomeDepotStatus vs
initSomeDepotStatus = SomeDepotStatus $ M.singleton 0 0

-- | Total base capacity of all slots, in eggs per second.
baseDepotCapacity
    :: forall vs slots. KnownNat vs
    => VehicleData vs
    -> DepotStatus vs slots
    -> Double
baseDepotCapacity VehicleData{..} =
    sumOf $ _DepotStatus
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
    :: KnownNat (slots TL.+ 1)
    => DepotStatus vs slots
    -> M.Map (Finite vs) (Finite (slots TL.+ 1))
vehicleHistory = M.fromListWith (+)
               . toListOf (_DepotStatus . folded . to (, 1))

-- | Get the BASE price of a given vehicle, if a purchase were to be made.
-- Does not check if purchase is legal (see 'upgradeVehicle').
vehiclePrice
    :: forall vs slots. (KnownNat vs, KnownNat slots)
    => VehicleData vs
    -> DepotStatus vs slots
    -> Finite vs
    -> Maybe Bock
vehiclePrice vd ds v = withKnownNat (SNat @slots %:+ SNat @1) $
                           fmap priceOf
                         . strengthen
                         . fromMaybe 0
                         . M.lookup v
                         . vehicleHistory
                         $ ds
  where
    priceOf :: Finite slots -> Bock
    priceOf i = fromMaybe 0 $
                  vd ^? _VehicleData . ixSV v . vCosts . ix (fromIntegral i)

-- | Purchase a vehicle upgrade.  Returns cost and new depot status,
-- if purchase is valid.
--
-- Purchase is invalid if purchasing a vehicle in a slot where a greater
-- vehicle is already purchased.
upgradeVehicle
    :: (KnownNat vs, KnownNat slots)
    => VehicleData vs
    -> Bonuses
    -> Finite slots
    -> Finite vs
    -> DepotStatus vs slots
    -> Either (Finite vs) (Bock, DepotStatus vs slots)
upgradeVehicle vd bs slot v ds0 =
    getComp . flip (_DepotStatus . at slot) ds0 $ \s0 -> Comp $ do
      case s0 of
        Just h | h >= v -> Left h
        _               -> Right ()
      -- should always be valid of the previous condition is true
      let price = fromJust $ vehiclePrice vd ds0 v
      return (price ^. bonusingFor bs BTVehicleCosts, Just v)

-- | Purchase a vehicle upgrade.  Returns cost and new depot status,
-- if purchase is valid.
--
-- Purchase is invalid if purchasing a vehicle in a slot where a greater
-- vehicle is already purchased, and also if there is no such slot.
upgradeSomeVehicle
    :: forall vs. KnownNat vs
    => VehicleData vs
    -> Bonuses
    -> Integer
    -> Finite vs
    -> SomeDepotStatus vs
    -> Either (SomeVehicleUpgradeError vs) (Bock, SomeDepotStatus vs)
upgradeSomeVehicle vd bs slot v sds0 =
    getComp . traverseSomeDepotStatus bs (Comp . go) $ sds0
  where
    go  :: KnownNat slots
        => DepotStatus vs slots
        -> Either (SomeVehicleUpgradeError vs) (Bock, DepotStatus vs slots)
    go ds0 = case packFinite slot of
      Nothing    -> Left SVUENoSlot
      Just slot' -> either (Left . SVUERegression) Right $
                      upgradeVehicle vd bs slot' v ds0

-- | List all possible vehicle upgrades.
vehicleUpgrades
    :: forall vs slots. KnownNat slots
    => VehicleData vs
    -> Bonuses
    -> DepotStatus vs slots
    -> (SV.Vector slots :.: SV.Vector vs :.: Either (Finite vs)) Bock
vehicleUpgrades vd bs ds = Comp . Comp $ SV.generate go
  where
    slots1 = SNat @slots %:+ SNat @1
    hist :: M.Map (Finite vs) (Finite (slots TL.+ 1))
    hist = withKnownNat slots1 $
             vehicleHistory ds
    go :: Finite slots -> SV.Vector vs (Either (Finite vs) Bock)
    go s = vd ^. _VehicleData
              & SV.imap
                  (\j h -> withKnownNat slots1 $ do
                      case currVeh of
                        Just v | v >= j -> Left v
                        _               -> Right ()
                      let n = M.findWithDefault 0 j hist
                      return . fromMaybe 0 $
                            h ^? vCosts
                               . ix (fromIntegral n)
                               . bonusingFor bs BTVehicleCosts
                  )
      where
        currVeh = ds ^? _DepotStatus . ix s

-- | List all possible vehicle upgrades for a 'SomeDepotStatus'.
someVehicleUpgrades
    :: forall vs. ()
    => VehicleData vs
    -> Bonuses
    -> SomeDepotStatus vs
    -> (V.Vector :.: SV.Vector vs :.: Either (Finite vs)) Bock
someVehicleUpgrades vd bs = view $ _SomeDepotStatus bs go
                                 . _Unwrapped
                                 . _Unwrapped
  where
    go  :: KnownNat slots
        => Getter (DepotStatus vs slots)
                  (V.Vector (SV.Vector vs (Either (Finite vs) Bock)))
    go = to (vehicleUpgrades vd bs)
       . _Wrapped
       . _Wrapped
       . to SV.fromSized
