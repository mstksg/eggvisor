{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Egg.GameData (
    GameConstants(..), gcBaseLayingRate, gcBaseSoulEggBonus
  , GameData(..), gdEggData, gdResearchData, gdHabData, gdVehicleData, gdConstants
  , SomeGameData(..)
  ) where

import           Control.Lens hiding      ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Egg.Egg
import           Egg.Habitat
import           Egg.Research
import           Egg.Vehicle
import           GHC.Generics             (Generic)
import           Type.Family.List

data GameConstants =
    GameConstants { _gcBaseLayingRate   :: Double     -- ^ eggs per minute
                  , _gcBaseSoulEggBonus :: Double
                  }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''GameConstants

data GameData :: Nat -> ([Nat], Nat) -> Nat -> Nat -> Type where
    GameData :: { _gdEggData      :: EggData eggs
                , _gdResearchData :: ResearchData tiers epic
                , _gdHabData      :: HabData habs
                , _gdVehicleData  :: VehicleData vehicles
                , _gdConstants    :: GameConstants
                }
             -> GameData eggs '(tiers, epic) habs vehicles

deriving instance ListC (Show <$> (ResearchTier <$> tiers))
    => Show (GameData eggs '(tiers, epic) habs vehicles)

gdEggData :: Lens (GameData e1 '(t, g) h v) (GameData e2 '(t, g) h v) (EggData e1) (EggData e2)
gdEggData f gd = (\ed -> gd { _gdEggData = ed }) <$> f (_gdEggData gd)
gdResearchData :: Lens (GameData e '(t1, g1) h v) (GameData e '(t2, g2) h v) (ResearchData t1 g1) (ResearchData t2 g2)
gdResearchData f gd = f (_gdResearchData gd) <&> \rd ->
                        GameData (_gdEggData gd) rd (_gdHabData gd) (_gdVehicleData gd) (_gdConstants gd)
gdHabData :: Lens (GameData e '(t, g) h1 v) (GameData e '(t, g) h2 v) (HabData h1) (HabData h2)
gdHabData f gd = (\hd -> gd { _gdHabData = hd }) <$> f (_gdHabData gd)
gdVehicleData :: Lens (GameData e '(t, g) h v1) (GameData e '(t, g) h v2) (VehicleData v1) (VehicleData v2)
gdVehicleData f gd = (\vd -> gd { _gdVehicleData = vd }) <$> f (_gdVehicleData gd)
gdConstants :: Lens' (GameData e '(t, g) h v) GameConstants
gdConstants f gd = (\vd -> gd { _gdConstants = vd }) <$> f (_gdConstants gd)

data SomeGameData :: Type where
    SomeGameData
        :: Sing eggs
        -> Sing '(tiers, epic)
        -> Sing habs
        -> Sing vehicles
        -> GameData eggs '(tiers, epic) habs vehicles
        -> SomeGameData

gdParseOptions :: Options
gdParseOptions = defaultOptions
    { fieldLabelModifier = camelTo2 '-' . drop 3
    }

instance FromJSON GameConstants where
    parseJSON  = genericParseJSON  gdParseOptions
instance ToJSON GameConstants where
    toJSON     = genericToJSON     gdParseOptions
    toEncoding = genericToEncoding gdParseOptions

instance (KnownNat egg, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
      => FromJSON (GameData egg '(tiers, epic) habs vehicles) where
    parseJSON  = withObject "GameData" $ \v ->
        GameData <$> v .: "eggs"
                 <*> v .: "research"
                 <*> v .: "habitats"
                 <*> v .: "vehicles"
                 <*> v .: "constants"

instance ToJSON (GameData egg research habs vehicles) where
    toJSON GameData{..} = object
        [ "eggs"      .= _gdEggData
        , "research"  .= _gdResearchData
        , "habitats"  .= _gdHabData
        , "vehicles"  .= _gdVehicleData
        , "constants" .= _gdConstants
        ]
    toEncoding GameData{..} = pairs . mconcat $
        [ "eggs"      .= _gdEggData
        , "research"  .= _gdResearchData
        , "habitats"  .= _gdHabData
        , "vehicles"  .= _gdVehicleData
        , "constants" .= _gdConstants
        ]

instance FromJSON SomeGameData where
    parseJSON = withObject "GameData" $ \v -> do
      sed :=> ed       :: SomeEggData      <- v .: "eggs"
      srd :=> Uncur rd :: SomeResearchData <- v .: "research"
      shd :=> hd       :: SomeHabData      <- v .: "habitats"
      svd :=> vd       :: SomeVehicleData  <- v .: "vehicles"
      cs               :: GameConstants    <- v .: "constants"
      return $ SomeGameData sed srd shd svd $ GameData ed rd hd vd cs
instance ToJSON SomeGameData where
    toJSON     (SomeGameData _ _ _ _ gd) = toJSON     gd
    toEncoding (SomeGameData _ _ _ _ gd) = toEncoding gd
