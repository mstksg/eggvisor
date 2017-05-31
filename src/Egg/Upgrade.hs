{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Egg.Upgrade (
  ) where

import           Control.Applicative
import           Data.Aeson.Encoding
import           Data.Aeson.Types
import           Data.Kind
import           Data.Yaml
import           Egg.Types
import           GHC.Generics        (Generic)
import           GHC.TypeLits
import qualified Data.HashMap.Lazy   as HM
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Vector         as V

data BonusAmount = BAIncrement Double
                 | BAPercent Double
                 | BAMultiplier Double
  deriving (Show, Eq, Ord, Generic)

bonusAmountParseOptions :: Options
bonusAmountParseOptions = defaultOptions
    { sumEncoding = TaggedObject
                      { tagFieldName      = "type"
                      , contentsFieldName = "value"
                      }
    , constructorTagModifier = camelTo2 '-' . drop 2
    }

instance FromJSON BonusAmount where
    parseJSON  = genericParseJSON  bonusAmountParseOptions
instance ToJSON BonusAmount where
    toJSON     = genericToJSON     bonusAmountParseOptions
    toEncoding = genericToEncoding bonusAmountParseOptions

scaleAmount :: Integer -> BonusAmount -> BonusAmount
scaleAmount n = \case
    BAIncrement  i -> BAIncrement  (fromIntegral n * i)
    BAPercent    p -> BAPercent    (fromIntegral n * p)
    BAMultiplier r -> BAMultiplier (r ^ n)

data BonusType =
        BTBuildCosts
      | BTDroneRewards
      | BTEggValue
      | BTFarmValue
      | BTFleetSize
      | BTHabCapacity
      | BTHatcheryCapacity
      | BTHatcheryRate
      | BTHoldingHatch
      | BTHoverVehicleCapacity
      | BTInternalHatchery
      | BTInternalHatcheryCalm
      | BTInternalHatcherySharing
      | BTLayingRate
      | BTLongWarpTime
      | BTMaxRunningBonus
      | BTPrestigeEggs
      | BTResearchCosts
      | BTRunningBonus
      | BTSiloQuality
      | BTSoulEggBonus
      | BTVehicleCapacity
      | BTVehicleCosts
      | BTVideoDoublerTime
      | BTVehicleSpeed
  deriving (Show, Eq, Ord, Generic)

bonusTypeParseOptions :: Options
bonusTypeParseOptions = defaultOptions
    { sumEncoding = UntaggedValue
    , constructorTagModifier = camelTo2 '-' . drop 2
    }

instance FromJSON BonusType where
    parseJSON  = genericParseJSON  bonusTypeParseOptions
instance ToJSON BonusType where
    toJSON     = genericToJSON     bonusTypeParseOptions
    toEncoding = genericToEncoding bonusTypeParseOptions
instance FromJSONKey BonusType where
    fromJSONKey = FromJSONKeyTextParser (parseJSON . String)
instance ToJSONKey BonusType where
    toJSONKey = toJSONKeyText $
        T.pack . constructorTagModifier bonusTypeParseOptions . show

data Bonus = Bonus { bType :: BonusType, bAmt :: BonusAmount }
    deriving (Show, Eq, Ord)

newtype Bonuses = Bonuses { bMap :: M.Map BonusType [BonusAmount] }
    deriving (Show, Eq, Ord)

instance Monoid Bonuses where
    mempty = Bonuses M.empty
    mappend (Bonuses x) (Bonuses y) = Bonuses (M.unionWith (++) x y)

data Research a =
    Research { rName        :: T.Text
             , rDescription :: T.Text
             , rBaseBonuses :: Bonuses
             , rCosts       :: Either Int (V.Vector a)
             }
  deriving (Show, Eq, Ord)

instance FromJSON a => FromJSON (Research a) where
    parseJSON = withObject "Research" $ \v ->
        Research <$> v .: "name"
                 <*> v .: "description"
                 <*> (mkBase =<< v .: "bonuses")
                 <*> (Left <$> (v .: "levels") <|> Right <$> (v .: "costs"))
      where
        mkBase :: M.Map BonusType Object -> Parser Bonuses
        mkBase = fmap (Bonuses . fmap (:[])) . traverse (.: "base-amount")

instance ToJSON a => ToJSON (Research a) where
    toJSON Research{..} = object
        [ "name"        .= rName
        , "description" .= rDescription
        , "bonuses"     .= object [ "base-amount" .= bMap rBaseBonuses ]
        , case rCosts of Left  l  -> "levels" .= l
                         Right cs -> "costs"  .= cs
        ]
    toEncoding Research{..} = pairs . mconcat $
        [ "name"        .= rName
        , "description" .= rDescription
        , pair "bonuses" (pairs ("base-amount" .= bMap rBaseBonuses))
        , case rCosts of Left  l  -> "levels" .= l
                         Right cs -> "costs"  .= cs
        ]

data ResearchData =
    ResearchData { rdCommon :: V.Vector (V.Vector (Research Double))
                 , rdEpic   :: V.Vector (Research Integer)
                 }
  deriving Show

instance FromJSON ResearchData where
    parseJSON = withObject "ResearchData" $ \v ->
      ResearchData <$> v .: "research"
                   <*> v .: "epic"
instance ToJSON ResearchData where
    toJSON ResearchData{..} = object
        [ "research" .= rdCommon
        , "epic"     .= rdEpic
        ]
    toEncoding ResearchData{..} = pairs . mconcat $
        [ "research" .= rdCommon
        , "epic"     .= rdEpic
        ]
