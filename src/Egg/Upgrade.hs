{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Egg.Upgrade (
    BonusAmount(..)
  , BonusType(..)
  , Bonuses(..)
  , Research(..)
  , ResearchData(..)
  , ResearchStatus(..)
  , scaleAmount
  , hasEffect
  , bonuses
  , maxLevel
  , emptyStatus
  , researchBonuses
  , totalBonuses
  , foldResearch
  ) where

import           Control.Applicative
import           Data.Aeson.Encoding
import           Data.Aeson.Types
import           Data.Foldable
import           Data.Kind
import           Data.Yaml
import           Egg.Types
import           GHC.Generics        (Generic)
import           GHC.TypeLits
import           Numeric.Natural
import qualified Data.HashMap.Lazy   as HM
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Vector         as V

data BonusAmount = BAIncrement Double
                 | BAPercent Double
                 | BAMultiplier Double
  deriving (Show, Eq, Ord, Generic)

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

newtype Bonuses = Bonuses { bMap :: M.Map BonusType [BonusAmount] }
    deriving (Show, Eq, Ord)

data Research a =
    Research { rName        :: T.Text
             , rDescription :: T.Text
             , rBaseBonuses :: Bonuses
             , rCosts       :: Either Natural (V.Vector a)
             }
  deriving (Show, Eq, Ord)

data ResearchData =
    ResearchData { rdCommon :: V.Vector (V.Vector (Research Double))
                 , rdEpic   :: V.Vector (Research Integer)
                 }
  deriving Show

data ResearchStatus =
    ResearchStatus { rsCommon :: V.Vector (V.Vector Natural)
                   , rsEpic   :: V.Vector Natural
                   }
  deriving Show

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

instance Monoid Bonuses where
    mempty = Bonuses M.empty
    mappend (Bonuses x) (Bonuses y) =
      bonuses (M.unionWith (++) x y)

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


scaleAmount :: Natural -> BonusAmount -> BonusAmount
scaleAmount n = \case
    BAIncrement  i -> BAIncrement  (fromIntegral n * i)
    BAPercent    p -> BAPercent    (fromIntegral n * p)
    BAMultiplier r -> BAMultiplier (r ^ n)

hasEffect :: BonusAmount -> Bool
hasEffect = \case
    BAIncrement  i -> i /= 0
    BAPercent    p -> p /= 0
    BAMultiplier r -> r /= 1

-- | Might be a bottleneck
bonuses :: M.Map BonusType [BonusAmount] -> Bonuses
bonuses = Bonuses . M.filter (not . null) . fmap (filter hasEffect)

maxLevel :: Research a -> Int
maxLevel = either fromIntegral V.length . rCosts

emptyStatus :: ResearchData -> ResearchStatus
emptyStatus ResearchData{..} =
    ResearchStatus (fmap (0 <$) rdCommon) (0 <$ rdEpic)

foldResearch
    :: Monoid b
    => (Either (Research Double) (Research Integer) -> Natural -> b)
    -> ResearchData
    -> ResearchStatus
    -> b
foldResearch f ResearchData{..} ResearchStatus{..} = mconcat
    [ (foldZip . foldZip) (f . Left) rdCommon rsCommon
    , foldZip (f . Right) rdEpic rsEpic
    ]
  where
    foldZip :: Monoid c => (a -> b -> c) -> V.Vector a -> V.Vector b -> c
    foldZip f x y = fold $ V.zipWith f x y

researchBonuses :: Research a -> Natural -> Bonuses
researchBonuses Research{..} s = case rBaseBonuses of
    Bonuses m -> Bonuses $ map (scaleAmount s) <$> m

totalBonuses :: ResearchData -> ResearchStatus -> Bonuses
totalBonuses = foldResearch (either researchBonuses researchBonuses)
