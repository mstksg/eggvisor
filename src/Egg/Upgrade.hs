{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Egg.Upgrade (
    BonusAmount(..)
  , BonusType(..)
  , Bonuses(..)
  , Research(..)
  , ResearchData(..)
  , ResearchStatus(..)
  , ResearchIx(..)
  , scaleAmount
  , hasEffect
  , bonuses
  , maxLevel
  , emptyStatus
  , researchBonuses
  , totalBonuses
  , foldResearch
  , purchaseResearch
  , researchIxData
  , researchIxStatus
  ) where

import           Control.Applicative hiding      (some)
import           Control.Lens hiding             ((.=), ix)
import           Data.Aeson.Encoding
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Foldable
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude hiding  (Flip)
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Type.Combinator.Singletons
import           Data.Type.Combinator.Util
import           Data.Type.Conjunction
import           Data.Type.Product               as TCP
import           Data.Type.Sum
import           Data.Type.Vector
import           GHC.Generics                    (Generic)
import           Numeric.Natural
import           Type.Class.Higher
import           Type.Class.Witness
import qualified Data.Map                        as M
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Data.Vector.Sized               as SV

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

data ResearchData :: [Nat] -> Nat -> Type where
    ResearchData
        :: { rdCommon :: Prod (Flip SV.Vector (Research Double)) tiers
           , rdEpic   :: SV.Vector epic (Research Integer)
           }
        -> ResearchData tiers epic
    deriving (Show, Eq, Ord)

type SomeResearchData = DSum Sing (Uncur ResearchData)

data ResearchStatus :: [Nat] -> Nat -> Type where
    ResearchStatus
        :: { rsCommon :: Prod (Flip SV.Vector Natural) tiers
           , rsEpic   :: SV.Vector epic Natural
           }
        -> ResearchStatus tiers epic
  deriving (Show, Eq, Ord)

data ResearchIx :: [Nat] -> Nat -> Type -> Type where
    RICommon :: Sum Finite tiers -> ResearchIx tiers epic Double
    RIEpic   :: Finite epic      -> ResearchIx tiers epic Integer

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
      Bonuses (M.unionWith (++) x y)

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

instance FromJSON SomeResearchData where
    parseJSON = withObject "ResearchData" $ \v -> do
        res   <- v .: "research"
        epics <- v .: "epic"
        withV res $ \resV ->
          some (withProd (f . getI) resV) $ \(_ :&: (unzipP->(resS :&: resP))) ->
            SV.withSized epics   $ \sizedV ->
              return (STuple2 (prodSing resS) SNat :=> Uncur (ResearchData resP sizedV))
      where
        f   :: V.Vector (Research Double)
            -> Some (Sing :&: Flip SV.Vector (Research Double))
        f xs = SV.withSized xs $ \xsV -> Some (SNat :&: Flip xsV)

instance ToJSON SomeResearchData where
    toJSON = \case
        _ :=> Uncur r -> toJSON r
    toEncoding = \case
        _ :=> Uncur r -> toEncoding r
instance ToJSON (ResearchData tiers epic) where
    toJSON ResearchData{..} = object
        [ "research" .= TCP.toList (SV.fromSized . getFlip) rdCommon
        , "epic"     .= SV.fromSized rdEpic
        ]
    toEncoding ResearchData{..} = pairs . mconcat $
        [ "research" .= TCP.toList (SV.fromSized . getFlip) rdCommon
        , "epic"     .= SV.fromSized rdEpic
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

bonuses :: M.Map BonusType [BonusAmount] -> Bonuses
bonuses = Bonuses . M.filter (not . null) . fmap (filter hasEffect)

maxLevel :: Research a -> Int
maxLevel = either fromIntegral V.length . rCosts

emptyStatus :: ResearchData tiers epic -> ResearchStatus tiers epic
emptyStatus ResearchData{..} =
    ResearchStatus (map1 (mapFlip (0 <$)) rdCommon) (0 <$ rdEpic)

foldResearch
    :: Monoid b
    => (Either (Research Double) (Research Integer) -> Natural -> b)
    -> ResearchData tiers epic
    -> ResearchStatus tiers epic
    -> b
foldResearch f ResearchData{..} ResearchStatus{..} = mconcat
    [ foldMap1 (\case Flip d :&: Flip s -> fold $ SV.zipWith (f . Left) d s
               )
        (zipP (rdCommon :&: rsCommon))
    , fold $ SV.zipWith (f . Right) rdEpic rsEpic
    ]

researchBonuses :: Research a -> Natural -> Bonuses
researchBonuses _ 0 = Bonuses M.empty
researchBonuses Research{..} s = case rBaseBonuses of
    Bonuses m -> Bonuses $ map (scaleAmount s) <$> m

totalBonuses :: ResearchData tiers epic -> ResearchStatus tiers epic -> Bonuses
totalBonuses = foldResearch (either researchBonuses researchBonuses)

purchaseResearch
    :: (KnownNat epic, SingI tiers)
    => ResearchData tiers epic
    -> ResearchIx tiers epic a
    -> ResearchStatus tiers epic
    -> (Maybe a, ResearchStatus tiers epic)
purchaseResearch rd ix = researchIxStatus ix $ \currLevel ->
    case view (researchIxData ix . to rCosts) rd of
      Left m   -> if currLevel < m
        then (Just 0 , currLevel + 1) \\ researchIxNum ix
        else (Nothing, currLevel)
      Right cs -> case cs V.!? fromIntegral (currLevel + 1) of
        Just c  -> (Just c , currLevel + 1)
        Nothing -> (Nothing, currLevel)

researchIxNum :: ResearchIx tiers epic a -> Wit (Num a)
researchIxNum = \case
    RICommon _ -> Wit
    RIEpic   _ -> Wit

ixSV :: KnownNat n => Finite n -> Lens' (SV.Vector n a) a
ixSV ix f v = (\x -> v SV.// [(fromIntegral ix, x)]) <$> f (SV.index v ix)

_rdCommon :: Lens (ResearchData t1 e)
                  (ResearchData t2 e)
                  (Prod (Flip SV.Vector (Research Double)) t1)
                  (Prod (Flip SV.Vector (Research Double)) t2)
_rdCommon f rd = (\v -> rd { rdCommon = v }) <$> f (rdCommon rd)

_rdEpic :: Lens (ResearchData t e1)
                (ResearchData t e2)
                (SV.Vector e1 (Research Integer))
                (SV.Vector e2 (Research Integer))
_rdEpic f rd = (\v -> rd { rdEpic = v }) <$> f (rdEpic rd)

_rsCommon :: Lens (ResearchStatus t1 e)
                  (ResearchStatus t2 e)
                  (Prod (Flip SV.Vector Natural) t1)
                  (Prod (Flip SV.Vector Natural) t2)
_rsCommon f rs = (\v -> rs { rsCommon = v }) <$> f (rsCommon rs)

_rsEpic :: Lens (ResearchStatus t e1) (ResearchStatus t e2) (SV.Vector e1 Natural) (SV.Vector e2 Natural)
_rsEpic f rs = (\v -> rs { rsEpic = v }) <$> f (rsEpic rs)

researchIxData
    :: (KnownNat epic, SingI tiers)
    => ResearchIx tiers epic a
    -> Lens' (ResearchData tiers epic) (Research a)
researchIxData = \case
    RICommon ix -> \f ->
      let g :: forall a. _ a -> _ (_ a)
          g x@(slot :&: (_ :&: SNat)) = (_2 . _1 . _Flip . ixSV slot) f x
      in  _rdCommon $ \rs -> map1 fanFst . fanSnd
            <$> sumProd g (ix :&: zipP (rs :&: singProd sing))
    RIEpic ix   -> _rdEpic . ixSV ix

researchIxStatus
    :: (KnownNat epic, SingI tiers)
    => ResearchIx tiers epic a
    -> Lens' (ResearchStatus tiers epic) Natural
researchIxStatus = \case
    RICommon ix -> \f ->
      let g :: forall a. ()
            => (Finite :&: (Flip SV.Vector Natural :&: Sing)) a
            -> _ ((Finite :&: (Flip SV.Vector Natural :&: Sing)) a)
          g x@(slot :&: (_ :&: SNat)) = (_2 . _1 . _Flip . ixSV slot) f x
      in  _rsCommon $ \rs -> map1 fanFst . fanSnd
            <$> sumProd g (ix :&: zipP (rs :&: singProd sing))
    RIEpic ix   -> _rsEpic . ixSV ix
