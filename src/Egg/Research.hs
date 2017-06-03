{-# LANGUAGE DataKinds                            #-}
{-# LANGUAGE DeriveGeneric                        #-}
{-# LANGUAGE FlexibleContexts                     #-}
{-# LANGUAGE FlexibleInstances                    #-}
{-# LANGUAGE FunctionalDependencies               #-}
{-# LANGUAGE GADTs                                #-}
{-# LANGUAGE KindSignatures                       #-}
{-# LANGUAGE LambdaCase                           #-}
{-# LANGUAGE MultiParamTypeClasses                #-}
{-# LANGUAGE OverloadedStrings                    #-}
{-# LANGUAGE PartialTypeSignatures                #-}
{-# LANGUAGE RankNTypes                           #-}
{-# LANGUAGE RecordWildCards                      #-}
{-# LANGUAGE ScopedTypeVariables                  #-}
{-# LANGUAGE TemplateHaskell                      #-}
{-# LANGUAGE TupleSections                        #-}
{-# LANGUAGE TypeApplications                     #-}
{-# LANGUAGE TypeFamilies                         #-}
{-# LANGUAGE TypeOperators                        #-}
{-# LANGUAGE TypeSynonymInstances                 #-}
{-# LANGUAGE ViewPatterns                         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Egg.Research (
    BonusAmount(..), AsBonusAmount(..)
  , BonusType(..), AsBonusType(..)
  , Bonuses(..), _Bonuses
  , Research(..), HasResearch(..)
  , ResearchData(..), rdCommon, rdEpic, SomeResearchData
  , ResearchStatus(..), rsCommon, rsEpic, SomeResearchStatus
  , ResearchIx(..)
  , scaleAmount
  , hasEffect
  , bonusEffect
  , bonusEffectFor
  , bonusing
  , bonusingFor
  , emptyBonuses
  , bonuses
  , maxLevel
  , emptyResearchStatus
  , researchBonuses
  , totalBonuses
  , foldResearch
  , purchaseResearch
  , researchIxNum
  , researchIxData
  , researchIxStatus
  , withSomeResearch
  ) where

import           Control.Applicative hiding      (some)
import           Control.Lens hiding             ((.=), (:<))
import           Control.Monad
import           Control.Monad.Trans.Writer
import           Data.Aeson.Encoding
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Foldable
import           Data.Kind
import           Data.Singletons
import           Data.Singletons.Prelude hiding  (Flip)
import           Data.Singletons.TypeLits
import           Data.Tuple
import           Data.Type.Combinator
import           Data.Type.Combinator.Singletons
import           Data.Type.Combinator.Util
import           Data.Type.Conjunction
import           Data.Type.Product               as TCP
import           Data.Type.Sum
import           Data.Type.Vector
import           Data.Vector.Sized.Util
import           GHC.Generics                    (Generic)
import           Numeric.Lens
import           Numeric.Natural
import           Type.Class.Higher
import           Type.Class.Witness
import qualified Data.Map                        as M
import qualified Data.Text                       as T
import qualified Data.Vector                     as V
import qualified Data.Vector.Sized               as SV

data BonusAmount
    -- | Bonus adds an absolute amount.  Scales by multiplication, and
    -- compounds by addition.
    = BAIncrement Double
    -- | Bonus adds a percentage point multiplier.  Scales by
    -- multiplication (three 10% buffs is a 15% buff) and compounds by
    -- appropriate multiplication.
    | BAPercent Double
    -- | Bonus multiplies by a given ratio.  Scales by exponentiation
    -- (three 2x buffs is a 8x buff) and compounds by multiplication.
    | BAMultiplier Double
  deriving (Show, Eq, Ord, Generic)

makeClassyPrisms ''BonusAmount

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

makeClassyPrisms ''BonusType

newtype Bonuses = Bonuses { _bMap :: M.Map BonusType [BonusAmount] }
    deriving (Show, Eq, Ord)

makePrisms ''Bonuses
makeWrapped ''Bonuses

data Research a =
    Research { _rName        :: T.Text
             , _rDescription :: T.Text
             , _rBaseBonuses :: Bonuses
             -- | 'Left' if no cost data, just giving the maximum level.
             -- 'Right' with cost data.
             , _rCosts       :: Either Natural (V.Vector a)
             }
  deriving (Show, Eq, Ord)

makeClassy ''Research

data ResearchData :: [Nat] -> Nat -> Type where
    ResearchData
        :: { _rdCommon :: Prod (Flip SV.Vector (Research Double)) tiers
           , _rdEpic   :: SV.Vector epic (Research Integer)
           }
        -> ResearchData tiers epic
    deriving (Show, Eq, Ord)

makeLenses ''ResearchData

type SomeResearchData = DSum Sing (Uncur ResearchData)

data ResearchStatus :: [Nat] -> Nat -> Type where
    ResearchStatus
        :: { _rsCommon :: Prod (Flip SV.Vector Natural) tiers
           , _rsEpic   :: SV.Vector epic Natural
           }
        -> ResearchStatus tiers epic
  deriving (Show, Eq, Ord)

makeLenses ''ResearchStatus

type SomeResearchStatus = DSum Sing (Uncur ResearchStatus)

-- | A safe index for a given research item, usable with 'ResearchData' and
-- 'ResearchStatus'.
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
        [ "name"        .= _rName
        , "description" .= _rDescription
        , "bonuses"     .= object [ "base-amount" .= _bMap _rBaseBonuses ]
        , case _rCosts of Left  l  -> "levels" .= l
                          Right cs -> "costs"  .= cs
        ]
    toEncoding Research{..} = pairs . mconcat $
        [ "name"        .= _rName
        , "description" .= _rDescription
        , pair "bonuses" (pairs ("base-amount" .= _bMap _rBaseBonuses))
        , case _rCosts of Left  l  -> "levels" .= l
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
instance (SingI tiers, KnownNat epic) => FromJSON (ResearchData tiers epic) where
    parseJSON = withObject "ResearchData" $ \v -> do
        res   <- v .: "research"
        resV  <- go sing res
        epics <- v .: "epic"
        epicsV <- case SV.toSized epics of
          Nothing -> fail "Bad number of items in list."
          Just eV -> return eV
        return $ ResearchData resV epicsV
      where
        go :: Sing ts -> [Value] -> Parser (Prod (Flip SV.Vector (Research Double)) ts)
        go = \case
          SNil -> \case
            []  -> return Ø
            _:_ -> fail "Too many items in list"
          SNat `SCons` ss -> \case
            []   -> fail "Too few items in list"
            x:xs -> (:<) <$> parseJSON x <*> go ss xs

instance ToJSON (ResearchData tiers epic) where
    toJSON ResearchData{..} = object
        [ "research" .= TCP.toList (SV.fromSized . getFlip) _rdCommon
        , "epic"     .= SV.fromSized _rdEpic
        ]
    toEncoding ResearchData{..} = pairs . mconcat $
        [ "research" .= TCP.toList (SV.fromSized . getFlip) _rdCommon
        , "epic"     .= SV.fromSized _rdEpic
        ]


-- | Scales a bonus amount by a "level".
scaleAmount :: Natural -> BonusAmount -> BonusAmount
scaleAmount n = \case
    BAIncrement  i -> BAIncrement  (fromIntegral n * i)
    BAPercent    p -> BAPercent    (fromIntegral n * p)
    BAMultiplier r -> BAMultiplier (r ^ n)

-- | Tells whether or not a bonus amount creates any actual effect.
hasEffect :: BonusAmount -> Bool
hasEffect = \case
    BAIncrement  i -> i /= 0
    BAPercent    p -> p /= 0
    BAMultiplier r -> r /= 1

-- | A "smart constructor" that clears bonuses with no effect.
bonuses :: M.Map BonusType [BonusAmount] -> Bonuses
bonuses = Bonuses . M.filter (not . null) . fmap (filter hasEffect)

-- | Apply a list of bonuses (from left to right) to a value.
bonusEffect :: [BonusAmount] -> Double -> Double
bonusEffect = flip . foldl' $ \e -> \case
    BAIncrement  i -> e + i
    BAPercent    p -> e * (1 + p / 100)
    BAMultiplier r -> e * r

-- | Invert a list of left-to-right bonuses on a value.
reverseBonusEffect :: [BonusAmount] -> Double -> Double
reverseBonusEffect = flip . foldr $ \case
    BAIncrement  i -> subtract i
    BAPercent    p -> (/ (1 + (p / 100)))
    BAMultiplier r -> (/ r)

-- | Iso on a value under a list of bonuses (from left to right).
bonusing :: [BonusAmount] -> Iso' Double Double
bonusing = flip foldl' id $ \e -> \case
             BAIncrement i  -> e . adding i
             BAPercent   p  -> e . multiplying (1 + p / 100)
             BAMultiplier r -> e . multiplying r
-- = flip . foldl' $ \e -> \case
--     BAIncrement i -> e . adding i
-- bonusing bs = iso (bonusEffect bs) (reverseBonusEffect bs)

-- | Iso on a value under the bonuses of a given bonus type.
bonusingFor
    :: Bonuses
    -> BonusType
    -> Iso' Double Double
bonusingFor bs bt = case M.lookup bt (_bMap bs) of
                      Nothing -> id
                      Just bl -> bonusing bl

-- | Apply bonuses for a given type on a value.
bonusEffectFor :: Bonuses -> BonusType -> Double -> Double
bonusEffectFor bs bt = maybe id bonusEffect $ M.lookup bt (_bMap bs)

-- | No bonuses
emptyBonuses :: Bonuses
emptyBonuses = Bonuses M.empty

-- | Maximum level for a given research.
maxLevel :: Research a -> Int
maxLevel = either fromIntegral V.length . _rCosts

-- | Empty status (no research).
emptyResearchStatus :: ResearchData tiers epic -> ResearchStatus tiers epic
emptyResearchStatus ResearchData{..} =
    ResearchStatus (map1 (mapFlip (0 <$)) _rdCommon) (0 <$ _rdEpic)

-- | Zips together all research data and research statuses into an
-- accumulator.
foldResearch
    :: Monoid b
    => (Either (Research Double) (Research Integer) -> Natural -> b)
    -> ResearchData tiers epic
    -> ResearchStatus tiers epic
    -> b
foldResearch f ResearchData{..} ResearchStatus{..} = mconcat
    [ foldMap1 (\case Flip d :&: Flip s -> fold $ SV.zipWith (f . Left) d s
               )
        (zipP (_rdCommon :&: _rsCommon))
    , fold $ SV.zipWith (f . Right) _rdEpic _rsEpic
    ]

-- | Bonuses from a given 'Research' at a given level.  Assumes no "maximum
-- level".
researchBonuses :: Research a -> Natural -> Bonuses
researchBonuses _ 0 = Bonuses M.empty
researchBonuses r l = _rBaseBonuses r & _Bonuses . traverse . traverse %~ scaleAmount l

-- | Total bonuses from a given 'ResearchStatus'.
totalBonuses :: ResearchData tiers epic -> ResearchStatus tiers epic -> Bonuses
totalBonuses = foldResearch (either researchBonuses researchBonuses)

-- | Purchase research at a given index, incrementing the counter in the
-- 'ResearchStatus'.  Returns 'Nothing' if research is already maxed out.
purchaseResearch
    :: (KnownNat epic, SingI tiers)
    => ResearchData tiers epic
    -> ResearchIx tiers epic a
    -> ResearchStatus tiers epic
    -> Maybe (a, ResearchStatus tiers epic)
purchaseResearch rd i =
    fmap swap . runWriterT . researchIxStatus i (\currLevel -> WriterT $
      case rd ^. researchIxData i . rCosts of
        Left m   -> (currLevel + 1, 0) <$ guard (currLevel < m)
                        \\ researchIxNum i
        Right cs -> (currLevel + 1,) <$> (cs V.!? fromIntegral (currLevel + 1))
    )

-- | Get a 'Num' instance from a 'ResearchIx'.
researchIxNum :: ResearchIx tiers epic a -> Wit (Num a)
researchIxNum = \case
    RICommon _ -> Wit
    RIEpic   _ -> Wit

-- | A lens into a 'ResearchData', given the appropriate index.
researchIxData
    :: (KnownNat epic, SingI tiers)
    => ResearchIx tiers epic a
    -> Lens' (ResearchData tiers epic) (Research a)
researchIxData = \case
    RICommon i -> \f ->
      let g :: forall a. _ a -> _ (_ a)
          g x@(slot :&: (_ :&: SNat)) = (_2 . _1 . _Flip . ixSV slot) f x
      in  rdCommon $ \rs -> map1 fanFst . fanSnd
            <$> sumProd g (i :&: zipP (rs :&: singProd sing))
    RIEpic i   -> rdEpic . ixSV i

-- | A lens into a 'ResearchStatus', given the appropriate index.
researchIxStatus
    :: (KnownNat epic, SingI tiers)
    => ResearchIx tiers epic a
    -> Lens' (ResearchStatus tiers epic) Natural
researchIxStatus = \case
    RICommon i -> \f ->
      let g :: forall a. _ a -> _ (_ a)
          g x@(slot :&: (_ :&: SNat)) = (_2 . _1 . _Flip . ixSV slot) f x
      in  rsCommon $ \rs -> map1 fanFst . fanSnd
            <$> sumProd g (i :&: zipP (rs :&: singProd sing))
    RIEpic i   -> rsEpic . ixSV i

withSomeResearch
    :: DSum Sing (Uncur res)
    -> (forall tiers epic. (KnownNat epic, SingI tiers) => res tiers epic -> r)
    -> r
withSomeResearch = \case
    STuple2 sTs SNat :=> Uncur r -> \f -> withSingI sTs $ f r
