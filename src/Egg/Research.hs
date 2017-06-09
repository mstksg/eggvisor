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
{-# LANGUAGE PolyKinds                            #-}
{-# LANGUAGE RankNTypes                           #-}
{-# LANGUAGE RecordWildCards                      #-}
{-# LANGUAGE ScopedTypeVariables                  #-}
{-# LANGUAGE StandaloneDeriving                   #-}
{-# LANGUAGE TemplateHaskell                      #-}
{-# LANGUAGE TupleSections                        #-}
{-# LANGUAGE TypeApplications                     #-}
{-# LANGUAGE TypeFamilies                         #-}
{-# LANGUAGE TypeOperators                        #-}
{-# LANGUAGE TypeSynonymInstances                 #-}
{-# LANGUAGE UndecidableInstances                 #-}
{-# LANGUAGE ViewPatterns                         #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Egg.Research (
    BonusAmount(..), AsBonusAmount(..)
  , BonusType(..), AsBonusType(..)
  , Bonuses(..), _Bonuses
  , Research(..), HasResearch(..)
  , ResearchTier(..), rtUnlock, rtTechs, SomeResearchTier
  , ResearchData(..), rdCommon, rdEpic, SomeResearchData
  , ResearchStatus(..), rsCommon, rsEpic, SomeResearchStatus
  , ResearchIx(..), _RICommon, _RIEpic
  , ResearchError(..), _REMaxedOut, _RELocked
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
  , researchCount
  , legalTiers
  , foldResearch
  , purchaseResearch
  , researchIxNum
  , researchIxData
  , researchIxStatus
  , researchIxStatusLegal
  , withSomeResearch
  , researchIxesCommon
  , researchIxesEpic
  , legalResearchIxesCommon
  , legalResearchIxesEpic
  -- * Why?
  , rdrsCommon
  , rdrsEpic
  , researchIx
  ) where

import           Control.Applicative hiding      (some)
import           Control.Lens hiding             ((.=), (:<), Index)
import           Control.Monad
import           Data.Aeson.Encoding
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Foldable
import           Data.Kind
import           Data.Monoid                     (First(..))
import           Data.Singletons
import           Data.Singletons.Prelude hiding  (Flip)
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Type.Combinator.Singletons
import           Data.Type.Combinator.Util
import           Data.Type.Conjunction
import           Data.Type.Index
import           Data.Type.Product               as TCP
import           Data.Type.Sum
import           Data.Type.Vector
import           Data.Vector.Sized.Util
import           Egg.Commodity
import           GHC.Generics                    (Generic)
import           Numeric.Lens
import           Numeric.Natural
import           Type.Class.Higher
import           Type.Class.Witness
import           Type.Family.Tuple
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
  deriving (Show, Eq, Ord, Generic)

makeClassy ''Research

data ResearchTier :: Nat -> Type where
    ResearchTier
        :: { _rtUnlock :: Natural
           , _rtTechs  :: SV.Vector n (Research Bock)
           }
        -> ResearchTier n
  deriving (Show, Eq, Ord, Generic)

makeLenses ''ResearchTier

type SomeResearchTier = DSum Sing ResearchTier

data ResearchData :: [Nat] -> Nat -> Type where
    ResearchData
        :: { _rdCommon :: Prod ResearchTier tiers
           , _rdEpic   :: SV.Vector epic (Research GoldenEgg)
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
    RICommon :: Sum Finite tiers -> ResearchIx tiers epic Bock
    RIEpic   :: Finite epic      -> ResearchIx tiers epic GoldenEgg

deriving instance Show (Sum Finite tiers) => Show (ResearchIx tiers epic a)

_RICommon :: Iso (ResearchIx t1 epic Bock) (ResearchIx t2 epic Bock)
                 (Sum Finite t1          ) (Sum Finite t2          )
_RICommon = iso (\case RICommon i -> i) RICommon

_RIEpic :: Iso (ResearchIx tiers e1 GoldenEgg) (ResearchIx tiers e2 GoldenEgg)
               (Finite e1                    ) (Finite e2                    )
_RIEpic = iso (\case RIEpic i -> i) RIEpic

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

researchTierParseOptions :: Options
researchTierParseOptions = defaultOptions
    { fieldLabelModifier = camelTo2 '-' . drop 3
    }

instance KnownNat n => FromJSON (ResearchTier n) where
    parseJSON  = genericParseJSON  researchTierParseOptions
instance KnownNat n => ToJSON (ResearchTier n) where
    toJSON     = genericToJSON     researchTierParseOptions
    toEncoding = genericToEncoding researchTierParseOptions
instance FromJSON SomeResearchTier where
    parseJSON = withObject "ResearchTier" $ \v -> do
      u <- v .: "unlock"
      t <- v .: "techs"
      SV.withSized t $ \tV ->
        return $ sing :=> ResearchTier u tV
instance ToJSON SomeResearchTier where
    toJSON = \case
        SNat :=> r -> toJSON r
    toEncoding = \case
        SNat :=> r -> toEncoding r
instance FromJSON SomeResearchData where
    parseJSON = withObject "ResearchData" $ \v -> do
        res   <- v .: "common"
        epics <- v .: "epic"
        withV res $ \resV ->
          some (withProd (dsumSome . getI) resV) $ \(_ :&: (unzipP->(resS :&: resP))) ->
            SV.withSized epics   $ \sizedV ->
              return (STuple2 (prodSing resS) SNat :=> Uncur (ResearchData resP sizedV))
instance ToJSON SomeResearchData where
    toJSON = \case
        _ :=> Uncur r -> toJSON r
    toEncoding = \case
        _ :=> Uncur r -> toEncoding r
instance (SingI tiers, KnownNat epic) => FromJSON (ResearchData tiers epic) where
    parseJSON = withObject "ResearchData" $ \v -> do
        res   <- v .: "common"
        resV  <- go sing res
        epics <- v .: "epic"
        epicsV <- case SV.toSized epics of
          Nothing -> fail "Bad number of items in list."
          Just eV -> return eV
        return $ ResearchData resV epicsV
      where
        go :: Sing ts -> [Value] -> Parser (Prod ResearchTier ts)
        go = \case
          SNil -> \case
            []  -> return Ø
            _:_ -> fail "Too many items in list"
          SNat `SCons` ss -> \case
            []   -> fail "Too few items in list"
            x:xs -> (:<) <$> parseJSON x <*> go ss xs

instance ToJSON (ResearchData tiers epic) where
    toJSON ResearchData{..} = object
        [ "common" .= TCP.toList (\ResearchTier{..} ->
                          object [ "unlock" .= _rtUnlock
                                 , "techs"  .= SV.fromSized _rtTechs
                                 ]
                        ) _rdCommon
        , "epic"   .= SV.fromSized _rdEpic
        ]
    toEncoding ResearchData{..} = pairs . mconcat $
        [ "common" .= TCP.toList (\ResearchTier{..} ->
                          object [ "unlock" .= _rtUnlock
                                 , "techs"  .= SV.fromSized _rtTechs
                                 ]
                        ) _rdCommon
        , "epic"   .= SV.fromSized _rdEpic
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

-- | Iso on a value under a list of bonuses (from left to right).
bonusing :: [BonusAmount] -> Iso' Double Double
bonusing = flip foldl' id $ \e -> \case
             BAIncrement i  -> e . adding i
             BAPercent   p  -> e . multiplying (1 + p / 100)
             BAMultiplier r -> e . multiplying r

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
maxLevel :: Research a -> Natural
maxLevel = either fromIntegral (fromIntegral . V.length) . _rCosts

-- | Empty status (no research).
emptyResearchStatus :: ResearchData tiers epic -> ResearchStatus tiers epic
emptyResearchStatus ResearchData{..} =
    ResearchStatus (map1 clear _rdCommon) (0 <$ _rdEpic)
  where
    clear :: ResearchTier a -> Flip SV.Vector Natural a
    clear = Flip . set mapped 0 . view rtTechs

-- | Zips together all research data and research statuses into an
-- accumulator.
foldResearch
    :: Monoid b
    => (Either (Research Bock) (Research GoldenEgg) -> Natural -> b)
    -> ResearchData tiers epic
    -> ResearchStatus tiers epic
    -> b
foldResearch f ResearchData{..} ResearchStatus{..} = mconcat
    [ foldMap1 (\case d :&: Flip s -> fold $ SV.zipWith (f . Left) (_rtTechs d) s
               )
        (zipP (_rdCommon :&: _rsCommon))
    , fold $ SV.zipWith (f . Right) _rdEpic _rsEpic
    ]

-- | Bonuses from a given 'Research' at a given level.  Assumes no "maximum
-- level".
researchBonuses :: Research a -> Natural -> Bonuses
researchBonuses _ 0 = Bonuses M.empty
researchBonuses r l = _rBaseBonuses r & _Bonuses . mapped . mapped %~ scaleAmount l

-- | Total bonuses from a given 'ResearchStatus'.
totalBonuses :: ResearchData tiers epic -> ResearchStatus tiers epic -> Bonuses
totalBonuses = foldResearch (either researchBonuses researchBonuses)

-- | How many common techs have been researched?
--
-- Used for opening tiers.
researchCount :: ResearchStatus tiers epic -> Natural
researchCount = sumOf $ rsCommon . liftTraversal (_Flip . folded)

legalTiers
    :: forall tiers epic. ()
    => ResearchData tiers epic
    -> ResearchStatus tiers epic
    -> Prod (C Bool) tiers
legalTiers rd rs = rd ^. rdCommon . to (map1 go)
  where
    tot = researchCount rs
    go  :: ResearchTier a
        -> C Bool a
    go = view $ rtUnlock . to (>= tot) . _Unwrapped

data ResearchError = REMaxedOut
                   | RELocked
  deriving (Show, Eq, Ord)

makePrisms ''ResearchError

-- | Purchase research at a given index, incrementing the counter in the
-- 'ResearchStatus'.  Returns 'Nothing' if research is already maxed out.
purchaseResearch
    :: forall tiers epic a. (KnownNat epic, SingI tiers)
    => ResearchData tiers epic
    -> ResearchIx tiers epic a
    -> ResearchStatus tiers epic
    -> Either ResearchError (a, ResearchStatus tiers epic)
purchaseResearch rd i rs0 = pp . getComp . researchIxStatusLegal rd i (Comp . go) $ rs0
  where
    bs = totalBonuses rd rs0
    pp :: Maybe (First a, ResearchStatus tiers epic)
        -> Either ResearchError (a, ResearchStatus tiers epic)
    pp Nothing                     = Left REMaxedOut
    pp (Just (First Nothing , _ )) = Left RELocked
    pp (Just (First (Just c), rs)) = case i of
        RICommon _ -> Right (c ^. bonusingFor bs BTResearchCosts, rs)
        RIEpic   _ -> Right (c, rs)
    go :: Natural -> Maybe (First a, Natural)
    go currLevel = first (First . Just) <$> case rd ^. researchIxData i . rCosts of
        Left m   -> (0, currLevel + 1) <$ guard (currLevel < m)
                        \\ researchIxNum i
        Right cs -> (, currLevel + 1) <$> (cs V.!? fromIntegral (currLevel + 1))

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
          g x@(slot :&: (_ :&: SNat)) = (_2 . _1 . rtTechs . ixSV slot) f x
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

-- | Lens into both the research data and research status common slots
-- together.
rdrsCommon
    :: Lens' ((Uncur ResearchData :&: Uncur ResearchStatus) (tiers # epic))
             (Prod (ResearchTier :&: Flip SV.Vector Natural) tiers)
rdrsCommon f (Uncur rd :&: Uncur rs) =
    f (zipP (_rdCommon rd :&: _rsCommon rs)) <&> \(unzipP->(rdc :&: rsc)) ->
      Uncur (rd & rdCommon .~ rdc) :&: Uncur (rs & rsCommon .~ rsc)

-- | Lens into both the research data and research status epic slots
-- together.
rdrsEpic
    :: KnownNat epic
    => Lens' ((Uncur ResearchData :&: Uncur ResearchStatus) (tiers # epic))
             (SV.Vector epic (Research GoldenEgg, Natural))
rdrsEpic f (Uncur rd :&: Uncur rs) =
    f (liftA2 (,) (_rdEpic rd) (_rsEpic rs)) <&> \rdrs ->
      Uncur (rd & rdEpic .~ fmap fst rdrs) :&: Uncur (rs & rsEpic .~ fmap snd rdrs)

-- | Lens into both a ResearchData and ResearchStatus together, from
-- a given index.
researchIx
    :: (SingI tiers, KnownNat epic)
    => ResearchIx tiers epic a
    -> Lens' ((Uncur ResearchData :&: Uncur ResearchStatus) (tiers # epic))
             (Research a, Natural)
researchIx = \case
    RICommon i -> \f ->
      let g :: forall a. _ a
            -> _ (_ a)
          g (slot :&: ((c :&: Flip e) :&: SNat)) =
              f (SV.index (_rtTechs c) slot, SV.index e slot) <&> \(d, s) ->
                let c' = c & rtTechs . ixSV slot .~ d
                    e' = e & ixSV slot .~ s
                in  slot :&: ((c' :&: Flip e') :&: SNat)
      in  rdrsCommon $ \rdrs ->
            map1 fanFst . fanSnd <$> sumProd g (i :&: zipP (rdrs :&: singProd sing))
    RIEpic i -> rdrsEpic . ixSV i

withSomeResearch
    :: DSum Sing (Uncur res)
    -> (forall tiers epic. (KnownNat epic, SingI tiers) => res tiers epic -> r)
    -> r
withSomeResearch = \case
    STuple2 sTs SNat :=> Uncur r -> \f -> withSingI sTs $ f r

-- | Traversal into a given index of a 'ResearchStatus' if the item is in
-- a legal tier.
--
-- Only a legal traversal if the mapping function doesn't change the legal
-- status.
researchIxStatusLegal
    :: (SingI tiers, KnownNat epic)
    => ResearchData tiers epic
    -> ResearchIx tiers epic a
    -> Traversal' (ResearchStatus tiers epic) Natural
researchIxStatusLegal rd = \case
    RICommon i -> \f rs0 -> getUncur . fanSnd <$>
      let totCount = researchCount rs0
          g :: forall a. _ a -> _ (_ a)
          g x@(slot :&: ((rt :&: _) :&: SNat))
            | _rtUnlock rt <= totCount = (_2 . _1 . _2 . _Flip . ixSV slot) f x
            | otherwise                = pure x
      in  (Uncur rd :&: Uncur rs0) & rdrsCommon %%~ \rtrs ->
            map1 fanFst . fanSnd <$> sumProd g (i :&: zipP (rtrs :&: singProd sing))
    RIEpic i   -> rsEpic . ixSV i

-- | All 'ResearchIx' into common researches.
researchIxesCommon
    :: SingI tiers
    => Prod (Flip SV.Vector (ResearchIx tiers epic Bock)) tiers
researchIxesCommon = go sing
  where
    go :: Sing ts -> Prod (Flip SV.Vector (ResearchIx ts epic Bock)) ts
    go = \case
      SNil         -> Ø
      SNat `SCons` ss ->
        let rest = map1 (over (_Flip . mapped . _RICommon) InR) (go ss)
        in  Flip (SV.generate (RICommon . InL)) :< rest

-- | All 'ResearchIx' into epic researches.
researchIxesEpic
    :: KnownNat epic
    => SV.Vector epic (ResearchIx tiers epic GoldenEgg)
researchIxesEpic = SV.generate RIEpic

-- | All legal 'ResearchIx' for common research.
legalResearchIxesCommon
    :: forall tiers epic. SingI tiers
    => ResearchData tiers epic
    -> ResearchStatus tiers epic
    -> Prod (Flip SV.Vector (Either ResearchError (ResearchIx tiers epic Bock))) tiers
legalResearchIxesCommon rd rs =
    imap1 (\i -> Flip . go i) (zipP (zipP (_rdCommon rd :&: _rsCommon rs) :&: singProd sing))
  where
    totCount = researchCount rs
    go  :: forall t. ()
        => Index tiers t
        -> ((ResearchTier :&: Flip SV.Vector Natural) :&: Sing) t
        -> SV.Vector t (Either ResearchError (ResearchIx tiers epic Bock))
    go i ((rt :&: Flip c) :&: SNat)
      | rt ^. rtUnlock > totCount = pure (Left RELocked)
      | otherwise                 =
          let mkIx
                  :: Finite t
                  -> Research Bock
                  -> Natural
                  -> Either ResearchError (ResearchIx tiers epic Bock)
              mkIx j r n
                | n < maxLevel r = Right . RICommon . someSum $ Some (i :&: j)
                | otherwise      = Left REMaxedOut
          in  SV.izipWith mkIx (rt ^. rtTechs) c

-- | All legal 'ResearchIx' for epic research.
--
-- 'Nothing' implies research is maxed-out.
legalResearchIxesEpic
    :: forall tiers epic. KnownNat epic
    => ResearchData tiers epic
    -> ResearchStatus tiers epic
    -> (SV.Vector epic :.: Maybe) (ResearchIx tiers epic GoldenEgg)
legalResearchIxesEpic rd rs = Comp $ SV.izipWith go (_rdEpic rd) (_rsEpic rs)
  where
    go  :: Finite epic
        -> Research GoldenEgg
        -> Natural
        -> Maybe (ResearchIx tiers epic GoldenEgg)
    go i r n
      | n < maxLevel r = Just . RIEpic $ i
      | otherwise      = Nothing
