{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

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
  -- , purchaseCommon
  ) where

import           Control.Applicative hiding      (some)
import           Control.Lens hiding             ((.=))
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Aeson.Encoding
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Foldable
import           Data.Function
import           Data.Kind
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
import           Data.Yaml
import           Egg.Types
import           GHC.Generics                    (Generic)
import           GHC.TypeLits
import           Numeric.Natural
import           Type.Class.Higher
import           Type.Class.Witness
import qualified Data.HashMap.Lazy               as HM
import qualified Data.Map                        as M
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
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
    RICommon :: Prod Finite tiers -> ResearchIx tiers epic Double
    RIEpic   :: Finite epic       -> ResearchIx tiers epic Integer

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
        s :=> Uncur r -> toJSON r
    toEncoding = \case
        s :=> Uncur r -> toEncoding r
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

purchaseCommon
    :: forall tiers epic. Every KnownNat tiers
    => ResearchData tiers epic
    -> Sum Finite tiers
    -> ResearchStatus tiers epic
    -> (Maybe Double, ResearchStatus tiers epic)
purchaseCommon ResearchData{..} ix rs0 =
    some (sumSome pickedTier) $ \(tier :&: ((Flip d :&: Flip s) :&: slot)) ->
      case every @_ @KnownNat tier of
        Wit -> flip (_rsCommon . ixProd tier . _Flip . ixSV slot) rs0 $ \currLevel ->
          case rCosts (SV.index d slot) of
            Left m   -> if currLevel < m
              then (Just 0 , currLevel + 1)
              else (Nothing, currLevel)
            Right cs -> case cs V.!? fromIntegral (currLevel + 1) of
              Just c  -> (Just c , currLevel + 1)
              Nothing -> (Nothing, currLevel)
  where
    pickedTier
        :: Sum ((Flip SV.Vector (Research Double) :&: Flip SV.Vector Natural) :&: Finite) tiers
    pickedTier = tagSum (zipP (rdCommon :&: rsCommon rs0)) ix

purchaseEpic
    :: forall tiers epic. KnownNat epic
    => ResearchData tiers epic
    -> Finite epic
    -> ResearchStatus tiers epic
    -> (Maybe Integer, ResearchStatus tiers epic)
purchaseEpic ResearchData{..} ix rs0 = flip (_rsEpic . ixSV ix) rs0 $ \currLevel ->
    case rCosts (SV.index rdEpic ix) of
      Left m   -> if currLevel < m
        then (Just 0 , currLevel + 1)
        else (Nothing, currLevel)
      Right cs -> case cs V.!? fromIntegral (currLevel + 1) of
        Just c  -> (Just c , currLevel + 1)
        Nothing -> (Nothing, currLevel)

ixSV :: KnownNat n => Finite n -> Lens' (SV.Vector n a) a
ixSV ix f v = (\x -> v SV.// [(fromIntegral ix, x)]) <$> f (SV.index v ix)

-- purchaseCommon ResearchData{..} t r = runStateT $ do
--     tier <- maybe (lift (Left REBadTier)) return $ rdCommon V.!? t
--     res  <- maybe (lift (Left REBadSlot)) return $ tier     V.!? r
--     currCommon <- gets rsCommon
--     currTier <- maybe (lift (Left REBadTier)) return $ currCommon V.!? t
--     currRes  <- maybe (lift (Left REBadSlot)) return $ currTier   V.!? r
--     let newLevel = currRes + 1
--         updated  = currCommon V.// [(t, currTier V.// [(r, fromIntegral newLevel)])]
--     case rCosts res of
--       Left m | newLevel > m -> lift $ Left REMaxedOut
--              | otherwise    -> do
--                  modify $ \rs -> rs { rsCommon = updated }
--                  return Nothing
--       Right v -> case v V.!? fromIntegral newLevel of
--         Nothing -> lift $ Left REMaxedOut
--         Just p  -> do
--           modify $ \rs -> rs { rsCommon = updated }
--           return (Just p)


-- -- data ResearchError = REBadTier
-- --                    | REBadSlot
-- --                    | REMaxedOut
-- --   deriving (Show, Eq, Ord)

-- -- purchaseCommon
-- --     :: ResearchData
-- --     -> Int
-- --     -> Int
-- --     -> ResearchStatus
-- --     -> Either ResearchError (Maybe Double, ResearchStatus)
-- -- purchaseCommon ResearchData{..} t r = runStateT $ do
-- --     tier <- maybe (lift (Left REBadTier)) return $ rdCommon V.!? t
-- --     res  <- maybe (lift (Left REBadSlot)) return $ tier     V.!? r
-- --     currCommon <- gets rsCommon
-- --     currTier <- maybe (lift (Left REBadTier)) return $ currCommon V.!? t
-- --     currRes  <- maybe (lift (Left REBadSlot)) return $ currTier   V.!? r
-- --     let newLevel = currRes + 1
-- --         updated  = currCommon V.// [(t, currTier V.// [(r, fromIntegral newLevel)])]
-- --     case rCosts res of
-- --       Left m | newLevel > m -> lift $ Left REMaxedOut
-- --              | otherwise    -> do
-- --                  modify $ \rs -> rs { rsCommon = updated }
-- --                  return Nothing
-- --       Right v -> case v V.!? fromIntegral newLevel of
-- --         Nothing -> lift $ Left REMaxedOut
-- --         Just p  -> do
-- --           modify $ \rs -> rs { rsCommon = updated }
-- --           return (Just p)

_rsCommon :: Lens (ResearchStatus t1 e)
                  (ResearchStatus t2 e)
                  (Prod (Flip SV.Vector Natural) t1)
                  (Prod (Flip SV.Vector Natural) t2)
_rsCommon f rs = (\v -> rs { rsCommon = v }) <$> f (rsCommon rs)

        -- :: { rdCommon :: Prod (Flip SV.Vector (Research Double)) tiers

_rsEpic :: Lens (ResearchStatus t e1) (ResearchStatus t e2) (SV.Vector e1 Natural) (SV.Vector e2 Natural)
_rsEpic f rs = (\v -> rs { rsEpic = v }) <$> f (rsEpic rs)
