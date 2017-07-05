{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Egg.Search (
    Goal(..)
  , search
  ) where

import           Control.Lens
import           Data.Finite
import           Data.Maybe
import           Data.Ord
import           Data.Reflection
import           Data.Semigroup
import           Data.Singletons
import           Data.Type.Combinator
import           Data.Type.Sum
import           Debug.Trace
import           Egg.Action
import           Egg.Commodity
import           Egg.Farm
import           Egg.GameData
import           Egg.Habitat
import           GHC.TypeLits
import           Numeric.Natural
import           Type.Class.Higher
import qualified Data.PQueue.Min      as PQ

data Goal eggs tiers = GPop       Natural
                     | GFullHabs
                     | GCash      Bock
                     -- | GResearch  (Sum Finite tiers) Natural
                     -- | GFarmValue Bock
                     -- | GEgg       (Finite eggs)

data GoalDist = GDAchieved
              | GDWait Double
              | GDNever
  deriving (Show, Eq, Ord)

data GoalHeur = GH { _gdDist   :: GoalDist
                   , _gdIncome :: Bock
                   , _gdPop    :: Double
                   , _gdDepot  :: Double
                   , _gdValue  :: Bock
                   }
  deriving (Show, Eq, Ord)

makeLenses ''GoalHeur

-- | Time to reach the goal, assuming no action is taken.
--
-- Used as heuristic for A star search
goalDist
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Goal eggs tiers
    -> GoalDist
goalDist gd fs = \case
    GPop p -> case waitTilPop gd (farmBonuses gd fs) Calm p fs of
      WaitTilSuccess _ Nothing  -> GDNever
      WaitTilSuccess t (Just _) -> GDWait t
      NoWait     -> GDAchieved
      NonStarter -> GDNever
    GFullHabs -> case fillTimes gd (farmBonuses gd fs) Calm fs of
      Left  _  -> GDNever
      Right ts -> let t = maximum ts
                  in  if t == 0
                        then GDAchieved
                        else GDWait t
    GCash b -> case waitUntilBocks gd Calm b fs of
      WaitTilSuccess t _ -> GDWait t
      NoWait             -> GDAchieved
      NonStarter         -> GDNever


data SearchNode eggs te habs vehicles =
    SN { _snActions :: [Either Double (SomeAction eggs te habs vehicles)]
       , _snFarm    :: FarmStatus eggs te habs vehicles
       }

search
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles, 1 <= habs, 1 <= vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Goal eggs tiers
    -> Maybe [Either Double (SomeAction eggs '(tiers, epic) habs vehicles)]
search gd fs0 g = reverse <$> go (PQ.singleton (Arg (heurOf fs0) (SN [] fs0)))
  where
    go  :: PQ.MinQueue (Arg GoalHeur (SearchNode eggs '(tiers, epic) habs vehicles))
        -> Maybe [Either Double (SomeAction eggs '(tiers, epic) habs vehicles)]
    go q0 = case PQ.minView q0 of
      Nothing      -> Nothing
      Just (Arg h (SN as fs1), q1) -> case h ^. gdDist of
        GDAchieved        -> Just as
        GDWait t | t == 0 -> Just as
        _ -> let branches = fst <$> actions gd fs1
                 newNodes = PQ.fromList
                          . mapMaybe (\case
                                Some a -> case runAction gd a fs1 of
                                  Left _    -> Nothing
                                  Right fs2 -> Just $ Arg (heurOf fs2) (SN (addCondensed (Right (Some a)) as) fs2)
                              )
                          $ branches
                 waiting = Arg (heurOf fs2) (SN (addCondensed (Left 1) as) fs2)
                   where fs2 = stepFarm gd Calm 1 fs1
                 q1 = waiting `PQ.insert` q0 `PQ.union` newNodes
             in  go q1
    condensor
        :: Either Double (SomeAction eggs '(tiers, epic) habs vehicles)
        -> Either Double (SomeAction eggs '(tiers, epic) habs vehicles)
        -> Maybe (Either Double (SomeAction eggs '(tiers, epic) habs vehicles))
    condensor (Left x) (Left y) = Just (Left (x + y))
    condensor (Right (Some (AHatch x))) (Right (Some (AHatch y))) = Just (Right (Some (AHatch (x + y))))
    condensor _        _        = Nothing
    addCondensed x []       = [x]
    addCondensed x (y : ys) = case condensor x y of
                                Nothing -> x : y : ys
                                Just z  -> z : ys
    heurOf :: FarmStatus eggs '(tiers, epic) habs vehicles -> GoalHeur
    heurOf fs = GH (goalDist gd fs g)
                   (farmIncome gd fs)
                   (totalChickens fs)
                   (farmDepotCapacity gd fs)
                   (farmValue gd fs)


-- data GoalDist = GDAchieved
--               | GDWait Double
--               | GDNever

-- actions
--     :: forall eggs tiers epic habs vehicles.
--        (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
--     => GameData   eggs '(tiers, epic) habs vehicles
--     -> FarmStatus eggs '(tiers, epic) habs vehicles
--     -> [(Some (Action eggs '(tiers, epic) habs vehicles), Maybe (Either Bock GoldenEgg))]
