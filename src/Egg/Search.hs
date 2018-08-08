{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
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
  , WaitAnd(..)
  , waitAmount
  -- , condenseWaits
  ) where

-- import           Data.Finite
-- import           Data.Reflection
-- import           Data.Semigroup
-- import           Data.Type.Combinator
-- import           Debug.Trace
-- import qualified Data.PQueue.Prio.Min    as PQ
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Writer
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.Singletons
import           Data.Singletons.Decide
import           Data.Type.Combinator
import           Data.Type.Sum              as TCS
import           Debug.Trace
import           Egg.Action
import           Egg.Commodity
import           Egg.Farm
import           Egg.GameData
import           Egg.Habitat
import           GHC.TypeLits
import           Numeric.Natural
import           Type.Class.Higher
import           Type.Family.List
import qualified Data.OrdPSQ                as Q
import qualified Data.Vector.Sized          as SV

data Goal = GPop       Natural
          | GFullHabs
          | GCash      Bock
          | GEggs      Natural
          -- | GResearch  (Sum Finite tiers) Natural
          -- | GFarmValue Bock
          -- | GEgg       (Finite eggs)

data GoalDist = GDAchieved
              | GDWait { _gdWait :: Double }
              | GDNever
  deriving (Show, Eq, Ord)

makeLenses ''GoalDist

-- data GoalHeur = GH { _gdDist   :: GoalDist
--                    , _gdIncome :: Down Bock
--                    , _gdPop    :: Down Double
--                    , _gdIHatch :: Down Double
--                    , _gdValue  :: Down Bock
--                    , _gdDepot  :: Down Double
--                    }
--   deriving (Show, Eq, Ord)

-- makeLenses ''GoalHeur

-- | Time to reach the goal, assuming no action is taken.
--
-- Used as heuristic for A star search.
goalDist
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, KnownNat habs)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Goal
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
    -- GEggs e -> case

-- addGD :: Double -> GoalDist -> GoalDist
-- addGD x = \case
--     GDAchieved -> GDWait x
--     GDWait y   -> GDWait (x + y)
--     GDNever    -> GDNever

type Time = Double

data WaitAnd a = WAWait Time
               | WADo   a
               | WAAnd  Time a
  deriving (Show, Functor)

-- data Path a = PNil
--             | PCons [a] Time (Path a)

type SearchQueue eggs te habs vehicles =
      Q.OrdPSQ (FarmStatus eggs te habs vehicles)
               Time
               [Either Time (SomeAction eggs te habs vehicles)]
               -- (Path (SomeAction eggs te habs vehicles))

-- data SearchNode eggs te habs vehicles =
--     SN { _snActions :: [Either Double (SomeAction eggs te habs vehicles)]
--        , _snCost    :: !Double
--        , _snFarm    :: FarmStatus eggs te habs vehicles
--        }

-- tracePath
--     :: (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
--     => GameData eggs '(tiers, epic) habs vehicles
--     -> [Either Time (SomeAction eggs '(tiers, epic) habs vehicles)]
--     -> [Either Time (SomeAction eggs '(tiers, epic) habs vehicles)]
-- tracePath gd xs = traceShow xs' xs
--   where
--     xs' = map (fmap (\case Some a -> renderAction gd a)) xs

search
    :: forall eggs tiers epic habs vehicles.
     ( KnownNat eggs
     , SingI tiers
     , KnownNat epic
     , KnownNat habs
     , KnownNat vehicles
     )
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Goal
    -> Maybe [WaitAnd (SomeAction eggs '(tiers, epic) habs vehicles)]
search gd fs0 g = condenseWaits . reverse <$> go (Q.singleton fs0 0 [])
  where
    mkNode
        :: FarmStatus eggs '(tiers, epic) habs vehicles
        -> Double
        -> [Either Time (SomeAction eggs '(tiers, epic) habs vehicles)]
        -> SomeAction eggs '(tiers, epic) habs vehicles
        -> Maybe (FarmStatus eggs '(tiers, epic) habs vehicles, Double, [Either Time (SomeAction eggs '(tiers, epic) habs vehicles)])
    mkNode !fs1 !c !as sa@(Some !a) = case a of
      APrestige     -> Nothing
      AEggUpgrade _ -> Nothing
      AHatch _      -> do
        let avail = sumOf (availableSpaces gd (farmBonuses gd fs1) . folded . folded) fs1
        guard $ avail > 0
        (t, fs2) <- case waitTilHatcheryFull gd Calm fs1 of
          WaitTilSuccess t (I fs2) -> Just (Just t , fs2)
          NoWait                   -> Just (Nothing, fs1)
          NonStarter               -> Nothing
        let numHatch = floor $ fs2 ^. fsHatchery
            waitEntry = case t of
              Nothing -> []
              Just t' -> [Left t']
            as' = Right (Some (AHatch numHatch))
                : waitEntry ++ as
        case hatchChickens gd numHatch fs2 of
          Left _    -> Nothing
          Right fs3 -> pure (fs3, c + fromMaybe 0 t, as')
      _             -> case runAction gd a fs1 of
              Left  e   -> case bockErrorIx a of
                Proved    i -> case i `TCS.index` e of
                  Just (I (PEInsufficientFunds b)) -> case waitUntilBocks gd Calm (b*1.01) fs1 of
                    -- WaitTilSuccess t (I fs2) ->
                    --   return (fs2, c + t, Left t : as)
                    WaitTilSuccess t (I fs2) -> case runAction gd a fs2 of
                      Left _    -> Nothing
                      Right fs3 -> return (fs3, c + t, Right sa : Left t : as)
                    NoWait                   -> error "hmm.."
                    NonStarter               -> Nothing
                  Nothing -> Nothing
                Disproved _ -> Nothing
              Right fs2 -> return (fs2, c, Right sa : as)
    go  :: SearchQueue eggs '(tiers, epic) habs vehicles
        -> Maybe [Either Time (SomeAction eggs '(tiers, epic) habs vehicles)]
    go !q0 = do
      (fs1, c, as, q1) <- Q.minView q0
      -- tracePath gd as `seq` pure ()
      let q2 = foldl' (\q' (fs', c', as') -> insertIfBetter fs' c' as' q') q1
             . mapMaybe (mkNode fs1 c as)
             . actions gd
             $ fs1
      -- traceShow (Q.size q1) $ pure ()
      -- traceShow c           $ pure ()
      -- traceShow (fmap (\case Some a -> renderAction gd a) <$> as) $ pure ()
      case goalDist gd fs1 g of
        GDAchieved        -> pure as
        GDWait t
          | t <= 0        -> pure as
          | otherwise     ->
            let fs2 = stepFarm gd Calm t fs1
                q3  = insertIfBetter fs2 (c + t) (Left t : as) q2
            in  go $ capQueue 25 q3
        _                 -> go $ capQueue 25 q2

-- greedySearch
--     :: 

    -- condensor
    --     :: Either Double (SomeAction eggs '(tiers, epic) habs vehicles)
    --     -> Either Double (SomeAction eggs '(tiers, epic) habs vehicles)
    --     -> Maybe (Either Double (SomeAction eggs '(tiers, epic) habs vehicles))
    -- condensor (Left x) (Left y) = Just (Left (x + y))
    -- condensor (Right (Some (AHatch x))) (Right (Some (AHatch y))) = Just (Right (Some (AHatch (x + y))))
    -- condensor _        _        = Nothing
    -- addCondensed x []       = [x]
    -- addCondensed x (y : ys) = case condensor x y of
    --                             Nothing -> x : y : ys
    --                             Just z  -> z : ys

insertIfBetter
    :: (Ord k, Ord p)
    => k
    -> p
    -> v
    -> Q.OrdPSQ k p v
    -> Q.OrdPSQ k p v
insertIfBetter k p v q = case Q.insertView k p v q of
    (Nothing     , q') -> q'
    (Just (p', _), q')
      | p < p'    -> q'
      | otherwise -> q

condenseWaits :: [Either Time a] -> [WaitAnd a]
condenseWaits [] = []
condenseWaits [Left  x] = [WAWait x]
condenseWaits [Right x] = [WADo x]
condenseWaits (Left x:Left  y:ys) = condenseWaits (Left (x + y) : ys)
condenseWaits (Left x:Right y:ys) = WAAnd x y : condenseWaits ys
condenseWaits (Right x:ys) = WADo x : condenseWaits ys

waitAmount
    :: WaitAnd a
    -> Time
waitAmount = \case
    WAWait w   -> w
    WADo     _ -> 0
    WAAnd  w _ -> w

capQueue
    :: (Ord k, Ord p)
    => Int
    -> Q.OrdPSQ k p v
    -> Q.OrdPSQ k p v
capQueue n = Q.fromList
           . take n
           . unfoldr (fmap (\(k,p,v,q) -> ((k,p,v),q)) . Q.minView)

