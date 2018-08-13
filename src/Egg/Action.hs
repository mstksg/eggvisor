{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Egg.Action (
    Action(..)
  , SomeAction
  , PurchaseError(..)
  , _PEInsufficientFunds
  , runAction
  , actionCost, ActionCost(..)
  , commonResearchActions
  , epicResearchActions
  , habActions
  , vehicleActions
  , eggActions
  , actions
  , renderAction
  , renderActionFS
  , bockErrorIx
  , actionErrors, ActionErrors
  , runActions
  ) where

-- import           Control.Applicative
-- import           Data.Type.Conjunction
-- import           Debug.Trace
-- import qualified Data.Map               as M
-- import qualified GHC.TypeLits           as TL
import           Control.Lens hiding       ((:<), Index)
import           Data.Bifunctor
import           Data.Finite
import           Data.Kind
import           Data.List
import           Data.Maybe
import           Data.Singletons.Decide    (Decision(..))
import           Data.Singletons.TH hiding ((%~))
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Type.Combinator.Util
import           Data.Type.Fin
import           Data.Type.Index
import           Data.Type.Product
import           Data.Type.Sum
import           Data.Type.Vector          as TCV
import           Data.Vector.Sized.Util
import           Egg.Commodity
import           Egg.Egg
import           Egg.Farm
import           Egg.GameData
import           Egg.Habitat
import           Egg.Research
import           Egg.Vehicle
import           Numeric.Lens
import           Numeric.Natural
import           Text.Printf
import           Type.Class.Higher
import           Type.Family.Nat
import qualified Data.Vector               as V
import qualified Data.Vector.Sized         as SV

newtype PurchaseError a = PEInsufficientFunds { _peCost :: a }
  deriving Show

makePrisms ''PurchaseError
makeWrapped ''PurchaseError

data Action :: Nat -> ([Nat], Nat) -> Nat -> Nat -> [Type] -> Type where
    AResearch
        :: ResearchIx tiers epic a
        -> Action eggs '(tiers, epic) habs vehicles
             '[PurchaseError a, ResearchError]
    AHab
        :: Fin N4
        -> Finite habs
        -> Action eggs '(tiers, epic) habs vehicles
             '[PurchaseError Bock, Finite habs]
    AVehicle
        :: Integer
        -> Finite vehicles
        -> Action eggs '(tiers, epic) habs vehicles
             '[PurchaseError Bock, SomeVehicleUpgradeError vehicles]
    AHatch
        :: Natural
        -> Action eggs '(tiers, epic) habs vehicles
             '[HatchError]
    AWatchVideo
        :: Action eggs '(tiers, epic) habs vehicles
             '[Double]
    AEggUpgrade
        :: Finite eggs
        -> Action eggs '(tiers, epic) habs vehicles
             '[UpgradeError]
    APrestige
        :: Action eggs '(tiers, epic) habs vehicles
             '[()]

type SomeAction eggs te habs vehicles = Some (Action eggs te habs vehicles)

-- deriving instance Show (Sum Finite tiers) => Show (Action eggs '(tiers, epic) habs vehicles errs)
deriving instance Show (Action eggs '(tiers, epic) habs vehicles errs)

bockErrorIx
    :: Action eggs '(tiers, epic) habs vehicles errs
    -> Decision (Index errs (PurchaseError Bock))
bockErrorIx = \case
    AResearch (RICommon _) -> Proved IZ
    AResearch (RIEpic   _) -> Disproved $ \case
      IS (IS i) -> case i of {}
    AHab _ _               -> Proved IZ
    AVehicle _ _           -> Proved IZ
    AHatch _               -> Disproved $ \case
      IS i -> case i of {}
    AWatchVideo            -> Disproved $ \case
      IS i -> case i of {}
    AEggUpgrade _          -> Disproved $ \case
      IS i -> case i of {}
    APrestige              -> Disproved $ \case
      IS i -> case i of {}

type ActionErrors habs vehicles
        = '[ PurchaseError Bock
           , PurchaseError GoldenEgg
           , ResearchError
           , Finite habs
           , SomeVehicleUpgradeError vehicles
           , HatchError
           , Double
           , UpgradeError
           , ()
           ]

actionErrors
    :: Action  eggs '(tiers, epic) habs vehicles errs
    -> Sum I errs
    -> Sum I (ActionErrors habs vehicles)
actionErrors = \case
    AResearch i -> \case
      InL e       -> case i of
        RICommon _ -> InL e
        RIEpic   _ -> InR (InL e)
      InR (InL e) -> InR (InR (InL e))
    AHab _ _ -> \case
      InL e       -> InL e
      InR (InL e) -> InR (InR (InR (InL e)))
    AVehicle _ _ -> \case
      InL e       -> InL e
      InR (InL e) -> InR (InR (InR (InR (InL e))))
    AHatch _ -> \case
      InL e       -> InR (InR (InR (InR (InR (InL e)))))
    AWatchVideo -> \case
      InL e       -> InR (InR (InR (InR (InR (InR (InL e))))))
    AEggUpgrade _ -> \case
      InL e       -> InR (InR (InR (InR (InR (InR (InR (InL e)))))))
    APrestige -> \case
      InL e       -> InR (InR (InR (InR (InR (InR (InR (InR (InL e))))))))

-- | Execute an action as a change in a 'FarmStatus'.
runAction
    :: (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> Action     eggs '(tiers, epic) habs vehicles errs
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Either (Sum I errs) (FarmStatus eggs '(tiers, epic) habs vehicles)
runAction gd = \case
    AResearch i -> \fs0 -> do
      (cost, fs1) <- first (inj . I) . getComp $
        fsResearch (Comp . purchaseResearch (gd ^. gdResearchData) i) fs0
      case i of
        RICommon _ ->
          fs1 & fsBocks %%~ \b ->
            if b >= cost
              then return (b - cost)
              else Left (inj . I $ PEInsufficientFunds cost)
        RIEpic _ ->
          fs1 & fsGoldenEggs %%~ \g ->
            if g >= cost
              then return (g - cost)
              else Left (inj . I $ PEInsufficientFunds cost)
    AHab s h    -> \fs0 -> do
      (cost, fs1) <- first (inj . I) $
        upgradeHab gd (farmBonuses gd fs0) s h fs0
      fs1 & fsBocks %%~ \b ->
        if b >= cost
          then return (b - cost)
          else Left (inj . I $ PEInsufficientFunds cost)
    AVehicle s v -> \fs0 -> do
      (cost, fs1) <- first (inj . I) . getComp $
        fsDepot (Comp . upgradeSomeVehicle (gd ^. gdVehicleData) (farmBonuses gd fs0) s v)
          fs0
      fs1 & fsBocks %%~ \b ->
        if b >= cost
          then return (b - cost)
          else Left (inj . I $ PEInsufficientFunds cost)
    AHatch c      -> first (inj . I) . hatchChickens gd c
    AWatchVideo   -> first (inj . I) . watchVideo gd
    AEggUpgrade e -> first (inj . I) . upgradeEgg gd e
    APrestige     -> maybe (Left (inj (I ()))) Right . prestigeFarm gd

runSomeAction
    :: (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> SomeAction eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Either (Sum I (ActionErrors habs vehicles)) (FarmStatus eggs '(tiers, epic) habs vehicles)
runSomeAction gd = \case
    Some a -> first (actionErrors a) . runAction gd a

-- | All possible common research actions and their costs.
commonResearchActions
    :: forall eggs tiers epic habs vehicles. SingI tiers
    => Prod (Flip SV.Vector
                (Action eggs '(tiers, epic) habs vehicles '[PurchaseError Bock, ResearchError])
            ) tiers
commonResearchActions = map1 (Flip . fmap AResearch . getFlip) researchIxesCommon

-- | All possible epic research actions and their costs.
epicResearchActions
    :: forall eggs tiers epic habs vehicles. (KnownNat epic)
    => SV.Vector epic (Action eggs '(tiers, epic) habs vehicles '[PurchaseError GoldenEgg, ResearchError])
epicResearchActions = fmap AResearch researchIxesEpic

-- | All possible hab upgrade actions and their costs.
habActions
    :: KnownNat habs
    => VecT N4 (SV.Vector habs) (Action eggs '(tiers, epic) habs vehicles '[PurchaseError Bock, Finite habs])
habActions = vgen_ $ SV.generate . AHab

-- | All possible vehicle upgrade actions and their costs.
--
-- Doesn't include potential extra slots
vehicleActions
    :: GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> (V.Vector :.: SV.Vector vehicles) (Action eggs '(tiers, epic) habs vehicles '[PurchaseError Bock, SomeVehicleUpgradeError vehicles])
vehicleActions gd fs =
    someVehicleUpgrades (gd ^. gdVehicleData) (farmBonuses gd fs) (fs ^. fsDepot)
      & _Comp . _Comp . imapped <.> imapped %@~ (\(i,j) _ -> I (AVehicle (fromIntegral i) j))
      & _Comp %~ (fmap . fmap) getI . getComp

-- | All possible Egg upgrade actions
eggActions
    :: KnownNat eggs
    => SV.Vector eggs (Action eggs '(tiers, epic) habs vehicles '[UpgradeError])
eggActions = SV.generate AEggUpgrade

-- | All possible actions, with their costs
actions
    :: forall eggs tiers epic habs vehicles.
       (SingI tiers, KnownNat habs)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> [SomeAction eggs '(tiers, epic) habs vehicles]
actions gd fs = mconcat
    [ commonResearchActions ^.. liftTraversal (_Flip . folded) . to Some
    -- , epicResearchActions   ^.. folded                         . to Some
    , habActions            ^.. folded                         . to Some
    -- , vehicleActions gd fs  ^.. folded                         . to Some
    -- , eggActions            ^.. folded                         . to Some
    ] ++
    [ Some (AHatch 1)
    -- , Some AWatchVideo
    -- , Some APrestige
    ]


data ActionCost = ACForbidden
                | ACNoCost
                | ACBock Bock
                | ACGoldenEgg GoldenEgg

actionCost
    :: forall eggs tiers epic habs vehicles err.
       (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Action     eggs '(tiers, epic) habs vehicles err
    -> ActionCost
actionCost gd fs = \case
    AResearch i -> case purchaseResearch (gd ^. gdResearchData) i (fs ^. fsResearch) of
      Left  _      -> ACForbidden
      Right (c, _) -> case i of
        RICommon _ -> ACBock c
        RIEpic   _ -> ACGoldenEgg c
    AHab s h -> case upgradeHab gd bs s h fs of
      Left  _      -> ACForbidden
      Right (c, _) -> ACBock c
    AVehicle s v -> case upgradeSomeVehicle (gd ^. gdVehicleData) bs s v (fs ^. fsDepot) of
      Left  _      -> ACForbidden
      Right (c, _) -> ACBock c
    AHatch c -> case hatchChickens gd c fs of
      Left  _ -> ACForbidden
      Right _ -> ACNoCost
    AWatchVideo -> case watchVideo gd fs of
      Left  _ -> ACForbidden
      Right _ -> ACNoCost
    AEggUpgrade e -> case upgradeEgg gd e fs of
      Left  _ -> ACForbidden
      Right _ -> ACNoCost
    APrestige -> case prestigeFarm gd fs of
      Nothing -> ACForbidden
      Just _  -> ACNoCost
  where
    bs = farmBonuses gd fs


renderAction
    :: (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> Action eggs '(tiers, epic) habs vehicles errs
    -> String
renderAction gd = \case
    AResearch i   -> printf "Research %s" (gd ^. gdResearchData . researchIxData i . rName)
    AHab s h      -> printf "Upgrade Hab (slot %d) to %s"
                       (fin s + 1)
                       (gd ^. hdHabs . ixSV h . habName)
    AVehicle s v  -> printf "Upgrade Vehicle (slot %d) to %s"
                       (s + 1)
                       (gd ^. vdVehicles . ixSV v . vName)
    AHatch n      -> printf "Hatch %d chickens" n
    AWatchVideo   -> "Watch bonus video"
    AEggUpgrade e -> printf "Upgrade egg to %s"
                       (gd ^. edEggs . ixSV e . eggName)
    APrestige     -> "Prestige"

renderActionFS
    :: (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Action eggs '(tiers, epic) habs vehicles errs
    -> String
renderActionFS gd fs = \case
    AResearch i   -> printf "Research %s (%d/%d)"
                       (gd ^. gdResearchData . researchIxData i . rName)
                       (fs ^. fsResearch . researchIxStatus i . adding 1)
                       (gd ^. gdResearchData . researchIxData i . to maxLevel)
    AHab s h      -> printf "Upgrade Hab (slot %d) to %s (from %s)"
                       (fin s + 1)
                       (gd ^. hdHabs . ixSV h . habName)
                       (case fs ^. hsSlots . ixV s of
                          Nothing -> "empty"
                          Just h' -> gd ^. hdHabs . ixSV h' . habName
                       )
    AVehicle s v  ->
      let oldVeh = withSomeDepotStatus (farmBonuses gd fs) (fs ^. fsDepot) $ \ds -> do
            s' <- packFinite s
            v' <- ds ^? _DepotStatus . ix s'
            return $ gd ^. vdVehicles . ixSV v' . vName
      in  printf "Upgrade Vehicle (slot %d) to %s (from %s)"
            (s + 1)
            (gd ^. vdVehicles . ixSV v . vName)
            (fromMaybe "empty" oldVeh)
    AHatch n      -> printf "Hatch %d chickens" n
    AWatchVideo   -> "Watch bonus video"
    AEggUpgrade e -> printf "Upgrade egg to %s (from %s)"
                       (gd ^. edEggs . ixSV e . eggName)
                       (gd ^. edEggs . ixSV (fs ^. fsEgg) . eggName)
    APrestige     -> "Prestige"

runActions
    :: (Foldable t, KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
    => GameData eggs '(tiers, epic) habs vehicles
    -> t (Either Double (Some (Action eggs '(tiers, epic) habs vehicles)))
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Either (Sum I (ActionErrors habs vehicles)) (FarmStatus eggs '(tiers, epic) habs vehicles)
runActions gd as = flip (foldl' (\fs a -> go a =<< fs)) as . Right
  where
    go (Left t)  = Right . stepFarm gd Calm t
    go (Right a) = runSomeAction gd a
