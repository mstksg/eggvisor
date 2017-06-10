{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Egg.Action (
    Action(..)
  , PurchaseError(..)
  , _PEInsufficientFunds
  , runAction
  ) where

import           Control.Lens hiding       ((:<), Index)
import           Data.Bifunctor
import           Data.Finite
import           Data.Kind
import           Data.Singletons.TH hiding ((%~))
import           Data.Singletons.TypeLits
import           Data.Type.Combinator
import           Data.Type.Combinator.Util
import           Data.Type.Conjunction
import           Data.Type.Fin
import           Data.Type.Index
import           Data.Type.Product
import           Data.Type.Sum
import           Data.Type.Vector          as TCV
import           Egg.Commodity
import           Egg.Farm
import           Egg.GameData
import           Egg.Habitat
import           Egg.Research
import           Egg.Vehicle
import           Type.Class.Higher
import           Type.Family.Nat
import qualified Data.Vector.Sized         as SV
import qualified GHC.TypeLits              as TL

newtype PurchaseError a = PEInsufficientFunds { _peCost :: a }

-- data PurchaseError = PEInsufficientBocks      { _peBocks      :: Bock      }
--                    | PEInsufficientGoldenEggs { _peGoldenEggs :: GoldenEgg }

makePrisms ''PurchaseError
makeWrapped ''PurchaseError

-- type ActionErrs habs vs = '[ '[PurchaseError, ResearchError]
--                            , '[PurchaseError, Finite habs  ]
--                            , '[PurchaseError, SomeVehicleUpgradeError vs]
--                            , '[]
--                            , '[UpgradeError]
--                            , '[]
--                            ]

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
    AWatchVideo
        :: Action eggs '(tiers, epic) habs vehicles
             '[]
    AEggUpgrade
        :: Finite eggs
        -> Action eggs '(tiers, epic) habs vehicles
             '[UpgradeError]
    APrestige
        :: Action eggs '(tiers, epic) habs vehicles
             '[]

runAction
    :: (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles, 1 TL.<= habs, 1 TL.<= vehicles)
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
      (cost, fs1) <- first (inj . I) . getComp $
        fsHabs (Comp . upgradeHab (gd ^. gdHabData) (farmBonuses gd fs0) s h)
               fs0
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
    AWatchVideo   -> Right . watchVideo gd
    AEggUpgrade e -> first (inj . I) . upgradeEgg gd e
    APrestige     -> Right . prestigeFarm gd

commonResearchActions
    :: forall eggs tiers epic habs vehicles. SingI tiers
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Prod (Flip SV.Vector (Either (Sum I '[PurchaseError Bock, ResearchError]) (Action eggs '(tiers, epic) habs vehicles '[PurchaseError Bock, ResearchError], Bock))) tiers
commonResearchActions gd fs =
    imap1 (\t -> iover (_Flip . imapped) (go t)) (legalResearchesCommon (gd ^. gdResearchData) (fs ^. fsResearch))
  where
    go  :: Index tiers a
        -> Finite a
        -> Either ResearchError Bock
        -> Either (Sum I '[PurchaseError Bock, ResearchError]) (Action eggs '(tiers, epic) habs vehicles '[PurchaseError Bock, ResearchError], Bock)
    go t s e = do
      cost <- first (inj . I) e
      if fs ^. fsBocks >= cost
        then return (AResearch . RICommon . someSum $ Some (t :&: s), cost)
        else Left . inj . I $ PEInsufficientFunds cost

epicResearchActions
    :: forall eggs tiers epic habs vehicles. (KnownNat epic)
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> (SV.Vector epic :.: Either (Sum I '[PurchaseError GoldenEgg, ResearchError])) (Action eggs '(tiers, epic) habs vehicles '[PurchaseError GoldenEgg, ResearchError], GoldenEgg)
epicResearchActions gd fs =
    legalResearchesEpic (gd ^. gdResearchData) (fs ^. fsResearch)
      & (_Comp . imapped) %@~ \i c -> do
        cost <- maybe (Left . inj . I $ REMaxedOut) Right c
        if fs ^. fsGoldenEggs >= cost
          then return (AResearch (RIEpic i), cost)
          else Left . inj . I $ PEInsufficientFunds cost

habActions
    :: KnownNat habs
    => GameData   eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> VecT N4 (SV.Vector habs :.: Either (Sum I '[PurchaseError Bock, Finite habs])) (Action eggs '(tiers, epic) habs vehicles '[PurchaseError Bock, Finite habs], Bock)
habActions gd fs =
    habUpgrades (gd ^. gdHabData) (farmBonuses gd fs) (fs ^. fsHabs)
      & (isets TCV.imap <. _Comp) <.> imapped %@~ \(s, h) c -> do
        cost <- first (inj . I) c
        if fs ^. fsBocks >= cost
          then return (AHab s h, cost)
          else Left . inj . I $ PEInsufficientFunds cost

-- vehicleActions
--     :: KnownNat vehicles
--     => GameData   eggs '(tiers, epic) habs vehicles
--     -> FarmStatus eggs '(tiers, epic) habs vehicles
--     -> VecT N4 (SV.Vector vehicles :.: Either (Sum I '[PurchaseError Bock, SomeVehicleUpgradeError vehicles])) (Action eggs '(tiers, epic) habs vehicles '[PurchaseError Bock, SomeVehicleUpgradeError vehicles], Bock)
-- vehicleActions gd fs = _

    -- => VehicleData vs
    -- -> Bonuses
    -- -> SomeDepotStatus vs
    -- -> (M.Map Integer :.: SV.Vector vs :.: Either (Finite vs)) Bock
    -- -- habUpgrades (gd ^. gdHabData) (farmBonuses gd fs) (fs ^. fsHabs)
    -- --   & (isets TCV.imap <. _Comp) <.> imapped %@~ \(s, h) c -> do
    -- --     cost <- first (inj . I) c
    -- --     if fs ^. fsBocks >= cost
    -- --       then return (AHab s h, cost)
    -- --       else Left . inj . I $ PEInsufficientFunds cost


