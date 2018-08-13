{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}


-- import           Data.Profunctor
-- import           Numeric.Lens
-- import qualified Data.Text               as T
-- import qualified Data.Text.Encoding      as T
-- import qualified GHC.TypeLits            as TL
import           Brick
import           Brick.BChan
import           Brick.Widgets.Center
import           Brick.Widgets.List
import           Brick.Widgets.ProgressBar
import           Control.Applicative
import           Control.Arrow hiding       (app)
import           Control.Auto.Blip
import           Control.Auto.Blip.Internal
import           Control.Auto.Effects
import           Control.Category
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.Maybe
import           Data.Proxy
import           Data.Semigroup
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Text.Lens
import           Data.Type.Combinator
import           Data.Type.Equality
import           Data.Type.Fin
import           Data.Vector.Sized.Util
import           Data.Yaml
import           Egg
import           GHC.TypeLits.Compare
import           Graphics.Vty hiding        ((<|>))
import           Prelude hiding             ((.), id)
import           Text.Printf
import           Type.Class.Higher
import qualified Control.Auto               as A
import qualified Data.Vector                as V

data FarmEvent eggs te habs vehicles =
      FEClock Double
    | FEAction (SomeAction eggs te habs vehicles)
    | FEDrone Drone

makePrisms ''FarmEvent

data PurchEvent =
      PEUpDown Bool
    | PEHomeEnd Bool
    | PESelect

makePrisms ''PurchEvent

data UIEvent =
      UIQuit
    | UIHab
    | UIVehicle
    | UIPurch PurchEvent

makePrisms ''UIEvent

type AppState eggs te habs vehicles
    = A.Auto Maybe
        (Either (FarmEvent eggs te habs vehicles) UIEvent) (Widget (), FarmStatus eggs te habs vehicles)

farmAuto
    :: forall eggs tiers epic habs vehicles. (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles)
    => GameData eggs '(tiers, epic) habs vehicles
    -> AppState eggs '(tiers, epic) habs vehicles
farmAuto gd = proc inp -> do
    (fEs, uEs) <- onEithers -< inp
    -- quit if UIQuit
    execB Nothing . filterB (has _UIQuit) -< uEs
    -- update farm if FarmEvent
    rec fs  <- scanB_ (foldl' processEvent) (initFarmStatus gd) . lagBlips_ -< ((:[]) <$> fEs) <> ((:[]) <$> as)
        pEs <- lagBlips_ . mapMaybeB (preview _UIPurch) -< uEs
        (as, men) <- menu -< (fs, pEs)
    disp <- sequenceA [
        arr (header . fst)
      , habs
      , vehs
      ] -< (fs, uEs)
    id -< (vBox $ disp ++ [men], fs)
      -- ++ [txt . T.unlines . take 20 . T.lines . T.decodeUtf8 $ encode fs]
  where
    processEvent fs0 = \case
      FEClock dt        -> stepFarmDT gd NotCalm dt fs0
      FEAction (Some a) -> case runAction gd a fs0 of
        Left _    -> fs0
        Right fs1 -> fs1
      FEDrone d         -> popDrone gd d fs0
    header fs = vBox
      [ str "Egg simulator"
      , hBox [ str "Egg: "
             , txt $ gd ^. edEggs . ixSV (fs ^. fsEgg) . eggName
             ]
      , hBox [ str "Cash: "
             , str . show $ fs ^. fsBocks
             ]
      , hBox [ str "Income: "
             , str . show $ farmIncome gd fs
             , str " per sec"
             ]
      , if fs ^. fsSoulEggs > 0
          then hBox [ str "Soul egg bonus: x"
                    , str . show $ 1 ^. bonusingSoulEggs gd fs
                    , str " ("
                    , str . show $ fs ^. fsSoulEggs
                    , str " )"
                    ]
          else emptyWidget
      , case fs ^. fsVideoBonus of
          Nothing -> emptyWidget
          Just v  -> hBox [ str "Video bonus: x2, "
                          , str $ printf "%.1f" (v / 60)
                          , str " mins remaining"
                          ]
      , hBox [ str "Farm value: "
             , str . show $ farmValue gd fs
             ]
      , progShow "Hatchery" (fs ^. fsHatchery) (hatcheryCapacity gd fs)
      ]
    habs = proc (fs, uEs) -> do
      hEs <- mapMaybeB (preview _UIHab) -< uEs
      expanded <- scanB (\e -> const (not e)) False -< hEs
      let bs = farmBonuses gd fs
          habCaps = habCapacities (gd ^. gdHabData) bs (fs ^. fsHabs)
          avails  = fs ^. fsHabs . availableSpaces (gd ^. gdHabData) bs
          breakdown = fold $
            (\h c a -> return $ case h of
                Nothing -> hCenter . str $ "(No hab in slot)"
                Just h' -> progShow (gd ^. hdHabs . ixSV h' . habName . unpacked)
                                (c - fromMaybe 0 a) c
            ) <$> (fs ^. fsHabs . hsSlots) <*> habCaps <*> avails
      id -< vBox $
        progShow "All Habs" (sum habCaps - sum (Comp avails)) (sum habCaps)
        : if expanded then breakdown else []
    vehs = proc (fs, uEs) -> do
      let (r, c) = farmLayingRateAvailability gd fs
      id -< progShow "Depot" r (r + fromMaybe 0 c)
    menu = proc (fs, pEs) -> do
        -- lst <- scanB_ processList (list () mempty 1) -< (pEs `description` _)
        let upgrs = [ (sa, c)
                    | sa@(Some a) <- actions gd fs
                    , Just c <- pure $ case actionCost gd fs a of
                                  ACForbidden   -> Nothing
                                  ACNoCost      -> Just Nothing
                                  ACBock b      -> Just (Just (Left b))
                                  ACGoldenEgg e -> Just (Just (Right e))
                    , case c of Just (Left b)  -> fs ^. fsBocks >= b
                                Just (Right e) -> fs ^. fsGoldenEggs >= e
                                Nothing        -> True
                    ]
            -- flip mapMaybe (actions gd fs) $ \a -> do
        lst <- A.accum_ processList (list () mempty 1) . substituteB
            -< (Left upgrs, Right <$> pEs)
        purch <- mapMaybeB (preview _PESelect) -< pEs
        let pB = case listSelectedElement lst of
                   Nothing -> NoBlip
                   Just e  -> FEAction (lst ^?! listElementsL . ix (fst e) . _1) <$ purch
        id -< (pB, renderList (renderAction' fs) True lst)
      where
        processList l = \case
          Left  acts -> l & listElementsL . iso V.toList V.fromList .~ acts
                          & listSelectedL .~ ((l ^. listSelectedL) <|> Just 0)
          Right pe   -> case pe of
            PEUpDown  True  -> listMoveUp l
            PEUpDown  False -> listMoveDown l
            PEHomeEnd True  -> listMoveTo 0 l
            PEHomeEnd False -> listMoveTo (lengthOf (listElementsL . folded) l) l
            _               -> l
        renderAction'
            :: FarmStatus eggs '(tiers, epic) habs vehicles
            -> Bool
            -> (SomeAction eggs '(tiers, epic) habs vehicles, Maybe (Either Bock GoldenEgg))
            -> Widget ()
        renderAction' fs b (Some a, cost) = str $ printf "%s%s (%s)" s d c
          where
            s, d, c :: String
            s | b         = "* "
              | otherwise = ""
            d = case a of
              AResearch i   -> printf "Research %s (%d/%d)"
                                 (gd ^. gdResearchData . researchIxData i . rName . unpacked)
                                 (fs ^. fsResearch . researchIxStatus i)
                                 (gd ^. gdResearchData . researchIxData i . to maxLevel)
              AHab l h      -> printf "Hab slot %d to %s" (fin l + 1)
                                 (gd ^. hdHabs . ixSV h . habName . unpacked)
              AVehicle l v  -> printf "Vehicle slot %d to %s" (l + 1)
                                 (gd ^. vdVehicles . ixSV v . vName . unpacked)
              AHatch n      -> printf "Hatch %d chickens" n
              AWatchVideo   -> "Watch video"
              AEggUpgrade e -> printf "Upgrade egg: %s" (gd ^. edEggs . ixSV e . eggName)
              APrestige     -> "Prestige"
            c = show cost
    progShow :: String -> Double -> Double -> Widget n
    progShow s x y = progressBar (Just $ printf "%s (%d / %d)" s (round @_ @Int x) (round @_ @Int y))
                                 (realToFrac (x / y))

toFarmEvent
    :: BrickEvent () Double
    -> Maybe (Either (FarmEvent eggs '(tiers, epic) habs vehicles) UIEvent)
toFarmEvent = \case
    VtyEvent e -> case e of
      EvKey (KChar 'q') _ -> Just $ Right UIQuit
      EvKey (KChar ' ') _ -> Just $ Left (FEAction (Some (AHatch 1)))
      EvKey (KChar 'h') _ -> Just $ Right UIHab
      EvKey (KChar 'v') _ -> Just $ Right UIVehicle
      EvKey (KChar 'd') _ -> Just $ Left (FEDrone EliteDrone)
      EvKey KUp         _ -> Just $ Right (UIPurch (PEUpDown True))
      EvKey KDown       _ -> Just $ Right (UIPurch (PEUpDown False))
      EvKey KHome       _ -> Just $ Right (UIPurch (PEHomeEnd True))
      EvKey KEnd        _ -> Just $ Right (UIPurch (PEHomeEnd False))
      EvKey KEnter      _ -> Just $ Right (UIPurch PESelect)
      _                   -> Nothing
    AppEvent dt -> Just $ Left (FEClock dt)
    _ -> Nothing


-- type AppState eggs '(tiers, epic) habs vehicles

-- renderApp
--     :: (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles, 1 TL.<= eggs, 1 TL.<= habs, 1 TL.<= vehicles)
--     => GameData eggs '(tiers, epic) habs vehicles
--     -> FarmStatus eggs '(tiers, epic) habs vehicles
--     -> Widget n
-- renderApp gd fs = vBox
--     [ str "Egg Simulator"
--     , txt $ gd ^. gdEggData . _EggData . ixSV (fs ^. fsEgg) . eggName
--     , str . show $ fs ^. fsBocks
--     , habs
--     ]
--   where
--     bs = farmBonuses gd fs
--     habs = vBox $
--         progShow "All Habs" (sum habCaps - sum (Comp avails)) (sum habCaps)
--         : fold ((\h c a -> return $ case h of
--                     Nothing -> hCenter . str $ "(No hab in slot)"
--                     Just h' -> progShow (gd ^. gdHabData . _HabData . ixSV h' . habName . unpacked)
--                                     (c - fromMaybe 0 a) c
--                 )
--                  <$> (fs ^. fsHabs . hsSlots) <*> habCaps <*> avails
--                )
--       where
--         progShow :: String -> Double -> Double -> Widget n
--         progShow s x y = progressBar (Just $ printf "%s (%d / %d)" s (round @_ @Int x) (round @_ @Int y))
--                                      (realToFrac (x / y))
--         habCaps = habCapacities (gd ^. gdHabData) bs (fs ^. fsHabs)
--         avails  = fs ^. fsHabs . availableSpaces (gd ^. gdHabData) bs

app
    :: ()
    => GameData eggs '(tiers, epic) habs vehicles
    -> App (AppState eggs '(tiers, epic) habs vehicles, Widget ()) Double ()
app gd = App draw cursor handle return am
  where
    draw = (:[]) . snd
    -- draw fs = [renderApp gd fs]
    --     -- [ str "Egg Simulator"
    --     -- , txt $ gd ^. gdEggData . _EggData . ixSV (fs ^. fsEgg) . eggName
    --     -- , str . show $ fs ^. fsBocks
    --     -- , str $ printf "%d / %d" (round @_ @Int cs) (round @_ @Int hcap)
    --     -- , progressBar (Just "Habs") (realToFrac $ cs / hcap)
    --     -- ]
    --   -- where
    --     -- cs = totalChickens (gd ^. gdHabData) (fs ^. fsHabs)
    --     -- hcap = totalHabCapacity (gd ^. gdHabData) (farmBonuses gd fs) (fs ^. fsHabs)
    cursor _ _ = Nothing
    handle (a0, o0) e = case toFarmEvent e of
      Nothing -> continue (a0, o0)
      Just fe -> case A.stepAuto a0 fe of
        Nothing       -> halt (a0, o0)
        Just ((o1, fs), a1) -> do
          liftIO $ encodeFile "data/sim-state.yaml" fs
          continue (a1, o1)
    am _ = attrMap defAttr $
        [(progressIncompleteAttr, white `on` black)
        ,(progressCompleteAttr  , black `on` white)
        ]

main :: IO ()
main = do
    sgde <- decodeFileEither "data/gamedata.yaml"
    case sgde of
      Left e -> error $ show e
      Right sgd -> withSomeGameData sgd $ \(gd :: GameData eggs '(tiers, epic) habs vehicles) -> do
        Just Refl <- return $ isLE (Proxy @1) (Proxy @eggs)
        Just Refl <- return $ isLE (Proxy @1) (Proxy @habs)
        Just Refl <- return $ isLE (Proxy @1) (Proxy @vehicles)
        clock <- newBChan 2
        void . forkIO . forever $ do
          threadDelay 100000
          writeBChan clock 0.1
        void $ customMain (mkVty defaultConfig) (Just clock) (app gd)
          (farmAuto gd, str "Loading..")

nextRight :: a -> Either e a -> EventM n (Next a)
nextRight d (Left _)  = continue d
nextRight _ (Right x) = continue x
