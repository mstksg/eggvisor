{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}


import           Brick
import           Brick.BChan
import           Brick.Widgets.Center
import           Brick.Widgets.ProgressBar
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Data.Maybe
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Text.Lens
import           Data.Type.Combinator
import           Data.Type.Equality
import           Data.Vector.Sized.Util
import           Data.Yaml
import           Egg
import           GHC.TypeLits.Compare
import           Graphics.Vty
import           Text.Printf
import qualified GHC.TypeLits              as TL

renderApp
    :: (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles, 1 TL.<= eggs, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData eggs '(tiers, epic) habs vehicles
    -> FarmStatus eggs '(tiers, epic) habs vehicles
    -> Widget n
renderApp gd fs = vBox
    [ str "Egg Simulator"
    , txt $ gd ^. gdEggData . _EggData . ixSV (fs ^. fsEgg) . eggName
    , str . show $ fs ^. fsBocks
    , habs
    ]
  where
    bs = farmBonuses gd fs
    habs = vBox $
        progShow "All Habs" (sum habCaps - sum (Comp avails)) (sum habCaps)
        : fold ((\h c a -> return $ case h of
                    Nothing -> hCenter . str $ "(No hab in slot)"
                    Just h' -> progShow (gd ^. gdHabData . _HabData . ixSV h' . habName . unpacked)
                                    (c - fromMaybe 0 a) c
                )
                 <$> (fs ^. fsHabs . hsSlots) <*> habCaps <*> avails
               )
      where
        progShow :: String -> Double -> Double -> Widget n
        progShow s x y = progressBar (Just $ printf "%s (%d / %d)" s (round @_ @Int x) (round @_ @Int y))
                                     (realToFrac (x / y))
        habCaps = habCapacities (gd ^. gdHabData) bs (fs ^. fsHabs)
        avails  = fs ^. fsHabs . availableSpaces (gd ^. gdHabData) bs

app
    :: (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles, 1 TL.<= eggs, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData eggs '(tiers, epic) habs vehicles
    -> App (FarmStatus eggs '(tiers, epic) habs vehicles) Double ()
app gd = App draw cursor handle return am
  where
    draw fs = [renderApp gd fs]
        -- [ str "Egg Simulator"
        -- , txt $ gd ^. gdEggData . _EggData . ixSV (fs ^. fsEgg) . eggName
        -- , str . show $ fs ^. fsBocks
        -- , str $ printf "%d / %d" (round @_ @Int cs) (round @_ @Int hcap)
        -- , progressBar (Just "Habs") (realToFrac $ cs / hcap)
        -- ]
      -- where
        -- cs = totalChickens (gd ^. gdHabData) (fs ^. fsHabs)
        -- hcap = totalHabCapacity (gd ^. gdHabData) (farmBonuses gd fs) (fs ^. fsHabs)

    cursor _ _ = Nothing
    handle fs0 = \case
      VtyEvent e  -> case e of
        EvKey (KChar 'q') _ -> halt fs0
        EvKey (KChar ' ') _ -> nextRight fs0 $ runAction gd (AHatch 1) fs0
        _              -> continue fs0
      AppEvent dt -> continue $ stepFarmDT gd NotCalm dt fs0
      _           -> continue fs0
    am _ = attrMap defAttr $
        [(progressIncompleteAttr, white `on` black)
        ,(progressCompleteAttr  , black `on` white)
        ]

main :: IO ()
main = do
    Just sgd <- decodeFile "data/gamedata.yaml"
    withSomeGameData sgd $ \(gd :: GameData eggs '(tiers, epic) habs vehicles) -> do
      Just Refl <- return $ isLE (Proxy @1) (Proxy @eggs)
      Just Refl <- return $ isLE (Proxy @1) (Proxy @habs)
      Just Refl <- return $ isLE (Proxy @1) (Proxy @vehicles)
      clock <- newBChan 2
      void . forkIO . forever $ do
        threadDelay 100000
        writeBChan clock 0.1
      void $ customMain (mkVty defaultConfig) (Just clock) (app gd) (initFarmStatus gd)

nextRight :: a -> Either e a -> EventM n (Next a)
nextRight d (Left _)  = continue d
nextRight _ (Right x) = continue x
