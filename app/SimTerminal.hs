{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}


import           Brick
import           Brick.BChan
import           Control.Concurrent
import           Control.Lens
import           Control.Monad
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Type.Equality
import           Data.Vector.Sized.Util
import           Data.Yaml
import           Egg.Action
import           Egg.Egg
import           Egg.Farm
import           Egg.GameData
import           Egg.Habitat
import           GHC.TypeLits.Compare
import           Graphics.Vty
import qualified GHC.TypeLits             as TL


app
    :: (KnownNat eggs, SingI tiers, KnownNat epic, KnownNat habs, KnownNat vehicles, 1 TL.<= eggs, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData eggs '(tiers, epic) habs vehicles
    -> App (FarmStatus eggs '(tiers, epic) habs vehicles) Double ()
app gd = App draw cursor handle return am
  where
    draw fs = (:[]) . vBox $
      [ str "Egg Simulator"
      , str $ gd ^. gdEggData . _EggData . ixSV (fs ^. fsEgg) . eggName
      , str . show $ fs ^. fsBocks
      , str . show $ totalChickens (gd ^. gdHabData) (fs ^. fsHabs)
      , str . show $ totalHabCapacity (gd ^. gdHabData) (farmBonuses gd fs) (fs ^. fsHabs)

      ]
    cursor _ _ = Nothing
    handle fs0 = \case
      VtyEvent e  -> case e of
        EvKey (KChar 'q') _   -> halt fs0
        EvKey (KChar ' ') _ -> nextRight fs0 $ runAction gd (AHatch 1) fs0
        _              -> continue fs0
      AppEvent dt -> continue $ stepFarmDT gd NotCalm dt fs0
      _           -> continue fs0
    am _ = attrMap defAttr []

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
