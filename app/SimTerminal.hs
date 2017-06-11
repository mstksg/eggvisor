{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}


import           Brick.Main
import           Control.Monad
import           Data.Proxy
import           Data.Type.Equality
import           Data.Yaml
import           Data.Singletons.TypeLits
import           Egg.Farm
import           Egg.GameData
import           GHC.TypeLits.Compare
import qualified GHC.TypeLits         as TL


app
    :: (KnownNat eggs, KnownNat habs, KnownNat vehicles, 1 TL.<= eggs, 1 TL.<= habs, 1 TL.<= vehicles)
    => GameData eggs '(tiers, epic) habs vehicles
    -> App (FarmStatus eggs '(tiers, epic) habs vehicles) () ()
app = undefined

main :: IO ()
main = do
    Just sgd <- decodeFile "data/gamedata.yaml"
    withSomeGameData sgd $ \(gd :: GameData eggs '(tiers, epic) habs vehicles) -> do
      Just Refl <- return $ isLE (Proxy @1) (Proxy @eggs)
      Just Refl <- return $ isLE (Proxy @1) (Proxy @habs)
      Just Refl <- return $ isLE (Proxy @1) (Proxy @vehicles)
      void $ defaultMain (app gd) (initFarmStatus gd)
