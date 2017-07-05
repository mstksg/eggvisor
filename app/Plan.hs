{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import           Data.Foldable
import           Data.Proxy
import           Data.Type.Equality
import           Data.Yaml
import           Egg
import           Egg.Search
import           GHC.TypeLits.Compare
import           Text.Printf
import           Type.Class.Higher

main :: IO ()
main = do
    sgde <- decodeFileEither "data/gamedata.yaml"
    case sgde of
      Left e -> error $ show e
      Right sgd -> withSomeGameData sgd $ \(gd :: GameData eggs '(tiers, epic) habs vehicles) -> do
        Just Refl <- return $ isLE (Proxy @1) (Proxy @eggs)
        Just Refl <- return $ isLE (Proxy @1) (Proxy @habs)
        Just Refl <- return $ isLE (Proxy @1) (Proxy @vehicles)
        putStrLn "Loaded data!  Searching..."
        let path = search gd (initFarmStatus gd) (GCash 1000)
        case path of
          Nothing -> putStrLn "no path found"
          Just ps -> do
            putStrLn "path found"
            -- print $ length ps
            forM_ ps $ \case
              Left w         -> printf "Wait %.1f s\n" w
              Right (Some a) -> putStrLn $ renderAction gd a

