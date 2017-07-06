{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

import           Data.Foldable
import           Data.Maybe
import           Data.Proxy
import           Data.Semigroup hiding (option)
import           Data.Type.Equality
import           Data.Yaml hiding      (Parser)
import           Egg
import           Egg.Search
import           GHC.TypeLits.Compare
import           Options.Applicative
import           Text.Printf
import           Type.Class.Higher

data Opts = O { oGoal :: Goal }

parseOpts :: Parser Opts
parseOpts = O <$> parseGoal
  where
    parseGoal :: Parser Goal
    parseGoal = parsePop <|> parseFull <|> parseCash
    parsePop = GPop <$> option auto ( long "pop"
                                   <> short 'p'
                                   <> metavar "CHICKENS"
                                   <> help "Target a farm population"
                                    )
    parseFull = flag' GFullHabs ( long "full"
                               <> short 'f'
                               <> help "Target full habs"
                                )
    parseCash = GCash . realToFrac @Double <$> option auto
                  ( long "cash"
                 <> short 'c'
                 <> metavar "BOCKS"
                 <> help "Target cash in the bank"
                  )

main :: IO ()
main = do
    O{..} <- execParser $ info (parseOpts <**> helper)
               ( fullDesc
              <> progDesc "Optimize a plan for achieving a goal"
              <> header "eggvisor-plan - Plan your goals!"
               )
    sgde <- decodeFileEither "data/gamedata.yaml"
    case sgde of
      Left e -> error $ show e
      Right sgd -> withSomeGameData sgd $ \(gd :: GameData eggs '(tiers, epic) habs vehicles) -> do
        Just Refl <- return $ isLE (Proxy @1) (Proxy @eggs)
        Just Refl <- return $ isLE (Proxy @1) (Proxy @habs)
        Just Refl <- return $ isLE (Proxy @1) (Proxy @vehicles)
        putStrLn "Loaded data!  Searching..."
        -- let path = search gd (initFarmStatus gd) (GCash 100000000)
        -- let path = search gd (initFarmStatus gd) (GPop 50000)
        let path = search gd (initFarmStatus gd) oGoal
        case condenseWaits <$> path of
          Nothing -> putStrLn "no path found"
          Just ps -> do
            putStrLn "path found"
            forM_ (zip [1 :: Int ..] ps) $ \case
              (i, WAWait w         ) -> printf "%d)\tWait %.1f s\n" i w
              (i, WADo     (Some a)) -> printf "%d)\t%s\n" i (renderAction gd a)
              (i, WAAnd  w (Some a)) -> printf "%d)\tWait %.1f s => %s\n" i w (renderAction gd a)
