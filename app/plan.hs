{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- import           Data.Maybe
-- import           Data.Semigroup hiding  (option)
import           Control.Monad.Trans.State
import           Data.Foldable
import           Data.Proxy
import           Data.Type.Equality
import           Data.Yaml hiding          (Parser)
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
        case path of
          Nothing -> putStrLn "no path found"
          Just ps -> do
            putStrLn "path found"
            flip evalStateT (initFarmStatus gd) . forM_ (zip [1 :: Int ..] ps) $ \(i, wa) -> StateT $ \fs0 ->
              case wa of
                WAWait w          -> do
                  printf "%d)\tWait %.1f s\n" i w
                  return ((), stepFarm gd Calm w fs0)
                WADo     (Some a) -> do
                  printf "%d)\t%s\n" i (renderActionFS gd fs0 a)
                  return $ case runAction gd a fs0 of
                    Left _ -> error "invalid action????"
                    Right fs1 -> ((), fs1)
                WAAnd  w (Some a) -> do
                  let fs1 = stepFarm gd Calm w fs0
                  printf "%d)\tWait %.1f s => %s\n" i w (renderActionFS gd fs1 a)
                  return $ case runAction gd a fs1 of
                    Left _ -> error "invalid action????"
                    Right fs2 -> ((), fs2)
