{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Egg.Egg (
    Egg(..)
  , eggName, eggDescription, eggValue, eggDiscover, eggUnlock, eggMultiplier
  , EggData(..), _EggData
  , SomeEggData
  ) where

import           Control.Lens hiding      ((.=))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Egg.Commodity
import           GHC.Generics             (Generic)
import           Text.Printf
import qualified Data.Text                as T
import qualified Data.Vector.Sized        as SV

data Egg = Egg { _eggName        :: T.Text
               , _eggDescription :: T.Text
               , _eggValue       :: Bock
               , _eggDiscover    :: Bock
               , _eggUnlock      :: Bock
               , _eggMultiplier  :: Double
               }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''Egg

data EggData eggs = EggData { _edEggs :: SV.Vector eggs Egg }
  deriving (Show, Eq, Ord, Generic)

makePrisms ''EggData
makeWrapped ''EggData

type SomeEggData = DSum Sing EggData

eggParseOptions :: Options
eggParseOptions = defaultOptions
    { fieldLabelModifier = camelTo2 '-' . drop 4
    }

instance FromJSON Egg where
    parseJSON  = genericParseJSON  eggParseOptions
instance ToJSON Egg where
    toJSON     = genericToJSON     eggParseOptions
    toEncoding = genericToEncoding eggParseOptions

instance FromJSON SomeEggData where
    parseJSON o = do
      es <- parseJSON o
      SV.withSized es $ \esV ->
        return $ SNat :=> EggData esV
instance ToJSON SomeEggData where
    toJSON = \case
        _ :=> r -> toJSON r
    toEncoding = \case
        _ :=> r -> toEncoding r
instance KnownNat eggs => FromJSON (EggData eggs) where
    parseJSON o = do
      es <- parseJSON o
      case SV.toSized es of
        Nothing  -> fail $ printf "Bad number of items in list. (Expected %d, got %d)"
                         (fromSing (SNat @eggs)) (length es)
        Just esV -> return $ EggData esV
instance ToJSON (EggData eggs) where
    toJSON     = toJSON     . SV.fromSized . _edEggs
    toEncoding = toEncoding . SV.fromSized . _edEggs
