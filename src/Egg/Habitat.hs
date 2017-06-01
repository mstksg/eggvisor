{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Egg.Habitat (
    Hab(..)
  , HabData(..)
  , SomeHabData
  ) where


import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Type.Combinator.Util
import           Data.Type.Vector
import           Data.Yaml
import           GHC.Generics              (Generic)
import           Type.Family.Nat
import qualified Data.Text                 as T
import qualified Data.Vector.Sized         as SV

data Hab = Hab { habName         :: T.Text
               , habBaseCapacity :: Integer
               , habCosts        :: Vec N4 Double
               }
  deriving (Show, Eq, Ord, Generic)

newtype HabData habs = HabData { _hdHabs :: SV.Vector habs Hab }

type SomeHabData = DSum Sing HabData

habParseOptions :: Options
habParseOptions = defaultOptions
    { fieldLabelModifier = camelTo2 '-' . drop 3
    }

instance FromJSON Hab where
    parseJSON  = genericParseJSON  habParseOptions
instance ToJSON Hab where
    toJSON     = genericToJSON     habParseOptions
    toEncoding = genericToEncoding habParseOptions

instance FromJSON SomeHabData where
    parseJSON = withObject "HabData" $ \v -> do
      habs <- v .: "habitats"
      SV.withSized habs $ \habsV ->
        return $ SNat :=> HabData habsV
instance ToJSON SomeHabData where
    toJSON = \case
        _ :=> r -> toJSON r
    toEncoding = \case
        _ :=> r -> toEncoding r
instance KnownNat habs => FromJSON (HabData habs) where
    parseJSON = withObject "HabData" $ \v -> do
      habs <- v .: "habitats"
      case SV.toSized habs of
        Nothing    -> fail "Bad number of items in list."
        Just habsV -> return $ HabData habsV
instance ToJSON (HabData habs) where
    toJSON HabData{..} = object
        [ "habitats" .= SV.fromSized _hdHabs
        ]
    toEncoding HabData{..} = pairs . mconcat $
        [ "habitats" .= SV.fromSized _hdHabs
        ]
