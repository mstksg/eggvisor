{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Egg.Habitat (
    Hab(..), habName, habBaseCapacity, habCosts
  , HabData(..), _HabData
  , SomeHabData
  , HabStatus(..), _HabStatus
  , initHabStatus
  , baseCapacity
  , slotValue
  , habHistory
  , habPrice
  , upgradeHab
  ) where


import           Control.Lens hiding        ((.=))
import           Control.Monad
import           Control.Monad.Trans.Writer
import           Data.Aeson.Types
import           Data.Dependent.Sum
import           Data.Finite
import           Data.Maybe
import           Data.Monoid
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.Tuple
import           Data.Type.Combinator.Util
import           Data.Type.Fin
import           Data.Type.Vector           as TCV
import           Data.Vector.Sized.Util
import           Data.Yaml
import           GHC.Generics               (Generic)
import           Numeric.Natural
import           Type.Family.Nat
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Vector.Sized          as SV

data Hab = Hab { _habName         :: T.Text
               , _habBaseCapacity :: Integer
               , _habCosts        :: Vec N4 Double
               }
  deriving (Show, Eq, Ord, Generic)

makeLenses ''Hab

newtype HabData habs = HabData { _hdHabs :: SV.Vector habs Hab }

makePrisms ''HabData
makeWrapped ''HabData

type SomeHabData = DSum Sing HabData

newtype HabStatus habs = HabStatus { _hsSlots :: Vec N4 (S.Set (Finite habs)) }

makePrisms ''HabStatus
makeWrapped ''HabStatus

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

initHabStatus :: KnownNat habs => HabStatus habs
initHabStatus = HabStatus (S.singleton 0 :+ pure S.empty)

baseCapacity
    :: forall habs. KnownNat habs
    => HabData habs
    -> HabStatus habs
    -> Integer
baseCapacity HabData{..} = sumOf $ _HabStatus
                                 . folded
                                 . folding S.lookupMax
                                 . to (SV.index _hdHabs)
                                 . habBaseCapacity

slotValue :: HabStatus habs -> Fin N4 -> Maybe (Finite habs)
slotValue hs i = S.lookupMax . TCV.index' i . _hsSlots $ hs

habHistory :: HabStatus habs -> M.Map (Finite habs) (Finite 4)
habHistory = M.fromListWith (+) . toListOf (_HabStatus . folded . folded . to (, 1))

habPrice :: KnownNat habs => HabData habs -> HabStatus habs -> Finite habs -> Double
habPrice hd hs hab = priceOf
                   . maybe FZ (fromJust . natFin . someNat . fromIntegral)
                   . M.lookup hab
                   . habHistory
                   $ hs
  where
    priceOf :: Fin N4 -> Double
    priceOf i = view (_HabData . ixSV hab . habCosts . ixV i) hd

upgradeHab
    :: KnownNat habs
    => HabData habs
    -> Fin N4
    -> Finite habs
    -> HabStatus habs
    -> Maybe (Double, HabStatus habs)
upgradeHab hd slot hab hs0 =
    fmap swap . runWriterT . flip (_HabStatus . ixV slot) hs0 $ \s0 -> WriterT $
      let valid = case S.lookupMax s0 of
                    Nothing -> True
                    Just h  -> h < hab
          price = habPrice hd hs0 hab
          s1    | valid     = S.insert hab s0
                | otherwise = s0
      in  (s1, price) <$ guard valid
